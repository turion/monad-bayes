{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Basic
-- Description : Distributions on full execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Basic
  ( Traced,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
  ( MonadDistribution (random),
    MonadFactor (..),
    MonadMeasure,
  )
import Control.Monad.Bayes.Density.Free (Density)
import Control.Monad.Bayes.Traced.Class
import Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    TraceT (..),
    mhTrans',
    scored,
    singleton, TraceT, output, hoist, runTraceT, traceT, mhTransWithBool,
  )
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans.Class
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import qualified Control.Monad.Bayes.Weighted as Weighted
import qualified Control.Monad.Bayes.Density.Free as Density

-- | Tracing monad that records random choices made in the program.
data Traced m a = Traced
  { -- | Run the program with a modified trace.
    model :: Weighted (Density Identity) a,
    -- | Record trace and output.
    traceDist :: TraceT m a
  }
  deriving stock Functor

instance Monad m => Applicative (Traced m) where
  pure x = Traced (pure x) (pure x)
  (Traced mf df) <*> (Traced mx dx) = Traced (mf <*> mx) (df <*> dx)

instance Monad m => Monad (Traced m) where
  (Traced mx dx) >>= f = Traced my dy
    where
      my = mx >>= model . f
      dy = dx >>= traceDist . f

instance MonadDistribution m => MonadDistribution (Traced m) where
  random = Traced random (lift random >>= singleton)

instance MonadFactor m => MonadFactor (Traced m) where
  score w = Traced (score w) (lift (score w) >> scored w)

instance MonadMeasure m => MonadMeasure (Traced m)

instance HasTraced Traced where
  hoistTrace f (Traced m d) = Traced m (hoist f d)

  marginal (Traced _ d) = output <$> runTraceT d

  mhStep (Traced m d) = Traced m $ traceT $ do
    aTrace <- runTraceT d
    runTraceT $ mhTrans' m $ traceT $ Identity aTrace

  mh n (Traced m d) = fmap (map output . NE.toList) (f n)
    where
      f k
        | k <= 0 = fmap (:| []) $ (traceT . Identity) <$> runTraceT d
        | otherwise = do
          (x :| xs) <- f (k - 1)
          y <- runTraceT $ mhTrans' m x
          return ((traceT $ Identity y) :| x : xs)

  runMHResult Traced { model, traceDist } = runTraceT traceDist >>= mhTransWithBool (Weighted.hoist (Density.hoist (pure . runIdentity)) model)

  getTrace Traced { traceDist } = runTraceT traceDist

  getModel = Weighted.hoist (Density.hoist (pure . runIdentity)) . model
