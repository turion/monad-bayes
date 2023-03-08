{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
    hoistTrace,
    marginal,
    mhStep,
    mh,
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Bayes.Class
  ( MonadDistribution (random),
    MonadFactor (..),
    MonadMeasure,
  )
import Control.Monad.Bayes.Density.Free (Density)
import Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    TraceT (..),
    mhTrans',
    scored,
    singleton, TraceT, output, hoist, runTraceT, traceT,
  )
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans.Class
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import Data.Functor.Product
import Iso.Deriving (As1 (..), Isomorphic)
import Control.Monad.Morph (hoist)

-- | Tracing monad that records random choices made in the program.
data Traced m a = Traced
  { -- | Run the program with a modified trace.
    model :: Weighted (Density Identity) a,
    -- | Record trace and output.
    traceDist :: TraceT m a
  }
  deriving stock Functor
  deriving (Applicative, Monad) via (Product (Weighted (Density Identity)) (TraceT m) `As1` Traced m)

instance Isomorphic (Product (Weighted (Density Identity)) (TraceT m) a) (Traced m a) where

instance MonadDistribution m => MonadDistribution (Traced m) where
  random = Traced random (lift random >>= singleton)

instance MonadFactor m => MonadFactor (Traced m) where
  score w = Traced (score w) (lift (score w) >> scored w)

instance MonadMeasure m => MonadMeasure (Traced m)

hoistTrace :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistTrace f (Traced m d) = Traced m (hoist f d)

-- | Discard the trace and supporting infrastructure.
marginal :: Monad m => Traced m a -> m a
marginal (Traced _ d) = output d

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: MonadDistribution m => Traced m a -> Traced m a
mhStep (Traced m d) = Traced m $ traceT $ do
  aTrace <- runTraceT d
  runTraceT $ mhTrans' m $ traceT $ Identity aTrace

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps.
mh :: MonadDistribution m => Int -> Traced m a -> m [a]
mh n (Traced m d) = fmap (map (runIdentity . output) . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) $ (traceT . Identity) <$> runTraceT d
      | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- runTraceT $ mhTrans' m x
        return ((traceT $ Identity y) :| x : xs)
