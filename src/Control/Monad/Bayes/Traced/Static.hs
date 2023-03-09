{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Static
-- Description : Distributions on execution traces of full programs
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Static
  ( Traced (..),
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
import Control.Monad.Bayes.Traced.Class (HasTraced (..))
import Control.Monad.Bayes.Traced.Common (Trace (..), TraceT, hoist, mhTransFree, mhTransWithBool, output, runTraceT, scored, singleton, traceT)
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (Pair))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)

-- | A tracing monad where only a subset of random choices are traced.
--
-- The random choices that are not to be traced should be lifted from the
-- transformed monad.
newtype Traced m a = Traced {getTraced :: Product (Weighted (Density m)) (TraceT m) a}
  deriving newtype (Functor, Applicative, Monad)

model :: Traced m a -> Weighted (Density m) a
model (Traced (Pair model' _)) = model'

traceDist :: Traced m a -> TraceT m a
traceDist (Traced (Pair _ traceDist')) = traceDist'

instance MonadTrans Traced where
  lift m = Traced $ Pair (lift $ lift m) (lift m)

instance MonadDistribution m => MonadDistribution (Traced m) where
  random = Traced $ Pair random (singleton =<< lift random)

instance MonadFactor m => MonadFactor (Traced m) where
  score w = Traced $ Pair (score w) (lift (score w) >> scored w)

instance MonadMeasure m => MonadMeasure (Traced m)

instance HasTraced Traced where
  hoistTrace f (Traced (Pair m d)) = Traced (Pair m (hoist f d))

  marginal = fmap output . runTraceT . traceDist

  mhStep (Traced (Pair m d)) = Traced $ Pair m $ traceT $ do
    aTrace <- runTraceT d
    runTraceT $ mhTransFree m $ traceT $ Identity aTrace

  mh n (Traced (Pair m d)) = fmap (map output . NE.toList) (f n)
    where
      f k
        | k <= 0 = fmap (:| []) $ runTraceT d
        | otherwise = do
            (x :| xs) <- f (k - 1)
            y <- runTraceT $ mhTransFree m x
            return (y :| x : xs)

  runMHResult traced = runTraceT (traceDist traced) >>= mhTransWithBool (model traced)

  getTrace = runTraceT . traceDist

  getModel = model
