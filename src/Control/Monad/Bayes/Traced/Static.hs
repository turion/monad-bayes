{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    mhTransFree,
    scored,
    singleton, TraceT, output, traceT,
  )
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import Data.Functor.Product (Product (Pair))
import Control.Monad.Bayes.Traced.Common (hoist, runTraceT)
import Data.Functor.Identity (Identity(..))

-- | A tracing monad where only a subset of random choices are traced.
--
-- The random choices that are not to be traced should be lifted from the
-- transformed monad.
newtype Traced m a = Traced { getTraced :: Product (Weighted (Density m)) (TraceT m) a}
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

hoistTrace :: (forall x. m x -> m x) -> Traced m a -> Traced m a
hoistTrace f (Traced (Pair m d)) = Traced (Pair m (hoist f d))

-- | Discard the trace and supporting infrastructure.
marginal :: Monad m => Traced m a -> m a
marginal = fmap output . runTraceT . traceDist

-- | A single step of the Trace Metropolis-Hastings algorithm.
mhStep :: MonadDistribution m => Traced m a -> Traced m a
mhStep (Traced (Pair m d)) = Traced $ Pair m $ traceT $ do
  aTrace <- runTraceT d
  runTraceT $ mhTransFree m $ traceT $ Identity aTrace

-- $setup
-- >>> import Control.Monad.Bayes.Class
-- >>> import Control.Monad.Bayes.Sampler.Strict
-- >>> import Control.Monad.Bayes.Weighted

-- | Full run of the Trace Metropolis-Hastings algorithm with a specified
-- number of steps. Newest samples are at the head of the list.
--
-- For example:
--
-- * I have forgotten what day it is.
-- * There are ten buses per hour in the week and three buses per hour at the weekend.
-- * I observe four buses in a given hour.
-- * What is the probability that it is the weekend?
--
-- >>> :{
--  let
--    bus = do x <- bernoulli (2/7)
--             let rate = if x then 3 else 10
--             factor $ poissonPdf rate 4
--             return x
--    mhRunBusSingleObs = do
--      let nSamples = 2
--      sampleIOfixed $ unweighted $ mh nSamples bus
--  in mhRunBusSingleObs
-- :}
-- [True,True,True]
--
-- Of course, it will need to be run more than twice to get a reasonable estimate.
mh :: MonadDistribution m => Int -> Traced m a -> m [a]
mh n (Traced (Pair m d)) = fmap (map output . NE.toList) (f n)
  where
    f k
      | k <= 0 = fmap (:| []) (fmap (traceT . Identity) $ runTraceT d)
      | otherwise = do
        (x :| xs) <- f (k - 1)
        y <- runTraceT $ mhTransFree m x
        return (traceT (Identity y) :| x : xs)
