{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Monad.Bayes.Traced.Class where

import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Bayes.Inference.MCMC.Config (MCMCConfig (..))
import Control.Monad.Bayes.Inference.SMC (SMCConfig (..)) -- FIXME want to move its config in separate module and smc algo to .SMC?
import Control.Monad.Bayes.Population (Population, spawn)
import Control.Monad.Bayes.Sequential.Coroutine (Sequential, hoistFirst, sequentially)
import Data.Monoid (Endo (..))
import Control.Monad.Bayes.Traced.Common (Trace, MHResult)
import Control.Monad.Bayes.Weighted (Weighted)
import qualified Control.Monad.Bayes.Density.Free as Free

-- | Record ("trace") random choices made during the program.
class (forall m. Monad m => Monad (t m)) => HasTraced t where
  -- FIXME Generic implementation from runMHResult?
  -- | A single step of the Trace Metropolis-Hastings algorithm.
  mhStep :: MonadDistribution m => t m a -> t m a

  -- FIXME Generic implementation
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
  mh :: MonadDistribution m => Int -> t m a -> m [a]

  -- | Apply a monad morphism to the trace, but not the density.
  hoistTrace :: (forall x. m x -> m x) -> t m a -> t m a

  -- | Freeze all traced random choices to their current values and stop tracing them.
  freeze :: Monad m => t m a -> t m a
  freeze = id

  -- | Discard the trace and supporting infrastructure.
  marginal :: Monad m => t m a -> m a

  -- FIXME Should this do an MH step at all? Or should it only extract the tracing thing?
  -- FIXME If does an MH step, it should go into t m, not m. But then it doubles the trace in the monad and the result, which is probably fine
  -- FIXME naming: runMH? runTraced
  -- FIXME can refactor with getTrace?
  -- | Run and extract the tracing part.
  runMHResult :: MonadDistribution m => t m a -> m (MHResult a)

  getModel :: Monad m => t m a -> Weighted (Free.Density m) a

  getTrace :: Monad m => t m a -> m (Trace a)

-- FIXME move to RMSMC module?
-- | Resample-move Sequential Monte Carlo.
rmsmc ::
  (MonadDistribution m, HasTraced t) =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  Sequential (t (Population m)) a ->
  Population m a
rmsmc (MCMCConfig {..}) (SMCConfig {..}) =
  marginal
    . sequentially (freeze . composeCopies numMCMCSteps mhStep . hoistTrace resampler) numSteps
    . hoistFirst (hoistTrace (spawn numParticles >>))

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k = withEndo (mconcat . replicate k)

withEndo :: (Endo a -> Endo b) -> (a -> a) -> b -> b
withEndo f = appEndo . f . Endo
