{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Monad.Bayes.Traced.Class where

import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Bayes.Inference.MCMC (MCMCConfig (..))
import Control.Monad.Bayes.Inference.SMC (SMCConfig (..))
import Control.Monad.Bayes.Population (Population, spawn)
import Control.Monad.Bayes.Sequential.Coroutine (Sequential, hoistFirst, sequentially)
import Data.Monoid (Endo (..))

-- | Record ("trace") random choices made during the program.
class (forall m. Monad m => Monad (t m)) => Traced t where
  -- | A single step of the Trace Metropolis-Hastings algorithm.
  mhStep :: MonadDistribution m => t m a -> t m a

  -- | Apply a monad morphism to the trace, but not the density.
  hoistTrace :: (forall x. m x -> m x) -> t m a -> t m a

  -- | Freeze all traced random choices to their current values and stop tracing them.
  freeze :: Monad m => t m a -> t m a
  freeze = id

  -- | Discard the trace and supporting infrastructure.
  marginal :: Monad m => t m a -> m a

-- | Resample-move Sequential Monte Carlo.
rmsmc ::
  (MonadDistribution m, Traced t) =>
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
