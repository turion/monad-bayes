{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Control.Monad.Bayes.Inference.MCMC
-- Description : Markov Chain Monte Carlo (MCMC)
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Inference.MCMC where

import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Bayes.Inference.MCMC.Config (MCMCConfig (..))
import Control.Monad.Bayes.Traced.Class
import Control.Monad.Bayes.Traced.Common
  ( MHResult (MHResult, trace),
    Trace,
    burnIn,
    mhTransWithBool,
    probDensity,
  )
import Control.Monad.Bayes.Weighted (Weighted, unweighted)
import Pipes ((>->))
import Pipes qualified as P
import Pipes.Prelude qualified as P
import qualified Control.Monad.Bayes.Traced.Static as Static

mcmc :: (MonadDistribution m, HasTraced t) => MCMCConfig -> t (Weighted m) a -> m [a]
mcmc (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ mh numMCMCSteps m

mcmcStatic :: MonadDistribution m => MCMCConfig -> Static.Traced (Weighted m) a -> m [a]
mcmcStatic = mcmc

-- | draw iid samples until you get one that has non-zero likelihood
independentSamples :: (HasTraced t, MonadDistribution m) => t m a -> P.Producer (MHResult a) m (Trace a)
independentSamples traced =
  P.repeatM (runMHResult traced)
    >-> P.map trace
    >-> P.takeWhile' ((== 0) . probDensity)
    >-> P.map (MHResult False)

-- FIXME does this make sense for every traced?

-- | convert a probabilistic program into a producer of samples
mcmcP :: (MonadDistribution m, HasTraced t) => MCMCConfig -> t m a -> P.Producer (MHResult a) m ()
mcmcP MCMCConfig {..} traced = do
  initialValue <- independentSamples traced >-> P.drain
  ( P.unfoldr
      ( fmap (Right . (\k -> (k, trace k)))
          . mhTransWithBool (getModel traced)) initialValue
          >-> P.drop numBurnIn
      )
