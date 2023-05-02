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
import Control.Monad.Bayes.Traced.Basic qualified as Basic
import Control.Monad.Bayes.Traced.Common
  ( MHResult (MHResult, trace),
    TraceT,
    burnIn,
    mhTransWithBool,
    runTraceT,
    traceT, Trace, probDensity,
  )
import Control.Monad.Bayes.Traced.Dynamic qualified as Dynamic
import Control.Monad.Bayes.Traced.Static qualified as Static
import Control.Monad.Bayes.Weighted (Weighted, unweighted)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Pipes ((>->))
import Pipes qualified as P
import Pipes.Prelude qualified as P

data Proposal = SingleSiteMH

data MCMCConfig = MCMCConfig {proposal :: Proposal, numMCMCSteps :: Int, numBurnIn :: Int}

defaultMCMCConfig :: MCMCConfig
defaultMCMCConfig = MCMCConfig {proposal = SingleSiteMH, numMCMCSteps = 1, numBurnIn = 0}

mcmc :: MonadDistribution m => MCMCConfig -> Static.Traced (Weighted m) a -> m [a]
mcmc (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ Static.mh numMCMCSteps m

mcmcBasic :: MonadDistribution m => MCMCConfig -> Basic.Traced (Weighted m) a -> m [a]
mcmcBasic (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ Basic.mh numMCMCSteps m

mcmcDynamic :: MonadDistribution m => MCMCConfig -> Dynamic.Traced (Weighted m) a -> m [a]
mcmcDynamic (MCMCConfig {..}) m = burnIn numBurnIn $ unweighted $ Dynamic.mh numMCMCSteps m

-- -- | draw iid samples until you get one that has non-zero likelihood
independentSamples :: Monad m => Static.Traced m a -> P.Producer (MHResult a) m (Trace a)
independentSamples (Static.Traced (Pair _w d)) =
  P.repeatM (runTraceT d)
    >-> P.map (traceT . Identity)
    >-> P.takeWhile' ((== 0) . probDensity)
    >-> P.map (MHResult False)

-- | convert a probabilistic program into a producer of samples
mcmcP :: MonadDistribution m => MCMCConfig -> Static.Traced m a -> P.Producer (MHResult a) m ()
mcmcP MCMCConfig {..} m@(Static.Traced (Pair w _)) = do
  initialValue <- independentSamples m >-> P.drain
  ( P.unfoldr (fmap (Right . (\k -> (k, trace k))) . mhTransWithBool w) initialValue
      >-> P.drop numBurnIn
    )
