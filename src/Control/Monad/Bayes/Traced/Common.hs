{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Control.Monad.Bayes.Traced.Common
-- Description : Numeric code for Trace MCMC
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Common
  ( Trace (..),
    TraceT (..),
    output,
    singleton,
    scored,
    mhTrans,
    mhTransWithBool,
    mhTransFree,
    mhTrans',
    burnIn,
    MHResult (..),
  )
where

import Control.Monad.Bayes.Class
  ( MonadDistribution (bernoulli, random),
    discrete,
  )
import Control.Monad.Bayes.Density.Free qualified as Free
import Control.Monad.Bayes.Density.State qualified as State
import Control.Monad.Bayes.Weighted as Weighted
  ( Weighted,
    hoist,
    weighted,
  )
import Control.Monad.Writer (WriterT (WriterT, runWriterT), Writer)
import Data.Functor.Identity (Identity (runIdentity))
import Numeric.Log (Log, ln)
import Statistics.Distribution.DiscreteUniform (discreteUniformAB)

data MHResult a = MHResult
  { success :: Bool,
    trace :: TraceT Identity a
  }

-- | Collection of random variables sampler during the program's execution.
data Trace = Trace
  { -- | Sequence of random variables sampler during the program's execution.
    variables :: [Double],
    -- | The probability of observing this particular sequence.
    probDensity :: Log Double
  }

deriving via ([Double], Log Double) instance Monoid Trace

newtype TraceT m a = TraceT { getTraceT :: WriterT Trace m a }
  deriving (Functor, Applicative, Monad)

output :: Functor m => TraceT m a -> m a
output = fmap fst . runWriterT . getTraceT

singleton :: Applicative m => Double -> TraceT m Double
singleton u = TraceT $ WriterT $ pure (u, Trace {variables = [u], probDensity = 1})

-- FIXME Shouldn't we rather implement MonadFactor?
scored :: Applicative m => Log Double -> TraceT m ()
scored w = TraceT $ WriterT $ pure ((), Trace {variables = [], probDensity = w})

-- FIXME Note: This is more like Gibbs sampling
-- Note: We don't do a small step, but an arbitrarily big step in a parameter dimension. Why does this even work? It's probably not called Metropolis-Hastings?
-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadDistribution m => Weighted (State.Density m) a -> Trace -> a -> TraceT m a
mhTrans m t@Trace {variables = us, probDensity = p} a = TraceT $ WriterT $ do
  let n = length us
  i <- discrete $ discreteUniformAB 0 (n - 1)
  u' <- random
  let us' = case splitAt i us of
        (xs, _ : ys) -> xs ++ (u' : ys)
        _ -> error "impossible"
  ((b, q), vs) <- State.density (weighted m) us'
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  pure if accept then (b, Trace vs q) else (a, t)

mhTransFree :: MonadDistribution m => Weighted (Free.Density m) a -> Trace -> a -> TraceT m a
mhTransFree m t a = TraceT . WriterT $ runIdentity . runWriterT . getTraceT . trace <$> mhTransWithBool m t a

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTransWithBool :: MonadDistribution m => Weighted (Free.Density m) a -> Trace -> a -> m (MHResult a)
mhTransWithBool m t@Trace {variables = us, probDensity = p} a = do
  let n = length us
  i <- discrete $ discreteUniformAB 0 (n - 1)
  u' <- random
  let us' = case splitAt i us of
        (xs, _ : ys) -> xs ++ (u' : ys)
        _ -> error "mhTransWithBool: impossible"
  ((b, q), vs) <- runWriterT $ weighted $ Weighted.hoist (WriterT . Free.density us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  success <- bernoulli ratio
  let trace = TraceT $ WriterT $ pure if success then (b, Trace vs q) else (a, t)
  return MHResult {success, trace}

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' :: MonadDistribution m => Weighted (Free.Density Identity) a -> Trace -> a -> TraceT m a
mhTrans' m = mhTransFree (Weighted.hoist (Free.hoist (return . runIdentity)) m)

-- | burn in an MCMC chain for n steps (which amounts to dropping samples of the end of the list)
burnIn :: Functor m => Int -> m [a] -> m [a]
burnIn n = fmap dropEnd
  where
    dropEnd ls = let len = length ls in take (len - n) ls
