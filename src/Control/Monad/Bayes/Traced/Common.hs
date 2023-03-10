{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Common
-- Description : Numeric code for Trace MCMC
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Common
  ( Trace,
    TraceT (..),
    runTraceT,
    traceT,
    hoist,
    output,
    probDensity,
    variables,
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
import Control.Monad.Bayes.Weighted
  ( Weighted,
    weighted,
  )
import Control.Monad.Bayes.Weighted qualified as Weighted
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Writer (WriterT (WriterT, runWriterT), runWriter)
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Product (..))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import GHC.Generics
import Numeric.Log (Log, ln)
import Statistics.Distribution.DiscreteUniform (discreteUniformAB)

data MHResult a = MHResult
  { success :: Bool,
    trace :: Trace a -- FIXME can we go without changing type signatures?
  }

-- | Collection of random variables sampler during the program's execution.
data TraceData = TraceData
  { -- | Sequence of random variables sampler during the program's execution.
    variablesTD :: [Double],
    -- | The probability of observing this particular sequence.
    probDensityTD :: Product (Log Double)
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (GenericSemigroupMonoid TraceData)

type Trace a = TraceT Identity a

traceData :: Trace a -> TraceData
traceData = snd . runWriter . getTraceT

 -- | Sequence of random variables sampler during the program's execution.
variables :: Trace a -> [Double]
variables = variablesTD . traceData

-- | The probability of observing this particular sequence.
probDensity :: Trace a -> Log Double
probDensity = getProduct . probDensityTD . traceData

newtype TraceT m a = TraceT {getTraceT :: WriterT TraceData m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

runTraceT :: Functor m => TraceT m a -> m (Trace a)
runTraceT = fmap (TraceT . WriterT . Identity) . runWriterT . getTraceT

traceT :: Functor m => m (Trace a) -> TraceT m a
traceT = TraceT . WriterT . fmap (runWriter . getTraceT)

hoist :: (forall x. m x -> m x) -> TraceT m a -> TraceT m a
hoist morph = TraceT . WriterT . morph . runWriterT . getTraceT

output :: Trace a -> a
output = fst . runWriter . getTraceT

singleton :: Applicative m => Double -> TraceT m Double
singleton u = TraceT $ WriterT $ pure (u, TraceData {variablesTD = [u], probDensityTD = 1})

-- FIXME Shouldn't we rather implement MonadFactor?
scored :: Applicative m => Log Double -> TraceT m ()
scored w = TraceT $ WriterT $ pure ((), TraceData {variablesTD = [], probDensityTD = Product w})

-- FIXME Note: This is more like Gibbs sampling
-- Note: We don't do a small step, but an arbitrarily big step in a parameter dimension. Why does this even work? It's probably not called Metropolis-Hastings?

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTrans :: MonadDistribution m => Weighted (State.Density m) a -> Trace a -> TraceT m a
mhTrans m trace = TraceT $ WriterT $ do
  let a = output trace
      t@TraceData {variablesTD = us, probDensityTD = p} = traceData trace
  let n = length us
  i <- discrete $ discreteUniformAB 0 (n - 1)
  u' <- random
  let us' = case splitAt i us of
        (xs, _ : ys) -> xs ++ (u' : ys)
        _ -> error "impossible"
  ((b, q), vs) <- State.density (weighted m) us'
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (getProduct p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  pure if accept then (b, TraceData vs $ Product q) else (a, t)

mhTransFree :: MonadDistribution m => Weighted (Free.Density m) a -> Trace a -> TraceT m a
mhTransFree m t = TraceT . WriterT $ runIdentity . runWriterT . getTraceT . trace <$> mhTransWithBool m t

-- | A single Metropolis-corrected transition of single-site Trace MCMC.
mhTransWithBool :: MonadDistribution m => Weighted (Free.Density m) a -> Trace a -> m (MHResult a)
mhTransWithBool m trace = do
  let a = output trace
      t@TraceData {variablesTD = us, probDensityTD = p} = traceData trace
  let n = length us
  i <- discrete $ discreteUniformAB 0 (n - 1)
  u' <- random
  let us' = case splitAt i us of
        (xs, _ : ys) -> xs ++ (u' : ys)
        _ -> error "mhTransWithBool: impossible"
  ((b, q), vs) <- runWriterT $ weighted $ Weighted.hoist (WriterT . Free.density us') m
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (getProduct p * fromIntegral (length vs)))
  success <- bernoulli ratio
  let trace = TraceT $ WriterT $ pure if success then (b, TraceData vs $ Product q) else (a, t)
  return MHResult {success, trace}

-- | A variant of 'mhTrans' with an external sampling monad.
mhTrans' :: MonadDistribution m => Weighted (Free.Density Identity) a -> Trace a -> TraceT m a
mhTrans' m = mhTransFree (Weighted.hoist (Free.hoist (return . runIdentity)) m)

-- | burn in an MCMC chain for n steps (which amounts to dropping samples of the end of the list)
burnIn :: Functor m => Int -> m [a] -> m [a]
burnIn n = fmap dropEnd
  where
    dropEnd ls = let len = length ls in take (len - n) ls
