{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Control.Monad.Bayes.Traced.Dynamic
-- Description : Distributions on execution traces that can be dynamically frozen
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
module Control.Monad.Bayes.Traced.Dynamic
  ( Traced,
    hoistTrace,
    marginal,
    freeze,
    mhStep,
    mh,
  )
where

import Control.Monad (join)
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
    singleton, TraceT, output, runTraceT, traceT, mhTransWithBool,
  )
import Control.Monad.Bayes.Weighted (Weighted)
import Control.Monad.Trans (MonadTrans (..))
import Data.List.NonEmpty as NE (NonEmpty ((:|)), toList)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Product ( Product(..) )
import Control.Monad.Bayes.Traced.Class (HasTraced (..))

-- | A tracing monad where only a subset of random choices are traced and this
-- subset can be adjusted dynamically.
newtype Traced m a = Traced {getTraced :: Compose m (Product (Weighted (Density m)) (TraceT Identity)) a}
  deriving newtype (Functor, Applicative)

pushM :: Monad m => m (Weighted (Density m) a) -> Weighted (Density m) a
pushM = join . lift . lift

instance Monad m => Monad (Traced m) where
  Traced cx >>= f = Traced $ Compose $ do
    Pair mx tx <- getCompose cx
    let m = mx >>= pushM . fmap fstProduct . getCompose . getTraced . f
    -- FIXME apply monad law
    t <- return tx >>= (fmap sndProduct . getCompose . getTraced . f . output)
    return $ Pair m t

instance MonadTrans Traced where
  lift m = Traced $ Compose $ fmap (Pair (lift $ lift m) . pure) m

instance MonadDistribution m => MonadDistribution (Traced m) where
  random = Traced $ Compose $ fmap (Pair random . singleton) random

instance MonadFactor m => MonadFactor (Traced m) where
  score w = Traced $ Compose $ fmap (Pair (score w)) (score w >> pure (scored w))

instance MonadMeasure m => MonadMeasure (Traced m)

fstProduct :: Product f g a -> f a
fstProduct (Pair fa _) = fa

sndProduct :: Product f g a -> g a
sndProduct (Pair _ ga) = ga

instance HasTraced Traced where
  hoistTrace f (Traced c) = Traced (Compose $ f $ getCompose c)

  marginal (Traced c) = output . sndProduct <$> getCompose c

  freeze (Traced c) = Traced $ Compose $ do
    Pair _ t <- getCompose c
    let x = output t
    return $ Pair (pure x) (pure x)

  mhStep (Traced c) = Traced $ Compose $ do
    Pair m t <- getCompose c
    t' <- runTraceT $ mhTransFree m t
    return $ Pair m $ traceT $ Identity t'

  mh n (Traced c) = do
    Pair m t <- getCompose c
    let f k
          | k <= 0 = return (t :| [])
          | otherwise = do
            (x :| xs) <- f (k - 1)
            y <- runTraceT $ mhTransFree m x
            return (traceT (Identity y) :| x : xs)
    fmap (map output . NE.toList) (f n)

  runMHResult traced = do
    Pair model trace <- getCompose $ getTraced traced
    mhTransWithBool model trace

  getTrace = fmap sndProduct . getCompose . getTraced

  getModel = pushM . fmap fstProduct . getCompose . getTraced
