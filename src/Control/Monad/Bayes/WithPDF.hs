{- |
If we have two Markov kernels p(x | theta) and p(theta), in principle we can define those in monad-bayes with types prior :: MonadMeasure m => m theta and likelihood :: MonadMeasure m => theta -> m x. Trouble is, if we want to observe an input datum x , we'd like to condition on that data. There are three choices currently:
* Good: Use Reuben's Bayesian data structure
* Bad: use likelihood theta >>= (condition . (x ==)) which runs into Borel's paradox if x is continuous (e.g. a Double)
* Ugly: calculate the PDF for the likelihood separately by hand, which is tedious and error-prone, and use factor
The only trouble with Bayesian is that it doesn't fit in the rest of the framework. It's not a monad because it doesn't compose: If it was a monad we could create prior >>= likelihood, which is the marginal likelihood, which has to integrate over the whole parameter space of theta. Clearly this isn't computable in many cases.
So I had an idea about a compromise. If we can't integrate theta, maybe we can Monte-Carlo it with enough samples!

The idea here is to record both how to sample a value from a distribution as well as the PDF of the distribution. We can't always compute the pdf exactly in a hierarchical model, but we now can ergonomically sample from it! This gives us access to an observe function which is like scoring, except that we don't have to compute the PDF manually.
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Bayes.WithPdf where

-- base
import Control.Monad (liftM, ap)

-- log-domain
import Numeric.Log

-- operational

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Operational (ProgramT, interpretWithMonadT, singleton, view, ProgramViewT (..), viewT)

data WithPdf m a = WithPdf
  { sample :: m a
  , pdf :: a -> m (Log Double)
  }

runWithPdf :: (Monad m, MonadFactor m) => WithPdf m a -> m a
runWithPdf WithPdf { sample, pdf} = do
  a <- sample
  p <- pdf a
  score p
  return a

newtype WithPdfT m a = WithPdfT {getWithPdfT :: ProgramT (WithPdf m) m a}
  deriving newtype (Functor, Applicative, Monad)

withPdfT :: WithPdf m a -> WithPdfT m a
withPdfT = WithPdfT . singleton

-- FIXME Implment Num

runWithPdfT :: (Monad m, MonadFactor m) => WithPdfT m a -> m a
runWithPdfT = runWithPdfT' . getWithPdfT
 where
  runWithPdfT' :: (Monad m, MonadFactor m) => ProgramT (WithPdf m) m a -> m a
  runWithPdfT' = interpretWithMonadT runWithPdf


instance MonadDistribution m => MonadDistribution (WithPdfT m) where
  random = withPdfT WithPdf
    { sample = random
    , pdf = const $ return 1
    }

  uniform lower upper = withPdfT WithPdf
    { sample = uniform lower upper
    , pdf = const $ return $ (1 /) $ Exp $ log $ lower - upper
    }

  normal mu sigma = withPdfT WithPdf
    { sample = normal mu sigma
    , pdf = return . normalPdf mu sigma
    }

instance MonadMeasure m => MonadFactor (WithPdfT m) where
  score probability = withPdfT WithPdf
    { sample = return ()
    , pdf = const $ return probability
    }
    -- Not sure why I had this first:
  -- score probability = WithPdfT
  --   { sample = score probability
  --   , pdf = const $ return 1
  --   }

-- | Semantically like @\a' -> condition $ a == a'@, but avoids Borel's paradox whenever possible
observe :: forall a m . (MonadFactor m, Eq a) => a -> WithPdfT m a -> m ()
observe a = (observe' =<<) . viewT . getWithPdfT
 where
  observe' :: MonadFactor m => ProgramViewT (WithPdf m) m a -> m ()
  observe' (Return a') = condition $ a == a'
  -- FIXME I've never used pdf here!
  observe' (WithPdf {sample, pdf} :>>= f) = do
    b <- sample
    observe a $ WithPdfT $ f b
 -- WithPdfT { pdf } = do
  -- probability <- pdf a
  -- score probability
