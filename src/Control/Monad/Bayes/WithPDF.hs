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
module Control.Monad.Bayes.WithPDF where

-- base
import Control.Monad (liftM, ap)

-- log-domain
import Numeric.Log

-- monad-bayes
import Control.Monad.Bayes.Class

data WithPDFT m a = WithPDFT
  { sample :: m a
  , pdf :: Eq a => a -> m (Log Double)
  }

-- FIXME Implment Num

instance Monad m => Functor (WithPDFT m) where
  fmap = liftM
  -- FIXME This still produces Borel's paradox! Can we add rewrite rules for typical functions like `(* x)`, `(+ x)` etc.?

instance Monad m => Applicative (WithPDFT m) where
  pure a = WithPDFT
    { sample = pure a
    , pdf = \a' -> return $ if a == a' then 1 else 0
    }
  a <*> f = WithPDFT
    { sample = sample a <*> sample f
    , pdf = \b -> do
        f' <- sample f
        a' <- sample a
        let pdff = pdf f
            pdfa = pdf a
        _
    }

instance Monad m => Monad (WithPDFT m) where
  WithPDFT { sample } >>= f = WithPDFT
    { sample = do
        a <- sample
        let WithPDFT { sample } = f a
        sample
    , pdf = \b -> do
        a <- sample
        let WithPDFT { pdf } = f a
        pdf b
    }
  -- FIXME This doesn't satisfy `ma >>= return == ma`!
  -- Can we add a rewrite rule that enforces that? (+ a test that checks it)

instance MonadDistribution m => MonadDistribution (WithPDFT m) where
  random = WithPDFT
    { sample = random
    , pdf = const $ return 1
    }

  uniform lower upper = WithPDFT
    { sample = uniform lower upper
    , pdf = const $ return $ (1 /) $ Exp $ log $ lower - upper
    }

  normal mu sigma = WithPDFT
    { sample = normal mu sigma
    , pdf = return . normalPdf mu sigma
    }

instance MonadFactor m => MonadFactor (WithPDFT m) where
  score probability = WithPDFT
    { sample = score probability
    , pdf = const $ return 1
    }

-- | Semantically like @\a' -> condition $ a == a'@, but avoids Borel's paradox
observe :: (MonadFactor m, Eq a) => a -> WithPDFT m a -> m ()
observe a WithPDFT { pdf } = do
  probability <- pdf a
  score probability
