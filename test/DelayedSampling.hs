module DelayedSampling where

import Test.Hspec

import Control.Monad.Bayes.DelayedSampling
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Bayes.Class

test :: SpecWith ()
test = describe "DelayedSampling" $ do

  it "samples the same value once realized" $ do
    Right (val1, val2) <- sampleIO $ runDelayedSamplingT $ do
      a <- normalDS (Const 0) (Const 1)
      (,) <$> sample a <*> sample a
    val1 `shouldBe` val2

  it "samples the same value as a direct draw" $ do
    Right val1 <- sampleIOfixed $ runDelayedSamplingT $ do
      a <- normalDS (Const 0) (Const 1)
      sample a
    val2 <- sampleIOfixed $ normal 0 1
    val1 `shouldBe` val2

  it "realizes variables even when applying calculation" $ do
    Right (val1, val2) <- sampleIO $ runDelayedSamplingT $ do
      a <- normalDS (Const 0) (Const 1)
      (,) <$> sample a <*> sample (a + 3)
    (val1 + 3) `shouldBe` val2
