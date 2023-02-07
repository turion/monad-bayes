module DelayedSampling where

import Test.Hspec

import Control.Monad.Bayes.DelayedSampling
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Weighted (runWeighted)

test :: SpecWith ()
test = describe "DelayedSampling" $ do
  describe "sampling" $ do
    it "samples the same value once realized" $ do
      Right (val1, val2) <- sampleIO $ evalDelayedSamplingT $ do
        a <- Var <$> normalDS (Const 0) (Const 1)
        (,) <$> sample a <*> sample a
      val1 `shouldBe` val2

    it "samples the same value as a direct draw" $ do
      Right val1 <- sampleIOfixed $ evalDelayedSamplingT $ do
        a <- Var <$> normalDS (Const 0) (Const 1)
        sample a
      val2 <- sampleIOfixed $ normal 0 1
      val1 `shouldBe` val2

    it "realizes variables even when applying calculation" $ do
      Right (val1, val2) <- sampleIO $ evalDelayedSamplingT $ do
        a <- Var <$> normalDS (Const 0) (Const 1)
        (,) <$> sample a <*> sample (a + 3)
      (val1 + 3) `shouldBe` val2
      Right (a, b, abSum) <- sampleIO $ evalDelayedSamplingT $ do
        a <- Var <$> normalDS (Const 0) (Const 1)
        b <- Var <$> normalDS (Const 0) (Const 1)
        (,,) <$> sample a <*> sample b <*> sample (a + b)
      (a + b) `shouldBe` abSum

    it "samples independent variables like in direct sampling" $ do
      Right val1 <- sampleIOfixed $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        b <- normalDS (Const 10) (Const 1)
        (,) <$> sample (Var a) <*> sample (Var b)
      val2 <- sampleIOfixed $ do
        a <- normal 0 1
        b <- normal 10 1
        return (a, b)
      val1 `shouldBe` val2

    it "samples variables in a hierarchical model like in direct sampling" $ do
      Right val1 <- sampleIOfixed $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        b <- normalDS (Var a) (Const 1)
        (,) <$> sample (Var a) <*> sample (Var b)
      val2 <- sampleIOfixed $ do
        a <- normal 0 1
        b <- normal a 1
        return (a, b)
      val1 `shouldBe` val2

  describe "observing" $ do
    it "adds a factor of the PDF when observing" $ do
      (Right (), p) <- sampleIO $ runWeighted $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        observe a 1
      p `shouldBe` normalPdf 0 1 1

    it "samples the observed value again" $ do
      (Right x, _) <- sampleIO $ runWeighted $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        observe a 1
        sample $ Var a
      x `shouldBe` 1

    it "throws an error when observing the same variable twice" $ do
      (Left e, _) <- sampleIO $ runWeighted $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        observe a 1
        observe a 2
      e `shouldBe` AlreadyRealized 0

    it "can observe variables in a hierarchical model" $ do
      ((Right a, graph), p) <- sampleIO $ runWeighted $ runDelayedSamplingT $ do
        debugGraphIO
        a <- normalDS (Const 0) (Const 2)
        debugGraphIO
        b <- normalDS (Var a) (Const 1)
        debugGraphIO
        observe b 1
        debugGraphIO
        sample $ Var a
      print graph
      print a
      print p
      p `shouldBe` normalPdf (4/5) (sqrt (5/4)) a
