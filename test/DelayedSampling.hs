{-# LANGUAGE NamedFieldPuns #-}

module DelayedSampling where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.DelayedSampling
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Bayes.Weighted (runWeighted)
import Data.Typeable (cast)
import Test.HUnit (assertFailure)
import Test.Hspec

shouldBeRight :: Show e => Either e a -> IO a
shouldBeRight = either (\e -> assertFailure $ "Expected Right, got Left (" ++ show e ++ ")") return

shouldBeLeft :: Show b => Either a b -> IO a
shouldBeLeft = either return $ \b -> assertFailure $ "Expected Left, got Right (" ++ show b ++ ")"

test :: SpecWith ()
test = describe "DelayedSampling" $ do
  describe "sample" $ do
    it "can sample" $ do
      val <- (shouldBeRight =<<) $ sampleIO $ evalDelayedSamplingT $ normalDS (Const 0) (Const 1)
      val `shouldBe` val -- Forces and checks whether not NaN
    it "should not allow to sample the same variable twice" $ do
      ErrorTrace {error_ = AlreadyRealized (ResolvedVariable {variable})} <- (shouldBeLeft =<<) $ sampleIO $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        (,) <$> sample a <*> sample a
      variable `shouldBe` Variable 0

    it "samples the same value as a direct draw" $ do
      val1 <- (shouldBeRight =<<) $ sampleIOfixed $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        sample a
      val2 <- sampleIOfixed $ normal 0 1
      val1 `shouldBe` val2

    it "samples independent variables like in direct sampling" $ do
      val1 <- (shouldBeRight =<<) $ sampleIOfixed $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        b <- normalDS (Const 10) (Const 1)
        (,) <$> sample a <*> sample b
      val2 <- sampleIOfixed $ do
        a <- normal 0 1
        b <- normal 10 1
        return (a, b)
      val1 `shouldBe` val2

    it "samples variables in a hierarchical model like in direct sampling" $ do
      val1 <- (shouldBeRight =<<) $ sampleIOfixed $ evalDelayedSamplingT $ do
        a <- addTrace "a" $ normalDS (Const 0) (Const 1)
        b <- addTrace "b" $ normalDS (Var a) (Const 1)
        debugGraphIO
        (,) <$> addTrace "sample a" (sample a) <*> (debugGraphIO >> addTrace "sample b" (sample b))
      val2 <- sampleIOfixed $ do
        a <- normal 0 1
        b <- normal a 1
        return (a, b)
      val1 `shouldBe` val2

  describe "observe" $ do
    it "adds a factor of the PDF when observing" $ do
      (Right (), p) <- sampleIO $ runWeighted $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        observe a 1
      p `shouldBe` normalPdf 0 1 1

    it "cannot sample an observed value again" $ do
      (result, _) <- sampleIO $ runWeighted $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        observe a 1
        sample a
      AlreadyRealized ResolvedVariable {variable} <- error_ <$> shouldBeLeft result
      getVariable variable `shouldBe` 0

    it "throws an error when observing the same variable twice" $ do
      (result, _) <- sampleIO $ runWeighted $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        observe a 1
        observe a 2
      AlreadyRealized ResolvedVariable {variable} <- error_ <$> shouldBeLeft result
      getVariable variable `shouldBe` 0

    it "can observe variables in a hierarchical model and weight correctly" $ do
      (_, p) <- sampleIO $ runWeighted $ runDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 2)
        b <- normalDS (Var a) (Const 1)
        observe b 1
        sample a
      p `shouldBe` normalPdf 0 (sqrt 3) 1

  describe "examples" $ do
    it "reproduces the example from the paper" $ do
      (result, _) <- sampleIO $ runWeighted $ evalDelayedSamplingT $ do
        a <- normalDS (Const 0) (Const 1)
        b <- normalDS (Var a) (Const 1)
        c <- normalDS (Var b) (Const 1)
        d <- normalDS (Var b) (Const 1)
        _ <- normalDS (Var c) (Const 1)
        _ <- normalDS (Var c) (Const 1)
        graft c
        graph1 <- debugGraph
        graft d
        graph2 <- debugGraph
        return (graph1, graph2)
      (graph1, graph2) <- shouldBeRight result
      getGraph graph1
        `shouldBe` [ SomeNode {getSomeNode = Initialized {initialDistribution = Normal (Const 0) (Const 1), marginalDistribution = Just $ Normal (Const 0) (Const 1)}},
                     SomeNode {getSomeNode = Initialized {initialDistribution = Normal (Var (Variable 0)) (Const 1), marginalDistribution = Just $ Normal (Const 0) (Const 2)}},
                     SomeNode {getSomeNode = Initialized {initialDistribution = Normal (Var (Variable 1)) (Const 1), marginalDistribution = Just $ Normal (Const 0) (Const 3)}},
                     SomeNode {getSomeNode = Initialized {initialDistribution = Normal (Var (Variable 1)) (Const 1), marginalDistribution = Nothing}},
                     SomeNode {getSomeNode = Initialized {initialDistribution = Normal (Var (Variable 2)) (Const 1), marginalDistribution = Nothing}},
                     SomeNode {getSomeNode = Initialized {initialDistribution = Normal (Var (Variable 2)) (Const 1), marginalDistribution = Nothing}}
                   ]
      c <- case getGraph graph2 !! 2 of
        SomeNode {getSomeNode} -> case cast getSomeNode :: Maybe (Node Double) of
          Just (Realized c) -> pure c
          _ -> assertFailure "Was not realized"
      getGraph graph2
        `shouldBe` [ SomeNode {getSomeNode = Initialized {initialDistribution = Normal (Const 0) (Const 1), marginalDistribution = Just $ Normal (Const 0) (Const 1)}},
                     SomeNode {getSomeNode = Initialized {initialDistribution = Normal (Var (Variable 0)) (Const 1), marginalDistribution = Just $ Normal (Const $ c * 2 / 3) (Const $ 2 / 3)}},
                     SomeNode {getSomeNode = Realized c},
                     SomeNode {getSomeNode = Initialized {initialDistribution = Normal (Var (Variable 1)) (Const 1), marginalDistribution = Just $ Normal (Const $ c * 2 / 3) (Const $ 5 / 3)}},
                     SomeNode {getSomeNode = Initialized {initialDistribution = Normal ((Var (Variable 2))) (Const 1), marginalDistribution = Just $ Normal (Const c) (Const 1)}},
                     SomeNode {getSomeNode = Initialized {initialDistribution = Normal ((Var (Variable 2))) (Const 1), marginalDistribution = Just $ Normal (Const c) (Const 1)}}
                   ]