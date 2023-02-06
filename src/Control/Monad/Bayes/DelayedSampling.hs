{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Monad.Bayes.DelayedSampling where

-- base
import Data.Maybe (listToMaybe)

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

-- monad-bayes
import Control.Monad.Bayes.Class hiding (Distribution)

newtype Variable = Variable Int
  deriving Show

-- FIXME do I need const here and realized later? Or can I get away with only variables?
-- I probably need const here still in order to deal with a num instance for value
data Value a where
  Var :: Variable -> Value Double
  Const :: a -> Value a
  VarPlus :: Variable -> Double -> Value Double
  Fmap :: (a -> b) -> Value a -> Value b
  Ap :: Value (a -> b) -> Value a -> Value b

instance Functor Value where
  fmap = Fmap

instance Applicative Value where
  pure = Const
  (<*>) = Ap

instance Num a => Num (Value a) where
  Var var + Const a = VarPlus var a
  Const a + Var var = VarPlus var a
  val1 + val2 = (+) <$> val1 <*> val2
  val1 * val2 = (*) <$> val1 <*> val2
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate
-- FIXME now I don't have show anymore because of the arbitrary function. any chance to get it back?
-- By showing a blackbox for a function? By overloading lambdas?

-- FIXME: Use syb (or recursion-schemes?) to push variable indices by an Int

data Distribution
  = Normal (Value Double) (Value Double)
  | Beta (Value Double) (Value Double)

data Node = Initialized Distribution | Realized Double

newtype Graph = Graph { getGraph :: [Node] }

empty :: Graph
empty = Graph []

data Error
  = IndexOutOfBounds Int
  deriving Show

newtype DelayedSamplingT m a = DelayedSamplingT { getDelayedSamplingT :: ExceptT Error (StateT Graph m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans DelayedSamplingT where
  lift = DelayedSamplingT . lift . lift

throw :: Monad m => Error -> DelayedSamplingT m a
throw = DelayedSamplingT . throwE

tryElse :: Monad m => Error -> Maybe a -> DelayedSamplingT m a
tryElse e = maybe (throw e) return

lookupVar :: Monad m => Variable -> DelayedSamplingT m Node
lookupVar (Variable i) = do
  graph <- DelayedSamplingT $ lift get
  tryElse (IndexOutOfBounds i) $ lookupListSafe i $ getGraph graph

-- FIXME all these are probably mean that I should use sequences for variables.
-- FIXME and named variables as well
lookupListSafe :: Int -> [a] -> Maybe a
lookupListSafe i = listToMaybe . drop i

replaceListSafe :: Int -> a -> [a] -> Maybe [a]
replaceListSafe i a as = case splitAt i as of
  (_prefix, []) -> Nothing
  (prefix, _ : suffix) -> Just $ prefix ++ (a : suffix)

-- FIXME name sample?
interpret :: MonadDistribution m => Node -> DelayedSamplingT m Double
interpret (Realized value) = return value
interpret (Initialized distribution) = interpretDistribution distribution

interpretDistribution :: MonadDistribution m => Distribution -> DelayedSamplingT m Double
interpretDistribution (Normal mean stdDev) = do
  meanValue <- sample mean
  stdDevValue <- sample stdDev
  lift $ normal meanValue stdDevValue
interpretDistribution (Beta a b) = do
  aValue <- sample a
  bValue <- sample b
  lift $ beta aValue bValue

realize :: MonadDistribution m => Variable -> DelayedSamplingT m Double
realize var@(Variable i) = do
  node <- lookupVar var
  result <- interpret node
  graph <- DelayedSamplingT $ lift get
  nodes <- tryElse (IndexOutOfBounds i) $ replaceListSafe i (Realized result) (getGraph graph)
  DelayedSamplingT $ lift $ put $ Graph nodes
  return result

-- FIXME name sample?
sample :: MonadDistribution m => Value a -> DelayedSamplingT m a
sample (Const a) = return a
sample (Var var) = realize var
sample (VarPlus var a) = (a +) <$> realize var
sample (Fmap f value) = f <$> sample value
sample (Ap f value) = sample f <*> sample value

runDelayedSamplingT :: Functor m => DelayedSamplingT m a -> m (Either Error a)
runDelayedSamplingT = fmap fst . flip runStateT empty . runExceptT . getDelayedSamplingT

newVar :: Monad m => Distribution -> DelayedSamplingT m Variable
newVar distribution = DelayedSamplingT $ lift $ do
  Graph distributions <- get
  put $ Graph $ Initialized distribution : distributions
  return $ Variable $ length distributions

normalDS :: Monad m => Value Double -> Value Double -> DelayedSamplingT m (Value Double)
normalDS mean stdDev = fmap Var $ newVar $ Normal mean stdDev

-- observe :: Variable ->
