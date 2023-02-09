{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Monad.Bayes.DelayedSampling where

-- base
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Typeable (cast, Typeable)

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

-- monad-bayes
import Control.Monad.Bayes.Class hiding (Distribution)
import Statistics.Function (square)
import Control.Monad.IO.Class (MonadIO (liftIO))

newtype Variable a = Variable Int
  deriving (Show, Eq)

-- FIXME do I need const here and realized later? Or can I get away with only variables?
-- I probably need const here still in order to deal with a num instance for value
-- Would be nice to have Functor & Applicative, but that is hard because we also need typeable
data Value a where
  Var :: Variable a -> Value a
  Const :: a -> Value a
  VarPlus :: Num a => Variable a -> a -> Value a
  Plus :: Num a => Value a -> Value a -> Value a
  Multiply :: Num a => Value a -> Value a -> Value a
  Abs :: Num a => Value a -> Value a
  Signum :: Num a => Value a -> Value a
  Negate :: Num a => Value a -> Value a

deriving instance Show a => Show (Value a)
deriving instance Eq a => Eq (Value a)

instance (Num a, Typeable a) => Num (Value a) where
  Var var + Const a = VarPlus var a
  Const a + Var var = VarPlus var a
  val1 + val2 = Plus val1 val2
  -- val1 + val2 = (+) <$> val1 <*> val2
  (*) = Multiply
  abs = Abs
  signum = Signum
  fromInteger = Const . fromInteger
  negate = Negate
-- FIXME When adding fmap: now I don't have show anymore because of the arbitrary function. any chance to get it back?
-- By showing a blackbox for a function? By overloading lambdas?

-- FIXME: Use syb (or recursion-schemes?) to push variable indices by an Int

-- FIXME Maybe I should put the functor bla etc. here?
-- Or parametrize distribution constructors over a?
data Distribution a where
  Normal :: Variable Double -> Variable Double -> Distribution Double
  Beta :: Variable Double -> Variable Double -> Distribution Double

deriving instance Show (Distribution a)

deriving instance Eq (Distribution a)

pdf :: MonadDistribution m => Distribution a -> a -> DelayedSamplingT m (Log Double)
pdf (Normal mean stdDev) a = do
  meanValue <- sample $ Var mean
  stdDevValue <- sample $ Var stdDev
  return $ normalPdf meanValue stdDevValue a
pdf (Beta _alpha _beta) _ = error "Not implemented beta distribution pdf yet"

data Node a = Initialized (Distribution a) | Realized a
  deriving (Show, Eq)

data SomeNode = forall a . (Eq a, Show a, Typeable a) => SomeNode { getSomeNode :: Node a }

deriving instance Show SomeNode

instance Eq SomeNode where
  SomeNode node1 == SomeNode node2 = fromMaybe False $ (node1 ==) <$> cast node2

castNode :: Typeable a => SomeNode -> Maybe (Node a)
castNode (SomeNode node) = cast node

newtype Graph = Graph { getGraph :: [SomeNode] }
  deriving (Show, Eq)

empty :: Graph
empty = Graph []

-- FIXME should be variable, not Int
data Error
  = IndexOutOfBounds Int
  | TypesInconsistent Int
  | AlreadyRealized Int
  -- | Only exists for MonadFail
  | Fail String
  deriving (Eq, Show)

newtype DelayedSamplingT m a = DelayedSamplingT { getDelayedSamplingT :: ExceptT Error (StateT Graph m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans DelayedSamplingT where
  lift = DelayedSamplingT . lift . lift

instance Monad m => MonadFail (DelayedSamplingT m) where

  fail = throw . Fail

throw :: Monad m => Error -> DelayedSamplingT m a
throw = DelayedSamplingT . throwE

tryElse :: Monad m => Error -> Maybe a -> DelayedSamplingT m a
tryElse e = maybe (throw e) return

lookupVar :: (Monad m, Typeable a) => Variable a -> DelayedSamplingT m (Node a)
lookupVar (Variable i) = do
  graph <- DelayedSamplingT $ lift get
  someNode <- tryElse (IndexOutOfBounds i) $ lookupListSafe i $ getGraph graph
  tryElse (TypesInconsistent i) $ castNode someNode

-- FIXME all these are probably mean that I should use sequences for variables.
-- FIXME and named variables as well
lookupListSafe :: Int -> [a] -> Maybe a
lookupListSafe i = listToMaybe . drop i

replaceListSafe :: Int -> a -> [a] -> Maybe [a]
replaceListSafe i a as = case splitAt i as of
  (_prefix, []) -> Nothing
  (prefix, _ : suffix) -> Just $ prefix ++ (a : suffix)

-- FIXME name sample?
interpret :: MonadDistribution m => Node a -> DelayedSamplingT m a
interpret (Realized value) = return value
interpret (Initialized distribution) = interpretDistribution distribution

interpretDistribution :: MonadDistribution m => Distribution a -> DelayedSamplingT m a
interpretDistribution (Normal mean stdDev) = do
  meanValue <- sample $ Var mean
  stdDevValue <- sample $ Var stdDev
  lift $ normal meanValue stdDevValue
interpretDistribution (Beta a b) = do
  aValue <- sample $ Var a
  bValue <- sample $ Var b
  lift $ beta aValue bValue

-- unsafe: doesn't check whether already realized
realizeAs :: (Typeable a, Monad m, Show a, Eq a) => a -> Variable a -> DelayedSamplingT m ()
realizeAs a (Variable i) = do
  graph <- DelayedSamplingT $ lift get
  nodes <- tryElse (IndexOutOfBounds i) $ replaceListSafe i (SomeNode $ Realized a) (getGraph graph)
  DelayedSamplingT $ lift $ put $ Graph nodes

realize :: (MonadDistribution m, Typeable a, Show a, Eq a) => Variable a -> DelayedSamplingT m a
realize var = do
  node <- lookupVar var
  result <- interpret node
  realizeAs result var
  return result

-- FIXME name sample?
sample :: (MonadDistribution m, Typeable a, Show a, Eq a) => Value a -> DelayedSamplingT m a
sample (Const a) = return a
sample (Var var) = realize var
sample (VarPlus var a) = (a +) <$> realize var
sample (Plus val1 val2) = (+) <$> sample val1 <*> sample val2
sample (Multiply val1 val2) = (*) <$> sample val1 <*> sample val2
sample (Abs val) = abs <$> sample val
sample (Signum val) = signum <$> sample val
sample (Negate val) = negate <$> sample val
-- sample (Fmap f value) = f <$> sample value
-- sample (Ap f value) = sample f <*> sample value

runDelayedSamplingT :: Functor m => DelayedSamplingT m a -> m (Either Error a, Graph)
runDelayedSamplingT = flip runStateT empty . runExceptT . getDelayedSamplingT

evalDelayedSamplingT :: Functor m => DelayedSamplingT m a -> m (Either Error a)
evalDelayedSamplingT = fmap fst . runDelayedSamplingT

newVar :: (Monad m, Typeable a, Show a, Eq a) => Distribution a -> DelayedSamplingT m (Variable a)
newVar distribution = DelayedSamplingT $ lift $ do
  Graph distributions <- get
  -- FIXME this is inefficient
  put $ Graph $ distributions ++ [SomeNode (Initialized distribution)]
  return $ Variable $ length distributions

normalDS :: Monad m => Variable Double -> Variable Double -> DelayedSamplingT m (Variable Double)
normalDS mean stdDev = newVar $ Normal mean stdDev

-- FIXME I'd like to observe on Value a, but I don't know how to do that with var1 + var2
-- FIXME read the paper again and try to abstract the MARGINALIZE step better
observe :: (MonadMeasure m, Typeable a, Show a, Eq a) => Variable a -> a -> DelayedSamplingT m ()
observe variable@(Variable i) a = do
  node <- lookupVar variable
  -- FIXME this doesn't yet work if I have realized nodes, those play the same role as Const!
  case node of
    -- FIXME this implements the normal distribution as a single-parameter distribution. But there is an exp. family for the double parameter as well!
    Initialized (Normal varMean stdDev) -> do
      -- FIXME this sampling before or after the lookup? in principle they could intertwine, or not? Or is that a topology forbidden by the paper algo?
      stdDevValue <- sample $ Var stdDev
      meanNode <- lookupVar varMean
      case meanNode of
        -- FIXME what does the original algorithm do when these are random as well? sample them first? or send further messages down?
        Initialized (Normal varPriorMean varPriorStdDev) -> do
        -- no actually marginalize it first completely!
          priorStdDev <- sample $ Var varPriorStdDev
          priorMean <- sample $ Var varPriorMean
          let precision = 1 / sqrt (1 / square stdDevValue + 1 / square priorStdDev)
              newMean = (a / square stdDevValue + priorMean / square priorStdDev) / precision
          reinitialize varMean $ Normal (Const newMean) (Const $ 1 / sqrt precision)
        Initialized _prior -> do
          _mean <- sample $ Var varMean
          -- FIXME dirty hack because the recursion over the graph is not yet optimal
          observe variable a
        Realized mean -> do
          -- FIXME dirty hack because I treat Const and Realized differently
          -- FIXME does this handle stdDev correctly?
          reinitialize variable $ Normal (Const mean) stdDev
          -- FIXME dirty hack because the recursion over the graph is not yet optimal
          observe variable a
    Initialized distribution -> do
      realizeAs a variable
      p <- pdf distribution a
      lift $ score p
    Realized _ -> throw (AlreadyRealized i)

-- FIXME this is just a step of marginalizing and might have a better name in the paper
-- FIXME unsafe, doesn't check whether already realized
reinitialize :: (Monad m, Typeable a, Show a, Eq a) => Variable a -> Distribution a -> DelayedSamplingT m ()
reinitialize (Variable i) distribution = do
  graph <- DelayedSamplingT $ lift get
  nodes <- tryElse (IndexOutOfBounds i) $ replaceListSafe i (SomeNode $ Initialized distribution) (getGraph graph)
  DelayedSamplingT $ lift $ put $ Graph nodes

debugGraph :: Monad m => DelayedSamplingT m Graph
debugGraph = DelayedSamplingT $ lift get

debugGraphIO :: MonadIO m => DelayedSamplingT m ()
debugGraphIO = DelayedSamplingT $ lift get >>= (liftIO . print)
