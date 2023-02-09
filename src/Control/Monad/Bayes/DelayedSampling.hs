{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.Bayes.DelayedSampling where

import Control.Monad.Bayes.Class hiding (Distribution)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.List.NonEmpty (unzip)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, cast)
import Statistics.Function (square)
import Prelude hiding (unzip)
import Data.Tuple (swap)
import Control.Arrow (first)

newtype Variable a = Variable Int
  deriving (Show, Eq)

-- FIXME do I need const here and realized later? Or can I get away with only variables?
-- I probably need const here still in order to deal with a num instance for value
-- Would be nice to have Functor & Applicative, but that is hard because we also need typeable
data Value a where
  Var :: Variable a -> Value a
  Const :: a -> Value a

deriving instance Show a => Show (Value a)

deriving instance Eq a => Eq (Value a)

-- FIXME: Use syb (or recursion-schemes?) to push variable indices by an Int

-- FIXME Maybe I should put the functor bla etc. here?
-- Or parametrize distribution constructors over a?
data Distribution a where
  Normal :: Value Double -> Value Double -> Distribution Double
  Beta :: Value Double -> Value Double -> Distribution Double

deriving instance Show (Distribution a)

deriving instance Eq (Distribution a)

pdf :: MonadDistribution m => Distribution a -> a -> DelayedSamplingT m (Log Double)
pdf (Normal mean stdDev) a = do
  meanValue <- sample mean
  stdDevValue <- sample stdDev
  return $ normalPdf meanValue stdDevValue a
pdf (Beta _alpha _beta) _ = error "Not implemented beta distribution pdf yet"

data Node a
  = Initialized
      { initialDistribution :: Distribution a,
        marginalDistribution :: Maybe (Distribution a)
      }
  | Realized a
  deriving (Show, Eq)

data SomeNode = forall a. (Eq a, Show a, Typeable a) => SomeNode {getSomeNode :: Node a}

deriving instance Show SomeNode

instance Eq SomeNode where
  SomeNode node1 == SomeNode node2 = fromMaybe False $ (node1 ==) <$> cast node2

castNode :: Typeable a => SomeNode -> Maybe (Node a)
castNode (SomeNode node) = cast node

newtype Graph = Graph {getGraph :: [SomeNode]}
  deriving (Show, Eq)

empty :: Graph
empty = Graph []

-- FIXME should be variable, not Int
data Error
  = IndexOutOfBounds Int
  | TypesInconsistent Int
  | AlreadyRealized Int
  | -- | Only exists for MonadFail
    Fail String
  deriving (Eq, Show)

newtype DelayedSamplingT m a = DelayedSamplingT {getDelayedSamplingT :: ExceptT Error (StateT Graph m) a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans DelayedSamplingT where
  lift = DelayedSamplingT . lift . lift

instance Monad m => MonadFail (DelayedSamplingT m) where
  fail = throw . Fail

throw :: Monad m => Error -> DelayedSamplingT m a
throw = DelayedSamplingT . throwE

tryElse :: Monad m => Error -> Maybe a -> DelayedSamplingT m a
tryElse e = maybe (throw e) return

-- FIXME look into lenses
onNode :: (Monad m, Eq a, Show a, Typeable a) => State (Node a) b -> Variable a -> DelayedSamplingT m b
onNode action (Variable i) = do
  graph <- DelayedSamplingT $ lift $ gets getGraph
  (graph', bMaybe) <- tryElse (IndexOutOfBounds i) $ modifyListSafe i (unzip . fmap (first SomeNode . swap . runState action) . castNode) graph
  DelayedSamplingT $ lift $ put $ Graph graph'
  tryElse (TypesInconsistent i) bMaybe
  -- tryElse (TypesInconsistent i) $ castNode someNode

lookupVar :: (Monad m, Typeable a, Eq a, Show a) => Variable a -> DelayedSamplingT m (Node a)
lookupVar = onNode get

-- FIXME all these are probably mean that I should use sequences for variables.
-- FIXME and named variables as well

modifyListSafe :: Int -> (a -> (Maybe a, b)) -> [a] -> Maybe ([a], b)
modifyListSafe i f as = case splitAt i as of
  (_prefix, []) -> Nothing
  (prefix, a : suffix) -> let (aMaybe, b) = f a in Just (prefix ++ (fromMaybe a aMaybe : suffix), b)

-- FIXME name sample?
interpret :: MonadDistribution m => Node a -> DelayedSamplingT m a
interpret (Realized value) = return value
interpret Initialized {initialDistribution, marginalDistribution = Nothing} = interpretDistribution initialDistribution
interpret Initialized {marginalDistribution = Just distribution} = interpretDistribution distribution

interpretDistribution :: MonadDistribution m => Distribution a -> DelayedSamplingT m a
interpretDistribution (Normal mean stdDev) = do
  meanValue <- sample mean
  stdDevValue <- sample stdDev
  lift $ normal meanValue stdDevValue
interpretDistribution (Beta a b) = do
  aValue <- sample a
  bValue <- sample b
  lift $ beta aValue bValue

-- unsafe: doesn't check whether already realized
realizeAs :: (Typeable a, Monad m, Show a, Eq a) => a -> Variable a -> DelayedSamplingT m ()
realizeAs a = onNode $ put $ Realized a

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

-- sample (Fmap f value) = f <$> sample value
-- sample (Ap f value) = sample f <*> sample value

runDelayedSamplingT :: Functor m => DelayedSamplingT m a -> m (Either Error a, Graph)
runDelayedSamplingT = flip runStateT empty . runExceptT . getDelayedSamplingT

evalDelayedSamplingT :: Functor m => DelayedSamplingT m a -> m (Either Error a)
evalDelayedSamplingT = fmap fst . runDelayedSamplingT

newVar :: (Monad m, Typeable a, Show a, Eq a) => Distribution a -> DelayedSamplingT m (Variable a)
newVar initialDistribution = DelayedSamplingT $ lift $ do
  Graph distributions <- get
  -- FIXME this is inefficient
  put $ Graph $ distributions ++ [SomeNode Initialized {initialDistribution, marginalDistribution = Nothing}]
  return $ Variable $ length distributions

normalDS :: Monad m => Value Double -> Value Double -> DelayedSamplingT m (Variable Double)
normalDS mean stdDev = newVar $ Normal mean stdDev

-- FIXME this would be a lot easier if I had a view function that dereferences the variables

-- FIXME I'd like to observe on Value a, but I don't know how to do that with var1 + var2
-- FIXME read the paper again and try to abstract the MARGINALIZE step better
observe :: (MonadMeasure m, Typeable a, Show a, Eq a) => Variable a -> a -> DelayedSamplingT m ()
observe variable@(Variable i) a = do
  node <- lookupVar variable
  -- FIXME this doesn't yet work if I have realized nodes, those play the same role as Const!
  case node of
    -- FIXME this implements the normal distribution as a single-parameter distribution. But there is an exp. family for the double parameter as well!
    Initialized {initialDistribution = Normal (Var varMean) stdDev, marginalDistribution = Nothing} -> do
      -- FIXME this sampling before or after the lookup? in principle they could intertwine, or not? Or is that a topology forbidden by the paper algo?
      stdDevValue <- sample stdDev
      meanNode <- lookupVar varMean
      case meanNode of
        Initialized { marginalDistribution = Just (Normal (Const priorMean) (Const priorStdDev))} -> do
        -- FIXME what does the original algorithm do when these are random as well? sample them first? or send further messages down?
        -- no actually marginalize it first completely!
          let precision = 1 / sqrt (1 / square stdDevValue + 1 / square priorStdDev)
              newMean = (a / square stdDevValue + priorMean / square priorStdDev) / precision
          setMarginalized varMean $ Normal (Const newMean) (Const $ 1 / sqrt precision)
        Initialized {} -> do
          _mean <- sample $ Var varMean
          -- FIXME dirty hack because the recursion over the graph is not yet optimal
          observe variable a
        Realized mean -> do
          -- FIXME dirty hack because I treat Const and Realized differently
          -- FIXME does this handle stdDev correctly?
          setMarginalized variable $ Normal (Const mean) stdDev
          -- FIXME dirty hack because the recursion over the graph is not yet optimal
          observe variable a
    Initialized { marginalDistribution = Just distribution } -> do
      realizeAs a variable
      p <- pdf distribution a
      lift $ score p
    -- FIXME Is this even allowed?
    Initialized { initialDistribution, marginalDistribution = Nothing } -> do
      realizeAs a variable
      p <- pdf initialDistribution a
      lift $ score p
    Realized _ -> throw (AlreadyRealized i)

-- FIXME this is just a step of marginalizing and might have a better name in the paper
-- FIXME unsafe, doesn't check whether already realized
setMarginalized :: (Monad m, Typeable a, Show a, Eq a) => Variable a -> Distribution a -> DelayedSamplingT m ()
setMarginalized variable marginalDistribution = flip onNode variable $ do
  node <- get
  case node of
    Realized _ -> error "Already realized" -- FIXME should really have a local monad with error
    initialized@Initialized { } -> put initialized { marginalDistribution = Just marginalDistribution }

debugGraph :: Monad m => DelayedSamplingT m Graph
debugGraph = DelayedSamplingT $ lift get

debugGraphIO :: MonadIO m => DelayedSamplingT m ()
debugGraphIO = DelayedSamplingT $ lift get >>= (liftIO . print)
