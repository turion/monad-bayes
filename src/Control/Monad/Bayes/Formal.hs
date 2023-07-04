{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Monad.Bayes.Formal where
import Control.Monad.Bayes.Class
import Control.Monad (ap, (>=>), liftM)
import Control.Monad.Trans (lift, MonadTrans)

data Dist a where
  Uniform :: Dist Double
  Normal :: Double -> Double -> Dist Double
  Bernoulli :: Double -> Dist Bool

pdf :: Dist a -> a -> Log Double
pdf Uniform x = if 0 <= x && x <= 1 then 1 else 0
pdf (Normal mu sigma) x = normalPdf mu sigma x
pdf (Bernoulli p) b = Exp $ log $ if b then p else 1 - p

sample :: MonadDistribution m => Dist a -> m a
sample Uniform = random
sample (Normal mu sigma) = normal mu sigma
sample (Bernoulli p) = bernoulli p

{-
newtype FormalT m a = FormalT { getFormalT :: ProgramT Dist m a }
  deriving (Functor, Applicative, Monad, MonadDistribution, MonadFactor, MonadMeasure )

instance MonadObserve (FormalT m) where
  observe' a action = do
    tip <- lift $ viewT action
    case tip of
      Lift ma -> do
        a' <- ma -- FIXME could also use MonadObserve from m
        condition $ a == a'
      ma :>>= f -> do
        a' <- ma
        observe' a $ f a'
      -- FIXME this is the wrong kind of free thing.
-}

data FreeAp f a where
  Nil :: FreeAp f ()
  (:*:) :: f a -> FreeAp f b -> FreeAp f (a, b)

runFreeAp :: MonadDistribution m => FreeAp Dist a -> m a
runFreeAp Nil = pure ()
runFreeAp (dist :*: fa) = (,) <$> sample dist <*> runFreeAp fa

data Coherence a b where
  UnitL :: Coherence ((), a) a
  UnitR :: Coherence (a, ()) a
  UnitLInv :: Coherence a ((), a)
  UnitRInv :: Coherence a (a, ())
  AssocL :: Coherence ((a, b), c) (a, (b, c))
  AssocR :: Coherence (a, (b, c)) ((a, b), c)

inv :: Coherence a b -> Coherence b a
inv UnitL = UnitLInv
inv UnitR = UnitRInv
inv UnitLInv = UnitL
inv UnitRInv = UnitR
inv AssocL = AssocR
inv AssocR = AssocL

runCoherence :: Coherence a b -> a -> b
runCoherence UnitL ((), a) = a
runCoherence UnitR (a, ()) = a
runCoherence UnitLInv a = ((), a)
runCoherence UnitRInv a = (a, ())
runCoherence AssocL ((a, b), c) = (a, (b, c))
runCoherence AssocR (a, (b, c)) = ((a, b), c)

data FreeM f m a where
  Lift :: m a -> FreeM f m a
  -- Iso :: (a -> b) -> (b -> a) -> FreeM f m a -> FreeM f m b
  TransportCoherence :: Coherence a b -> FreeM f m a -> FreeM f m b
  FreeAp :: FreeAp f a -> FreeM f m a
  -- FIXME or should the first one be FreeAp?
  (:>>=) :: FreeM f m a -> (a -> FreeM f m b) -> FreeM f m b

instance Applicative m => Functor (FreeM f m) where
  fmap = liftM

instance Applicative m => Applicative (FreeM f m) where
  pure = Lift . pure
  (<*>) = ap -- FIXME

instance Applicative m => Monad (FreeM f m) where
  (fa :>>= f) >>= g = fa :>>= (f >=> g)
  ma >>= f = ma :>>= f
  -- FIXME does that not loop?

instance MonadTrans (FreeM f) where
  lift = Lift

runFreeM :: MonadDistribution m => FreeM Dist m a -> m a
runFreeM (FreeAp fa) = runFreeAp fa
runFreeM (fa :>>= f) = runFreeM fa >>= runFreeM . f
runFreeM (Lift ma) = ma
-- runFreeM (Iso f _ ma) = f <$> runFreeM ma
runFreeM (TransportCoherence coh ma) = runCoherence coh <$> runFreeM ma

newtype FormalT m a = FormalT { getFormalT :: FreeM Dist m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

runFormalT :: MonadDistribution m => FormalT m a -> m a
runFormalT FormalT {getFormalT} = runFreeM getFormalT

liftDist :: Dist a -> FormalT m a
liftDist dist = FormalT $ TransportCoherence UnitR $ FreeAp $ dist :*: Nil

instance Applicative m => MonadDistribution (FormalT m) where
  random = liftDist Uniform

instance MonadFactor m => MonadFactor (FormalT m) where
  score = lift . score

instance MonadMeasure m => MonadObserve (FormalT m) where
  -- observe a FormalT { getFormalT } = --observeWith (==) a getFormalT
  observe a FormalT { getFormalT } = observeWith' (==) a getFormalT

observeWith' :: MonadMeasure m => (a -> a -> Bool) -> a -> FreeM Dist m a -> FormalT m ()
observeWith' _ a (FreeAp fa) = observeFreeAp' a fa
-- observeWith' eq a (Iso f g ma) = observeWith' (\a1 a2 -> eq (f a1) (f a2)) (g a) ma
observeWith' eq a (Lift ma) = do
  a' <- lift ma
  condition $ eq a a'
observeWith' eq a (fa :>>= f) = do
  a' <- lift $ runFreeM fa
  observeWith' eq a $ f a'
observeWith' eq a (TransportCoherence coh ma) = observeWith' (\a1 a2 -> eq (runCoherence coh a1) (runCoherence coh a2)) (runCoherence (inv coh) a) ma

{-
observeWith :: MonadMeasure m => (a -> a -> Bool) -> a -> FreeM Dist m (a, b) -> FormalT m b
observeWith _ a (FreeAp (fa :*: fas)) = do
  score $ pdf fa a
  FormalT $ FreeAp fas
-- observeWith eq a (Iso f g (Iso f' g' ma)) = observeWith eq a $ Iso (f . f') (g' . g) ma
-- observeWith eq a (Iso f g (FreeAp (fa :*: fas))) = _
-- observeWith _  _ (Iso f _ (FreeAp Nil)) = let (_, b) = f () in pure b
-- observeWith eq a (Iso f _ (Lift ma)) = observeWith eq a $ Lift $ f <$> ma
-- observeWith eq a (Iso f _ (fa :>>= h)) = observeWith eq a $ fa :>>= (fmap f . h)
observeWith eq a (Lift ma) = do
  (a', b) <- lift ma
  condition $ eq a a'
  return b
observeWith eq a (fa :>>= f) = do
  a' <- lift $ runFreeM fa
  observeWith eq a $ f a'
observeWith eq a (TransportCoherence coh (Lift ma)) = observeWith eq a $ Lift $ runCoherence coh <$> ma
observeWith eq a (TransportCoherence coh (fa :>>= h)) = observeWith eq a $ fa :>>= (fmap (runCoherence coh) . h)
observeWith eq a (TransportCoherence UnitR (FreeAp (fa :*: Nil))) = _
observeWith eq a (TransportCoherence UnitLInv ma) = _
observeWith eq a (TransportCoherence UnitRInv ma) = _
observeWith eq a (TransportCoherence AssocL ma) = _
observeWith eq a (TransportCoherence AssocR ma) = _
-}






-- -- FIXME This doesn't work, either move iso to FreeAp or do the pushing around in FreeM
-- observeFreeAp :: MonadFactor m => a -> FreeAp Dist (a, b) -> FormalT m b
-- observeFreeAp _ Nil = pure ()
-- observeFreeAp a (fa :*: fas) = case a of
--   (a1, b) -> do
--     score $ pdf fa a1
--     observeFreeAp b fas

observeFreeAp' :: MonadFactor m => a -> FreeAp Dist a -> FormalT m ()
observeFreeAp' _ Nil = pure ()
observeFreeAp' a (fa :*: fas) = case a of
  (a1, b) -> do
    score $ pdf fa a1
    observeFreeAp' b fas

-- Thought: RMSMC is per definition leaky if I keep all the old variables around. Only cleverer new versions could be better
{-
data NetworkTopology
  = Node Type
  | (:*:) NetworkTopology NetworkTopology
  | (:*>:) NetworkTopology NetworkTopology
  | (:+:) NetworkTopology NetworkTopology

-- Could also make a GADT
type family Semantics (network :: NetworkTopology) :: Type where
  Semantics (Node a) = a
  Semantics (n1 :*: n2) = (Semantics n1, Semantics n2)
  Semantics (n1 :+: n2) = Either (Semantics n1) (Semantics n2)
  Semantics (n1 :*>: n2) = (Semantics n1, Semantics n1 -> Semantics n2)

data Network (topology :: NetworkTopology) m a where
  NetworkNode :: m a -> Network (Node a) m a
  NetworkProduct :: Network t1 m a -> Network t2 m b -> Network (t1 :*: t2) m (a, b)
  NetworkSum :: Network t1 m a -> Network t2 m a -> Network (t1 :+: t2) m a
  NetworkImplication :: Network t1 m a -> Network t2 m b -> Network (t1 :*>: t2) m (a, b) -- FIXME naming

runNetwork :: Network t m a ->
-}
