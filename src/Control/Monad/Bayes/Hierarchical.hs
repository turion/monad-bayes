{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- base
import Data.Kind (Type)


-- operational
import Control.Monad.Operational
import GHC.Base (Symbol)
import Control.Monad.Bayes.Class (MonadDistribution, Kernel)
import Data.Proxy (Proxy)

type Priors = [(Symbol, Type)]

data MarkovKernel a (m :: Type -> Type) b where
  Normal :: Double -> MarkovKernel Double m Double
  Custom :: MonadDistribution m => (a -> m b) -> MarkovKernel a m b

sampleKernel :: MonadDistribution m => MarkovKernel a m b -> a -> m b
sampleKernel (Normal sigma) mu = normal mu sigma

data Hierarchical (priors :: Priors) (m :: Type -> Type) where
  -- Empty :: Hierarchical '[] m
  Prior :: MarkovKernel () m a -> Hierarchical '[ '(var, a)] m
  Likelihood :: MarkovKernel prior m a -> Hierarchical ('(var, prior) ': priors) m -> Hierarchical ('(var', a) ': '(var, prior) ': priors) m

-- FIXME I want to build up the model in RandomT as well. How do I do that?
-- * Polymorphism in priors? Needs special do extension and will make type checking hard
-- * Incomplete model type that will be validated?

data Real

-- FIXME doesn't need m
data Var (priors :: Priors) (m :: Type -> Type) a where
  VarHere :: Var ('(var, a) ': priors) m a
  VarThere :: Var priors m a -> Var ('(var, prior) ': priors) m a

class HasVar (name :: Symbol) a priors where
  var :: Proxy name -> Var priors m a

instance HasVar name a ('(name, a) ': priors) where
  var = const VarHere

instance HasVar name a priors => HasVar name a ('(otherName, b) ': priors) where
  var proxy = VarThere $ var proxy

sampleVar :: Hierarchical priors m -> Var priors m a -> m a
sampleVar (Likelihood kernel model) VarHere = do
  prior <- sampleVar model VarHere
  sampleKernel kernel prior
sampleVar (Prior kernel) VarHere = do
  sampleKernel kernel ()
sampleVar (Likelihood _kernel model) (VarThere var) = sampleVar model var

data RandomAction priors m a where
  VarAction :: Var priors m a -> RandomAction priors m a
  Observe :: Var priors m a -> a -> RandomAction priors m ()

type RandomT priors m a = ProgramT (RandomAction priors m) m a

runRandomT :: RandomT priors m a -> Hierarchical priors m -> m (a, Hierarchical priors m)
runRandomT prog model = do
  progView <- viewT prog
  case progView of
    Return a -> return (a, model)

    VarAction var :>>= f -> do
      b <- sampleVar model var
      runRandomT (f b) model

    Observe var a :>>= f -> _

initVar :: HasVar name a model => Proxy name -> MarkovKernel prior m a -> prior -> RandomT model m a
initVar = _

myModel :: RandomT '[ '("mu", Double), '("y", Double)] m Double
myModel = do
  mu <- initVar (Proxy @"mu") $ normal 0 1
  y <- var @"y" $ normal mu 1
  observe y 2
  return mu
