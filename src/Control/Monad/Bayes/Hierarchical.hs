{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- base
import Data.Kind (Type)

data Hierarchical (priors :: [(String, Type)]) m a where
  Likelihood :: (prior -> m a) -> Hierarchical ('(var, prior) ': '[]) m a
  Hyperprior :: (hyperprior -> m prior) -> Hierarchical ('(var, prior) ': priors) m a -> Hierarchical ('var', hyperprior) ': '(var, prior) ': priors) m a
