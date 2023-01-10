{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Bayes.Hierarchical2 where
import GHC.Base (Symbol, Type)

data Var (vars :: [(Symbol, Type)]) m a where
  Here :: Var ('(name, a) ': vars) m a
  There :: Var vars m b -> Var ('(name, a) ': vars) m b

data Model (vars :: [(Symbol, Type)]) m a where
  Var :: Var vars m a -> Model vars m a
  Const :: Double -> Model vars m Double
  Normal :: Model vars m Double -> Double -> Model vars m Double

data ModelUpdate vars m a where
  Observe :: Var vars m a -> a -> ModelUpdate vars m a

type RandomT vars m a = ProgramT (ModelUpdate vars m a) m a
