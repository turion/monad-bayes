module Control.Monad.Bayes.ExponentialFamily where

data ExponentialFamily a where
  -- | How do I make sure in a function a -> ExponentialFamily b, the function stays the same but v can vary? It seems I need Arrows!
  ExponentialFamily :: VectorSpace v Double => (a -> v) -> v -> ExponentialFamily a
  -- ExponentialFamily :: VectorSpace v Double => (a -> v) -> (theta -> v) -> ExponentialFamily a

newtype ExponentialFamily m a = ExponentialFamily
  { getExponentialFamily :: Ap.FreeT (Applicative.Free ExponentialFamily) m a }
