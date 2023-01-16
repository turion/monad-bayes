{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
module Control.Monad.Bayes.Map where

-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
import Data.Map.Strict (Map, singleton, fromDistinctAscList, toList, fromList, union, mapKeys)

-- log-domain
import Numeric.Log
import Control.Arrow (first, (|||))

data MapT m b = forall a . Ord a => MapT
  { getMapT :: m (Map a (Log Double))
  , getFmap :: a -> b
  }

-- Internal use only, because it exposes the structure of the map and doesn't delete duplicates
runMapTInternal :: Functor m => MapT m b -> m [(b, Log Double)]
runMapTInternal MapT { getMapT, getFmap } = fmap (first getFmap) . toList <$> getMapT

-- | Removes duplicates and re-sorts the internal structure.
--   Can save space, and time in future calculations.
optimize :: (Functor m, Ord b) => MapT m b -> MapT m b
optimize mapTmb = MapT
  { getMapT = fromList <$> runMapTInternal mapTmb
  , getFmap = id
  }

union :: Applicative m => MapT m b -> MapT m b -> MapT m b
union (MapT getMapT1 getFmap1) (MapT getMapT2 getFmap2) = MapT
  { getMapT = do
      mapT1 <- getMapT1
      mapT2 <- getMapT2
      pure $ Data.Map.Strict.union (mapKeys Left mapT1) (mapKeys Right mapT2)
  , getFmap = getFmap1 ||| getFmap2
  }

instance Functor (MapT m) where
  fmap f MapT { getMapT, getFmap } = MapT
    { getMapT
    , getFmap = f . getFmap
    }

instance Applicative m => Applicative (MapT m) where
  pure a = MapT
    { getMapT = pure $ singleton () 1
    , getFmap = const a
    }

  MapT getMapT1 getFmap1 <*> MapT getMapT2 getFmap2 = MapT
    { getMapT = do
        map1 <- getMapT1
        map2 <- getMapT2
        pure $ fromDistinctAscList $ do
          (value1, prob1) <- toList map1
          (value2, prob2) <- toList map2
          return ((value1, value2), prob1 * prob2)
    , getFmap = \(a1, a2) -> getFmap1 a1 $ getFmap2 a2
    }

instance Monad m => Monad (MapT m) where
  MapT { getMapT, getFmap } >>= f = MapT
    { getMapT = do
        pure $ fromList $ concat $ _
    , getFmap = id
    }
