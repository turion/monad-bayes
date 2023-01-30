{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Bayes.Map where

-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
-- containers
import Data.Map.Strict (Map, singleton, fromDistinctAscList, toList, fromList, union, mapKeys, unionWith, empty, keys)

-- kan-extensions
import Data.Functor.Coyoneda

-- log-domain
import Numeric.Log
import Control.Arrow (first, (|||), second)
import Control.Monad.Trans
import Data.Functor.Classes
import Control.Monad.Trans.Free.Ap
import Control.Monad.Bayes.Population (runPopulation, Population, population)
import Control.Monad (join)
import Data.Foldable (Foldable(fold))
import Control.Foldl (Fold)

newtype PMap a = PMap { getPMap :: Map a (Log Double) }

instance Ord a => Semigroup (PMap a) where
  pmap1 <> pmap2 = PMap $ unionWith (+) (getPMap pmap1) (getPMap pmap2)

instance Ord a => Monoid (PMap a) where
  mempty = PMap empty

instance Foldable PMap where
  foldMap f = foldMap f . keys . getPMap

singletonPMap :: a -> PMap a
singletonPMap a = PMap $ singleton a 1

factorPMap :: Log Double -> PMap a -> PMap a
factorPMap p = PMap . fmap (* p) . getPMap

runPMap :: PMap a -> [(a, Log Double)]
runPMap = toList . getPMap

factorFoldPMaps :: (Foldable f, Functor f, Ord a) => f (PMap a, Log Double) -> PMap a
factorFoldPMaps = fold . fmap (uncurry $ flip factorPMap)

joinPMap :: Ord a => PMap (PMap a) -> PMap a
joinPMap = factorFoldPMaps . runPMap

toCMap :: PMap a -> CMap a
toCMap = CMap . liftCoyoneda

newtype CMap a = CMap { getCMap :: Coyoneda PMap a }
  deriving (Functor, Foldable)

instance Ord a => Semigroup (CMap a) where
  cmap1 <> cmap2 = toCMap $ runCMap cmap1 <> runCMap cmap2

instance Ord a => Monoid (CMap a) where
  mempty = toCMap mempty

runCMap :: Ord a => CMap a -> PMap a
runCMap (CMap (Coyoneda f pmap)) = PMap $ mapKeys f $ getPMap pmap

factorFoldCMaps :: (Foldable f, Functor f, Ord a) => f (CMap a, Log Double) -> CMap a
factorFoldCMaps = toCMap . factorFoldPMaps . fmap (first runCMap)

joinCMap :: Ord a => CMap (CMap a) -> CMap a
joinCMap = toCMap . factorFoldPMaps . runCMapInternal . fmap runCMap

runCMapInternal :: CMap a -> [(a, Log Double)]
runCMapInternal (CMap (Coyoneda f as)) = map (first f) $ runPMap as

-- FIXME Traversable? Alternative?

instance Applicative CMap where
  pure a = CMap $ Coyoneda (const a) $ PMap $ singleton () 1
  CMap (Coyoneda f1 map1) <*> CMap (Coyoneda f2 map2) = CMap $ Coyoneda
    (\(a1, a2) -> f1 a1 $ f2 a2) $
    PMap $ fromDistinctAscList $ do
      (value1, prob1) <- toList $ getPMap map1
      (value2, prob2) <- toList $ getPMap map2
      return ((value1, value2), prob1 * prob2)

-- | The monad instance is much slower than Applicative, so make sure you have ApplicativeDo activated all the time!
newtype PopulationT m a = PopulationT { getPopulationT :: FreeT CMap m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

{-
runPopulationTCMap :: (Ord a, Monad m) => PopulationT m a -> m (CMap a)
runPopulationTCMap = fmap runFreeCMap . joinFreeT . getPopulationT
  where
    runFreeCMap :: Ord a => Free CMap a -> CMap a
    runFreeCMap = runFreeFCMap . runFree
    runFreeFCMap :: Ord a => FreeF CMap a (Free CMap a) -> CMap a
    runFreeFCMap (Pure a) = pure a
    runFreeFCMap (Free cmap) = joinCMap $ runFreeCMap <$> cmap
runPopulationTCMap :: (Ord a, Monad m) => PopulationT m a -> m (CMap a)
runPopulationTCMap = _ . getPopulationT
  where
    runFreeTCMap :: (Ord a, Monad m) => FreeT CMap m a -> m (CMap a)
    runFreeTCMap = join . fmap _ . runFreeT
    runFreeFCMap :: Ord a => FreeF CMap a (Free CMap a) -> CMap a
    runFreeFCMap (Pure a) = pure a
    runFreeFCMap (Free cmap) = joinCMap $ runFreeCMap <$> cmap
-}
runPopulationTCMap :: (Ord a, Monad m) => PopulationT m a -> m (CMap a)
runPopulationTCMap = iterT (fmap factorFoldCMaps . sequence . map strength . runCMapInternal) . fmap pure . getPopulationT
  where
    strength :: Functor m => (m a, b) -> m (a, b)
    strength (ma, b) = ( , b) <$> ma

pushM :: (Monad m) => m (PopulationT m b) -> PopulationT m b
pushM = PopulationT . FreeT . join . fmap (runFreeT . getPopulationT)

union :: PopulationT m b -> PopulationT m b -> PopulationT m b
union population1 population2 = PopulationT $ _

-- Internal use only, because it exposes the structure of the map and doesn't delete duplicates
runPopulationTInternal :: Monad m => PopulationT m b -> m [(b, Log Double)]
runPopulationTInternal = iterT runCMapInternalM . fmap (\b -> [(b, 1)]) . getPopulationT
  where
    runCMapInternalM :: Monad m => CMap (m [(a, Log Double)]) -> m [(a, Log Double)]
    runCMapInternalM = fmap concat . sequence . map (\(as, p) -> map (second (* p)) <$> as) . runCMapInternal

-- FIXME This ought to be faster by using union/concat
-- | Removes duplicates and re-sorts the internal structure.
--   O(n * log (n)) in the current size of the list.
--   Can save space, and time in future calculations.
optimize :: (Ord b, Monad m) => PopulationT m b -> PopulationT m b
optimize = PopulationT . FreeT . fmap (Free . fmap return . CMap . liftCoyoneda . PMap . fromList) . runPopulationTInternal

-- Efficiently concatenates
concatPopulation :: (Ord b, Monad m) => PopulationT m (PopulationT m b) -> PopulationT m b
concatPopulation = _ . getPopulationT
  where
    thing :: FreeF CMap (PopulationT m b) (FreeT CMap m a) -> PopulationT m b
    thing (Pure population) = population
    thing (Free cmap) = _

runPopulationT :: (Ord b, Monad m) => PopulationT m b -> m [(b, (Log Double))]
runPopulationT population = do
  step <- runFreeT $ getPopulationT population
  cmap <- case step of
    Pure b -> pure $ pure b
    Free x -> _
  return $ toList $ getPMap $ runCMap cmap

{-
union :: Applicative m => PopulationT m b -> PopulationT m b -> PopulationT m b
union (PopulationT getPopulationT1 getFmap1) (PopulationT getPopulationT2 getFmap2) = PopulationT
  { getPopulationT = do
      PopulationT1 <- getPopulationT1
      PopulationT2 <- getPopulationT2
      pure $ Data.Map.Strict.union (mapKeys Left PopulationT1) (mapKeys Right PopulationT2)
  , getFmap = (|||) <$> getFmap1 <*> getFmap2
  }

instance Functor (PopulationT m) where
  fmap f PopulationT { getPopulationT, getFmap } = PopulationT
    { getPopulationT
    , getFmap = (f .) <$> getFmap
    }

instance Applicative m => Applicative (PopulationT m) where
  pure a = PopulationT
    { getPopulationT = pure $ singleton () 1
    , getFmap = pure $ const a
    }

  PopulationT getPopulationT1 getFmap1 <*> PopulationT getPopulationT2 getFmap2 = PopulationT
    { getPopulationT = do
        map1 <- getPopulationT1
        map2 <- getPopulationT2
        pure $ fromDistinctAscList $ do
          (value1, prob1) <- toList map1
          (value2, prob2) <- toList map2
          return ((value1, value2), prob1 * prob2)
    , getFmap = do
        f1 <- getFmap1
        f2 <- getFmap2
        pure $ \(a1, a2) -> f1 a1 $ f2 a2
    }

instance Monad m => Monad (PopulationT m) where
  PopulationT { getPopulationT, getFmap } >>= f = PopulationT
    { getPopulationT = do
        pure $ fromList $ concat $ _
    , getFmap = id
    }
-}

{-
Generalisation:

* Weighted sets, by an arbitrary monoid
* Weighted by binary number probability (find library for that) gives a compression scheme for the type
  * Yet to figure out how to sensibly encode floats and integers
  * Time series inference gives time series compression
-}
