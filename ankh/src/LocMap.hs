module LocMap where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)
import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as JS
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap

import Hex

newtype LocMap a = LocMap (IntMap (IntMap a))
  deriving (Generic,Eq,Ord,Show)

lmEmpty :: LocMap a
lmEmpty = LocMap IntMap.empty

lmUnion :: LocMap a -> LocMap a -> LocMap a
lmUnion (LocMap a) (LocMap b) = LocMap (IntMap.unionWith IntMap.union a b)

lmFromList :: [(Loc,a)] -> LocMap a
lmFromList = foldr (uncurry lmInsert) lmEmpty

lmFromListWith :: (a -> a -> a) -> [(Loc,a)] -> LocMap a
lmFromListWith f xs =
  LocMap
    (IntMap.fromListWith u [ (x, IntMap.singleton y a) | (Loc x y, a) <- xs ])
  where
  u = IntMap.unionWith f




lmInsert :: Loc -> a -> LocMap a -> LocMap a
lmInsert (Loc x y) a (LocMap mp) =
  LocMap (IntMap.insertWith IntMap.union x (IntMap.singleton y a) mp)

lmLookup :: Loc -> LocMap a -> Maybe a
lmLookup (Loc x y) (LocMap mp) =
  do mp1 <- IntMap.lookup x mp
     IntMap.lookup y mp1

lmMapWithKey :: (Loc -> a -> b) -> LocMap a -> LocMap b
lmMapWithKey f (LocMap mp) = LocMap (IntMap.mapWithKey g mp)
  where
  g x     = IntMap.mapWithKey (h x)
  h x y a = f (Loc x y) a

imToJS :: (a -> JS.Value) -> IntMap a -> JS.Value
imToJS f x =
  JS.object [ JS.fromString (show k) JS..= f v | (k,v) <- IntMap.toList x ]

instance Semigroup (LocMap a) where
  (<>) = lmUnion

instance Monoid (LocMap a) where
  mempty  = lmEmpty
  mappend = lmUnion

instance ToJSON a => ToJSON (LocMap a) where
  toJSON (LocMap mp) = imToJS (imToJS JS.toJSON) mp


