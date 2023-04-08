module Common.Bag where

import Data.Maybe(fromMaybe)
import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Map as Map
import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.RNG


newtype Bag a = Bag (Map a Int)
  deriving (Generic,ToJSON,Show)

bagEmpty :: Bag a
bagEmpty = Bag Map.empty

-- | Change the amount of something in the bag.
-- If the change is negative and there is not enough,
-- then remove as much as possible.
bagChange :: Ord a => Int -> a -> Bag a -> Bag a
bagChange x r (Bag b) =
  Bag
  let cur = Map.findWithDefault 0 r b
      new = cur + x
  in if new > 0 then Map.insert r new b else Map.delete r b

-- | Change the amount of something in the bag.
-- Fails if the change is negative and there isn't enough.
bagChangeMaybe :: Ord a => Int -> a -> Bag a -> Maybe (Bag a)
bagChangeMaybe x r (Bag b) =
  let cur = Map.findWithDefault 0 r b
      new = cur + x
  in case compare new 0  of
       GT -> Just (Bag (Map.insert r new b))
       EQ -> Just (Bag (Map.delete r b))
       LT -> Nothing

bagUnion :: Ord a => Bag a -> Bag a -> Bag a
bagUnion (Bag a) (Bag b) = Bag (Map.unionWith (+) a b)

bagDifference :: Ord a => Bag a -> Bag a -> Bag a
bagDifference (Bag a) (Bag b) = Bag (Map.differenceWith rm a b)
  where rm x y = if x > y then Just (x - y) else Nothing

bagSize :: Bag a -> Int
bagSize (Bag mp) = Map.foldl' (+) 0 mp

bagContains :: Ord a => a -> Bag a -> Int
bagContains a (Bag b) = Map.findWithDefault 0 a b

bagIsEmpty :: Bag a -> Bool
bagIsEmpty (Bag b) = Map.null b

bagToNumList :: Bag a -> [(a,Int)]
bagToNumList (Bag mp) = Map.toList mp

bagToList :: Bag a -> [a]
bagToList b = [ x | (a,n) <- bagToNumList b, x <- replicate n a ]

bagToSet :: Bag a -> Set a
bagToSet (Bag mp) = Map.keysSet mp

bagFromNumList :: Ord a => [(a,Int)] -> Bag a
bagFromNumList xs = Bag (Map.fromListWith (+) xs)

bagFromList :: Ord a => [a] -> Bag a
bagFromList = foldr (bagChange 1) bagEmpty

bagPick :: Ord a => Bag a -> RNG -> Maybe (a, RNG)
bagPick b rng
  | bagIsEmpty b = Nothing
  | otherwise    = let (r,rng1) = pickWeighted (bagToNumList b) rng
                   in Just (r, rng1)

bagRemoveAll :: Ord a => a -> Bag a -> (Int, Bag a)
bagRemoveAll r (Bag b) = (fromMaybe 0 mb, Bag b1)
  where
  (mb, b1) = Map.updateLookupWithKey (\_ _ -> Nothing) r b

bagMap :: (Ord a, Ord b) => (a -> b) -> Bag a -> Bag b
bagMap f m = bagFromNumList [ (f a, n) | (a,n) <- bagToNumList m ]
