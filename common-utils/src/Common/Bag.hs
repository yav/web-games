module Common.Bag where

import Data.Maybe(fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map
import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.RNG


newtype Bag a = Bag (Map a Int)
  deriving (Generic,ToJSON)

bagEmpty :: Bag a
bagEmpty = Bag Map.empty

bagChange :: Ord a => Int -> a -> Bag a -> Bag a
bagChange x r (Bag b) =
  Bag
  let cur = Map.findWithDefault 0 r b
      new = cur + x
  in if new > 0 then Map.insert r new b else Map.delete r b

bagContains :: Ord a => a -> Bag a -> Int
bagContains a (Bag b) = Map.findWithDefault 0 a b

bagIsEmpty :: Bag a -> Bool
bagIsEmpty (Bag b) = Map.null b

bagToList :: Bag a -> [(a,Int)]
bagToList (Bag mp) = Map.toList mp

bagFromNumList :: Ord a => [(a,Int)] -> Bag a
bagFromNumList xs = Bag (Map.fromListWith (+) xs)

bagFromList :: Ord a => [a] -> Bag a
bagFromList = foldr (bagChange 1) bagEmpty

bagPick :: Ord a => Bag a -> RNG -> Maybe (a, RNG)
bagPick b rng
  | bagIsEmpty b = Nothing
  | otherwise    = let (r,rng1) = pickWeighted (bagToList b) rng
                   in Just (r, rng1)

bagRemoveAll :: Ord a => a -> Bag a -> (Int, Bag a)
bagRemoveAll r (Bag b) = (fromMaybe 0 mb, Bag b1)
  where
  (mb, b1) = Map.updateLookupWithKey (\_ _ -> Nothing) r b

