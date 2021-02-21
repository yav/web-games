module Bag where

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

bagIsEmpty :: Bag a -> Bool
bagIsEmpty (Bag b) = Map.null b

bagToList :: Bag a -> [(a,Int)]
bagToList (Bag mp) = Map.toList mp

bagFromList :: Ord a => [a] -> Bag a
bagFromList = foldr (bagChange 1) bagEmpty

bagPick :: Ord a => Bag a -> RNG -> Maybe (a, RNG)
bagPick b rng
  | bagIsEmpty b = Nothing
  | otherwise    = let (r,rng1) = pickWeighted (bagToList b) rng
                   in Just (r, rng1)



