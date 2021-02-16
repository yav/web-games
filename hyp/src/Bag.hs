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

bagAdd :: Ord a => a -> Bag a -> Bag a
bagAdd r (Bag b) = Bag (Map.insertWith (+) r 1 b)

bagRemove :: Ord a => a -> Bag a -> Bag a
bagRemove r (Bag b) =
  Bag
  case Map.lookup r b of
    Just n | n > 1 -> Map.adjust (subtract 1) r b
    _              -> Map.delete r b

bagIsEmpty :: Bag a -> Bool
bagIsEmpty (Bag b) = Map.null b

bagToList :: Bag a -> [(a,Int)]
bagToList (Bag mp) = Map.toList mp

bagFromList :: Ord a => [a] -> Bag a
bagFromList = foldr bagAdd bagEmpty

bagDraw :: Ord a => Bag a -> RNG -> (Maybe a, Bag a, RNG)
bagDraw b rng
  | bagIsEmpty b = (Nothing, b, rng)
  | otherwise    = let (r,rng1) = pickWeighted (bagToList b) rng
                   in (Just r, bagRemove r b, rng1)

bagDrawUpTo :: Ord a => Int -> Bag a -> RNG -> ([a], Bag a, RNG)
bagDrawUpTo n b rng
  | n > 0, (Just a, b1, rng1) <- bagDraw b rng =
    let (as,b2,rng2) = bagDrawUpTo (n-1) b1 rng1
    in (a:as,b2,rng2)

  | otherwise = ([], b, rng)


