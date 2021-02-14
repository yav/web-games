module Bag where

import Data.Map(Map)
import qualified Data.Map as Map
import System.Random.TF(TFGen)
import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.Utils


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

bagDraw :: Ord a => Bag a -> TFGen -> Maybe (a, (Bag a, TFGen))
bagDraw b rng
  | bagIsEmpty b = Nothing
  | otherwise    = let (r,rng1) = pickWeighted (bagToList b) rng
                   in Just (r, (bagRemove r b, rng1))
