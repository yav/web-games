module Bag where

import Data.Map(Map)
import qualified Data.Map as Map
import System.Random.TF(TFGen)
import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.Utils
import Resource


newtype Bag = Bag (Map Resource Int)
  deriving (Generic,ToJSON)

bagEmpty :: Bag
bagEmpty = Bag Map.empty

bagAdd :: Resource -> Bag -> Bag
bagAdd r (Bag b) = Bag (Map.insertWith (+) r 1 b)

bagRemove :: Resource -> Bag -> Bag
bagRemove r (Bag b) =
  Bag
  case Map.lookup r b of
    Just n | n > 1 -> Map.adjust (subtract 1) r b
    _              -> Map.delete r b

bagIsEmpty :: Bag -> Bool
bagIsEmpty (Bag b) = Map.null b

bagToList :: Bag -> [(Resource,Int)]
bagToList (Bag mp) = Map.toList mp

bagFromList :: [Resource] -> Bag
bagFromList = foldr bagAdd bagEmpty

bagDraw :: Bag -> TFGen -> Maybe (Resource, (Bag, TFGen))
bagDraw b rng
  | bagIsEmpty b = Nothing
  | otherwise    = let (r,rng1) = pickWeighted (bagToList b) rng
                   in Just (r, (bagRemove r b, rng1))
