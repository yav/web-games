module Terrain where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

data Terrain = Plains | Forest | Swamp | Mountain
  deriving (Eq,Generic,ToJSON)

extraEntryCost :: Terrain -> Int
extraEntryCost t =
  case t of
    Forest   -> 1
    Mountain -> 1
    _        -> 0

extraExitCost :: Terrain -> Int
extraExitCost t =
  case t of
    Swamp    -> 1
    Mountain -> 1
    _        -> 0

totalMoveCost :: Terrain -> Terrain -> Int
totalMoveCost from to = 1 + extraCost
  where
  extraCost
    | from == to = 0
    | otherwise  = extraExitCost from + extraEntryCost to

