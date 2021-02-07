module Terrain where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

data Terrain = Plains | Forest | Swamp | Mountain
  deriving (Generic,ToJSON)

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

