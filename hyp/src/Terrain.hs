module Terrain where

data Terrain = Plains | Forest | Swamp | Mountain

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

