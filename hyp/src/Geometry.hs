module Geometry where

import Data.Map(Map)
import qualified Data.Map as Map

import Common.Utils(enumAll)
import Common.Field

import Tile

type Loc      = (Int,Int)
newtype Board = Board (Map Loc Tile)

data Dir = NE | E | SE | SW | W | NW
  deriving (Eq,Ord,Show,Enum,Bounded)

toCart :: Dir -> Int -> (Int,Int)
toCart dir n =
  case dir of
    NE -> (0,n)
    E  -> (n,0)
    SE -> (n,-n)
    SW -> (0,-n)
    W  -> (-n,0)
    NW -> (-n,n)

tileAt :: Loc -> Field Board Tile
tileAt loc = brd .> mapAt loc
  where
  brd = Field { getField = \(Board b) -> b
              , setField = \m _ -> Board m
              }

onBoard :: Loc -> Board -> Bool
onBoard l (Board b) = l `Map.member` b

neighbours :: Loc -> Board -> [Loc]
neighbours (x,y) b = [ loc
                     | d <- enumAll
                     , let (dx,dy) = toCart d 1
                           loc     = (x + dx, y + dy)
                     , onBoard loc b
                     ]



