module Geometry where

import GHC.Generics(Generic)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(find)

import Data.Aeson(ToJSON(..))

import Common.Utils(enumAll)
import Common.Field

import Resource
import Tile

newtype Board = Board (Map Loc Tile)

data Loc      = Loc { locX, locY :: Int }
  deriving (Eq,Ord,Generic,ToJSON)

origin :: Loc
origin = Loc 0 0


data Dir = NE | E | SE | SW | W | NW
  deriving (Eq,Ord,Show,Enum,Bounded)

rot :: Int -> Dir -> Dir
rot n = toEnum . (`mod` 6) . (+n) . fromEnum

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

neighbour :: Dir -> Loc -> Loc
neighbour d (Loc x y) = Loc (x + dx) (y + dy)
  where (dx,dy) = toCart d 1

neighbours :: Loc -> Board -> [Loc]
neighbours l b = [ loc
                 | d <- enumAll
                 , let loc = neighbour d l
                 , onBoard loc b
                 ]


--------------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = Board Map.empty

placeTile :: Loc -> Tile -> Board -> Board
placeTile l t (Board b) = Board (Map.insert l t b)

placeStart :: Loc -> Dir -> Maybe Resource -> Board -> Board
placeStart l d r = placeTile (neighbour (rot 1 d) l) b
                 . placeTile (neighbour d l) a
                 . placeTile l p
  where
  findStart x = case find ((x ==) . tileNumber) startTiles of
                  Just t  -> setField tileVisible True t
                  Nothing -> error "Missing start tile."
  p = findStart Capital
  a = findStart (TNE r)
  b = findStart (TNW r)




--------------------------------------------------------------------------------

instance ToJSON Board where
  toJSON (Board b) = toJSON (Map.toList b)
