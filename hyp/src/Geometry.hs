module Geometry where

import GHC.Generics(Generic)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(find)

import Data.Aeson(ToJSON(..),FromJSON)

import Common.Basics(PlayerId)
import Common.Utils(enumAll)
import Common.Field

import Bag(bagContains)
import Resource
import Terrain
import Tile

data Board = Board
  { _boardMap     :: Map Loc Tile
  , _boardCapital :: Map PlayerId Loc
  }

data Loc      = Loc { locX, locY :: Int }
  deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)

declareFields ''Board

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
tileAt loc = boardMap .> mapAt loc

onBoard :: Loc -> Board -> Bool
onBoard l b = l `Map.member` getField boardMap b

neighbour :: Dir -> Loc -> Loc
neighbour d (Loc x y) = Loc (x + dx) (y + dy)
  where (dx,dy) = toCart d 1

neighbours :: Loc -> Board -> [Loc]
neighbours l b = [ loc
                 | d <- enumAll
                 , let loc = neighbour d l
                 , onBoard loc b
                 ]

countWorkers :: PlayerId -> Board -> Int
countWorkers pid =
  sum . map (tileCountUnits pid) . Map.elems . getField boardMap


enterCityLocs :: PlayerId -> Board -> [(Loc,CityId,UnitType)]
enterCityLocs p b = [ (l,s,u) | (l,t) <- Map.toList (getField boardMap b)
                              , (s,u) <- tileEnterCities p t
                    ]

enterRuinLocs :: PlayerId -> Board -> [(Loc,RuinId,UnitType)]
enterRuinLocs p b = [ (l,s,u) | (l,t) <- Map.toList (getField boardMap b)
                              , (s,u) <- tileEnterRuins p t
                    ]

cloneLocs :: PlayerId -> Board -> [Loc]
cloneLocs playerId board =
  [ l | (l,t) <- Map.toList (getField boardMap board)
      , tileCountUnits playerId t > 0
  ]

moveLocs :: PlayerId -> Int -> Board -> [(Loc,[(Int,Loc)])]
moveLocs playerId movePts board =
  [ (from,opts)
    | (from,fromTile) <- Map.toList (getField boardMap board)
    , tileCanMoveFrom playerId fromTile
    , let opts =
            [ (cost, to)
            | to <- neighbours from board
            , let toTerrain = tileTerrain (getField (tileAt to) board)
                  cost = totalMoveCost (tileTerrain fromTile) toTerrain
            , cost <= movePts
            ]
    , not (null opts)
  ]

blockedUnits :: PlayerId -> Board -> [(Loc,Int)]
blockedUnits playerId board =
  [ (loc,n)
  | (loc,tile) <- Map.toList (getField boardMap board)
  , let n = bagContains LockedUnit (getField (playerUnits playerId) tile)
  , n > 0
  ]

lockedUnits :: PlayerId -> Board -> [(Loc,[CityId],[RuinId])]
lockedUnits playerId board =
  [ (loc, tileUnitsInCities playerId tile, tileUnitsInRuins playerId tile)
  | (loc,tile) <- Map.toList (getField boardMap board)
  ]

flyLocs :: PlayerId -> Board -> [(Loc,[Loc])]
flyLocs playerId board =
  [ (from, neighbours from board)
    | (from,fromTile) <- Map.toList (getField boardMap board)
    , tileCanFlyFrom playerId fromTile
  ]

revealTiles :: Loc -> Board -> [(Loc,Tile)]
revealTiles l board =
  [ (x,setField tileVisible True t)
  | x <- neighbours l board
  , let t = getField (tileAt x) board
  , not (getField tileVisible t)
  ]

--------------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = Board { _boardMap = Map.empty
                   , _boardCapital = Map.empty
                   }

placeTile :: Loc -> Tile -> Board -> Board
placeTile l t = updField boardMap (Map.insert l t)

placeStart :: Maybe PlayerId -> Loc -> Dir -> Maybe Resource -> Board -> Board
placeStart pl l d r = setCap
                    . placeTile (neighbour (rot 1 d) l) b
                    . placeTile (neighbour d l) a
                    . placeTile l (setCapital pl p)
  where
  findStart x = case find ((x ==) . tileNumber) startTiles of
                  Just t  -> setField tileVisible True t
                  Nothing -> error "Missing start tile."
  p = findStart Capital
  a = findStart (TNE r)
  b = findStart (TNW r)

  setCap = case pl of
             Nothing  -> id
             Just pid -> updField boardCapital (Map.insert pid l)


--------------------------------------------------------------------------------

instance ToJSON Board where
  toJSON = toJSON . Map.toList . getField boardMap
