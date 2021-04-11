module Geometry where

import GHC.Generics(Generic)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(find)
import Control.Monad(guard)

import Data.Aeson(ToJSON(..),FromJSON)

import Common.Basics(PlayerId)
import Common.Utils(enumAll)
import Common.Field

import Resource
import Terrain
import Tile
import Bag

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

--------------------------------------------------------------------------------
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

-- | also used for firtify locs
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
  , let n = tileCountBlocked playerId tile
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

neighbourPlayers :: PlayerId -> Board -> Set PlayerId
neighbourPlayers playerId board =
  Set.unions
    [ playersOf l
    | (loc,tile) <- Map.toList (getField boardMap board)
    , tileCountUnits playerId tile > 0
    , l <- loc : neighbours loc board
    ]
  where 
  playersOf loc = tilePresentOpponents playerId (getField (tileAt loc) board)


attackTargets ::
  PlayerId -> Board -> [(Loc,[PlayerId],[CityId],[RuinId])]
attackTargets playerId board =
  [ ans
  | (loc,tile) <- Map.toList (getField boardMap board)
  , tileHasOutsideUnits playerId tile
  , ans <- tileTargets playerId loc tile
  ]


rangedAttackTargets ::
  PlayerId -> Board -> [(Loc,[PlayerId],[CityId],[RuinId])]
rangedAttackTargets playerId board =
  [ ans
  | (loc,tile) <- Map.toList (getField boardMap board)
  , tileHasOutsideUnits playerId tile
  , ans <- tileTargets playerId loc tile ++
           [ a | n <- neighbours loc board
               , a <- tileTargets playerId n (getField (tileAt n) board)
           ]
  ]

tileTargets :: PlayerId -> Loc -> Tile -> [(Loc,[PlayerId],[CityId],[RuinId])]
tileTargets playerId loc tile =
  do let out    = tileOutsideOpponents playerId tile
         cities = tileOpponentsInCities playerId tile ++
                  tileGhostInCities tile
         ruins  = tileOpponentsInRuins playerId tile ++
                  tileGhostInRuins tile
     guard (not (null out && null cities && null ruins))
     pure (loc,out,cities,ruins)

tilesWithFortifications :: PlayerId -> Board -> [(Loc,Int)]
tilesWithFortifications playerId board =
  [ (loc,n)
  | (loc,tile) <- Map.toList (getField boardMap board)
  , let us = getField (playerUnits playerId) tile
        n  = bagContains Fortification us
  , n > 0
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
