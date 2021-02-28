module AppTypes (module AppTypes, Input) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(mapAccumL)
import GHC.Generics

import Data.Aeson(ToJSON)

import Common.Basics
import Common.Field
import Common.RNG

import Bag
import Resource
import Geometry
import Tile
import Layout
import PlayerState
import Turn


data Update =
    PlaceCube PlayerId CubeLoc Resource
  | RemoveCube PlayerId CubeLoc
  | SetTurn Turn

  | ChangeBag PlayerId BagName Resource Int
  | ChangeGems PlayerId Int

  | Upgrade           PlayerId Resource Int
  | ResetUpgrade      PlayerId Resource

  | ChangeUnit PlayerId UnitType Loc Int

  | SetCity Loc CityId TileSpot
  | SetRuin Loc RuinId TileSpot


  deriving (Generic,ToJSON)



data State = State
  { _gamePlayers  :: Map PlayerId PlayerState
  , _gameTurn     :: Turn
  , gameTurnOrder :: [PlayerId]
  , _gameEndOn    :: Maybe PlayerId
  , _gameBoard     :: Board
  -- map
  -- tech market

  } deriving (Generic,ToJSON)

declareFields ''State

type Finished = State
type View = State


initialState :: RNG -> Bool -> [PlayerId] -> State
initialState rng useFog ps = State
  { gameTurnOrder = ps
  , _gameEndOn = Nothing
  , _gamePlayers  = Map.fromList pstates
  , _gameBoard = brd
  , _gameTurn = newTurn (head ps)
  }
  where
  mkP r p = let (r1,r2) = splitRNG r
            in (r1, (p, emptyPlayerState r2))
  (boardRng,pstates) = mapAccumL mkP rng ps
  brd = setupBoard boardRng useFog [ (Just p, Nothing) | p <- ps ]

playerState :: PlayerId -> Field State PlayerState
playerState p = gamePlayers .> mapAt p


currentPlayer :: State -> (PlayerId,PlayerState)
currentPlayer state = (playerId,player)
  where
  playerId = turnPlayer (getField gameTurn state)
  player   = getField (playerState playerId) state

--------------------------------------------------------------------------------

playerView :: PlayerId -> State -> View
playerView _ = id -- XXX: hide other player's ruin tokenundefineds


doUpdate :: Update -> State -> Either Finished State
doUpdate upd =
  case upd of
    PlaceCube playerId loc r ->
      Right . setField (playerState playerId .> costSpot loc .> spotResource)
                       (Just r)

    RemoveCube playerId loc ->
      Right . setField (playerState playerId .> costSpot loc .> spotResource)
                       Nothing

    ChangeBag playerId nm r n ->
      Right . updField (playerState playerId .> playerBag .> mapAt nm)
                       (bagChange n r)

    ChangeGems playerId n ->
      Right . updField (playerState playerId .> playerGems) (+n)

    SetTurn t ->
      Right . setField gameTurn t

    Upgrade playerId r n ->
      Right . updField (playerState playerId .> playerDevel .> mapAt r) (+n)

    ResetUpgrade playerId r ->
      Right . setField (playerState playerId .> playerDevel .> mapAt r) 0

    ChangeUnit playerId ty loc n ->
      Right . updField (gameBoard .> tileAt loc .> playerUnits playerId)
                       (bagChange n ty)


    SetCity loc cityId val ->
      Right . setField (gameBoard .> tileAt loc
                                  .> tileCities .> mapAt cityId .> citySpot)
                       val

    SetRuin loc ruinId val ->
      Right . setField (gameBoard .> tileAt loc
                                  .> tileRuins .> mapAt ruinId .> ruinSpot)
                       val

