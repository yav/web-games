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
import Layout
import PlayerState
import Turn
import Action


data Update =
    PlaceCube PlayerId CubeLoc Resource
  | RemoveCube PlayerId CubeLoc
  | SetTurn Turn

  | RemoveFromReady   PlayerId Resource
  | AddToReady        PlayerId Resource
  | RemoveFromBag     PlayerId Resource
  | AddToBag          PlayerId Resource
  | AddToDiscard      PlayerId Resource
  | RemoveFromDiscard PlayerId Resource
  deriving (Generic,ToJSON)


data State = State
  { _gamePlayers  :: Map PlayerId PlayerState
  , _gameTurn     :: Turn
  , gameTurnOrder :: [PlayerId]
  , gameBoard     :: Board
  -- map
  -- tech market

  } deriving (Generic,ToJSON)

declareFields ''State

type Finished = State
type View = State


initialState :: RNG -> Bool -> [PlayerId] -> State
initialState rng useFog ps = State
  { gameTurnOrder = ps
  , _gamePlayers  = Map.fromList pstates
  , gameBoard = brd
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

    RemoveFromReady playerId r ->
      Right . updField (playerState playerId .> playerAvailable)
                       (bagRemove r)
    AddToReady playerId r ->
      Right . updField (playerState playerId .> playerAvailable)
                       (bagAdd r)

    RemoveFromBag playerId r ->
      Right . updField (playerState playerId .> playerBag)
                       (bagRemove r)
    AddToBag playerId r ->
      Right . updField (playerState playerId .> playerBag)
                       (bagAdd r)


    RemoveFromDiscard playerId r ->
      Right . updField (playerState playerId .> playerDiscarded)
                       (bagRemove r)
    AddToDiscard playerId r ->
      Right . updField (playerState playerId .> playerDiscarded)
                       (bagAdd r)

    SetTurn t ->
      Right . setField gameTurn t



