module AppTypes (module AppTypes, Input) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(mapAccumL)
import GHC.Generics

import Data.Aeson(ToJSON)

import Common.Basics
import Common.Field
import Common.RNG

import Geometry
import Layout
import PlayerState
import Turn


data Update = XXXUpdate
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

doUpdate :: Update -> State -> Either Finished State
doUpdate _ = Right

playerView :: PlayerId -> State -> View
playerView _ = id -- XXX: hide other player's ruin tokenundefineds

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

