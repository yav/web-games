module AppTypes (module AppTypes, Input) where

import Data.Map(Map)
import qualified Data.Map as Map
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
  , _gameRNG      :: RNG
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
  , _gamePlayers  = Map.fromList [ (p,emptyPlayerState) | p <- ps ]
  , gameBoard = brd
  , _gameTurn = newTurn (head ps)
  , _gameRNG = rng1
  }
  where
  (rng1,boardRng) = splitRNG rng
  brd = setupBoard boardRng useFog [ (Just p, Nothing) | p <- ps ]


