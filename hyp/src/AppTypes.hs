module AppTypes (module AppTypes, Input) where

import Data.Map(Map)
import qualified Data.Map as Map
import GHC.Generics

import Data.Aeson(ToJSON(..))
import System.Random.TF(TFGen)

import Common.Basics
import Action
import Tech
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
  , gameBoard :: Board
  , test :: [[Tech]]
  -- map
  -- tech market
  } deriving (Generic,ToJSON)

type Finished = State
type View = State

doUpdate :: Update -> State -> Either Finished State
doUpdate _ = Right

playerView :: PlayerId -> State -> View
playerView _ = id -- XXX: hide other player's ruin tokenundefineds

initialState :: TFGen -> Bool -> [PlayerId] -> State
initialState rng useFog ps = State
  { gameTurnOrder = ps
  , _gamePlayers  = Map.fromList [ (p,emptyPlayerState) | p <- ps ]
  , gameBoard = brd
  , test = [deck1,deck2,deck3,deck4]
  , _gameTurn = newTurn (head ps)
  }
  where
  brd = setupBoard rng useFog [ (Just p, Nothing) | p <- ps ]
