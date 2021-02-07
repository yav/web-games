module AppTypes where

import Data.Map(Map)
import qualified Data.Map as Map
import GHC.Generics

import Data.Aeson(FromJSON,ToJSON(..))
import System.Random.TF(TFGen)

import Common.Basics
import Action
import Tech
import Geometry
import Layout
import PlayerState

data Input = XXXInput
  deriving (Eq,Ord,Show,Generic,FromJSON,ToJSON)

data Update = XXXUpdate
  deriving (Generic,ToJSON)

data State = State
  { _gamePlayers  :: Map PlayerId PlayerState
  , gameTurnOrder :: [PlayerId]
  , gameBoard :: Board
  , test :: [Tech]
  -- map
  -- tech market
  } deriving (Generic,ToJSON)

type Finished = State

doUpdate :: Update -> State -> Either Finished State
doUpdate _ = Right

initialState :: TFGen -> Bool -> [PlayerId] -> State
initialState rng useFog ps = State
  { gameTurnOrder = ps
  , _gamePlayers  = Map.fromList [ (p,emptyPlayerState) | p <- ps ]
  , gameBoard = setupBoard rng useFog (map (const Nothing) ps)
  , test = deck3
  }
