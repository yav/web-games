module AppTypes where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Aeson(FromJSON,ToJSON(..))
import GHC.Generics

import Common.Basics
import PlayerState

data Input = XXXInput
  deriving (Eq,Ord,Show,Generic,FromJSON,ToJSON)

data Update = XXXUpdate
  deriving (Generic,ToJSON)

data State = State
  { _gamePlayers  :: Map PlayerId PlayerState
  , gameTurnOrder :: [PlayerId]
  -- map
  -- tech market
  } deriving (Generic,ToJSON)

type Finished = State

doUpdate :: Update -> State -> Either Finished State
doUpdate _ = Right

initialState :: [PlayerId] -> State
initialState ps = State
  { gameTurnOrder = ps
  , _gamePlayers  = Map.fromList [ (p,emptyPlayerState) | p <- ps ]
  }
