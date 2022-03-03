module Game where

import Data.Map(Map)
import qualified Data.Map as Map

import GHC.Generics(Generic)
import Common.Basics(PlayerId)
import Data.Aeson(ToJSON)

import Basics
import Player
import Board


data Game = Game
  { turnOrder   :: [PlayerId]
  , _playerTeam :: Map PlayerId TeamId
  , _teamState  :: Map TeamId Player
  , _board      :: Board
  , _actions    :: Map Action Int  -- remaining actions
  , _nextEvent  :: Int
  } deriving (Generic,ToJSON)

newGame :: [(PlayerId,God)] -> Game
newGame ps = Game
  { turnOrder   = players
  , _playerTeam = teams
  , _teamState  = Map.fromList (map mkP ps)
  , _board      = newBoard
  , _actions    = Map.fromList [ (a,actionLimit pnum a) | a <- allActions ]
  , _nextEvent  = 0
  }
  where
  pnum      = length ps
  players   = map fst ps
  teams     = Map.fromList (zip players (map TeamId [ 0 .. ]))
  mkP (p,g) = (teams Map.! p, newPlayer g)
