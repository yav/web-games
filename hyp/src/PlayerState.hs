module PlayerState where

import Data.Map(Map)
import qualified Data.Map as Map
import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.Utils(enumAll)
import Common.Field

import Resource
import Bag
import BoardActions
import Tech


data PlayerState = PlayerState
  { _playerBag       :: Bag
  , _playerAvailable :: Bag
  , _playerDiscarded :: Bag
  , _playerGems      :: Int
  , _playerDevel     :: Map Resource Int
  , _playerBoard     :: Map GroupName [BoardAction]
  , _playerTech      :: Map Int Tech
  } deriving (Generic,ToJSON)

declareFields ''PlayerState

emptyPlayerState :: PlayerState
emptyPlayerState = PlayerState
  { _playerBag       = bagEmpty
  , _playerAvailable = bagEmpty
  , _playerDiscarded = bagEmpty
  , _playerGems      = 0
  , _playerDevel     = Map.fromList [ (r,0) | r <- enumAll ]
  , _playerBoard     = emptyPlayerBoard
  , _playerTech      = Map.empty
  }


playerGainTech :: Tech -> PlayerState -> PlayerState
playerGainTech t = updField playerTech add
  where add mp = Map.insert (Map.size mp) t mp

