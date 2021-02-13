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

type TechId = Int

data PlayerState = PlayerState
  { _playerBag       :: Bag
  , _playerAvailable :: Bag
  , _playerDiscarded :: Bag
  , _playerGems      :: Int
  , _playerDevel     :: Map Resource Int
  , _playerBoard     :: Map GroupName [BoardAction]
  , _playerTech      :: Map TechId Tech
  } deriving (Generic,ToJSON)

data CubeLoc = OnBoard Resource Int Int -- ^ group, row, spot
             | OnTech TechId Int -- ^ tech, spot

declareFields ''PlayerState

costSpot :: CubeLoc -> Field PlayerState ResourceSpot
costSpot loc =
  case loc of
    OnBoard r n i -> playerBoard .> mapAt r .> listAt n .> baCost .> listAt i
    OnTech t i    -> playerTech  .> mapAt t .> techCost .> listAt i



emptyPlayerState :: PlayerState
emptyPlayerState =
  setField (costSpot (OnBoard Red 0 1) .> spotResource) (Just Green) $
  setField (costSpot (OnBoard Purple 0 0) .> spotResource) (Just Purple)
   PlayerState
  { _playerBag       = bagAdd Orange
                     $ bagFromList [ r | r <- enumAll, r /= Gray ]
  , _playerAvailable = bagEmpty
  , _playerDiscarded = bagEmpty
  , _playerGems      = 0
  , _playerDevel     = Map.fromList [ (r,0) | r <- enumAll, r /= Gray ]
  , _playerBoard     = emptyPlayerBoard
  , _playerTech      = Map.empty
  }


playerGainTech :: Tech -> PlayerState -> PlayerState
playerGainTech t = updField playerTech add
  where add mp = Map.insert (Map.size mp) t mp

