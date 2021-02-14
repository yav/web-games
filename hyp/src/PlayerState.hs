module PlayerState where

import Data.Map(Map)
import qualified Data.Map as Map
import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.Utils(enumAll)
import Common.Field

import Resource
import Action
import Bag
import BoardActions

type TechId = Int

data PlayerState = PlayerState
  { _playerBag       :: Bag Resource
  , _playerAvailable :: Bag Resource
  , _playerDiscarded :: Bag Resource
  , _playerGems      :: Int
  , _playerDevel     :: Map Resource Int
  , _playerTech      :: Map TechId Tech
  } deriving (Generic,ToJSON)

data CubeLoc = CubeLoc TechId Int Int -- ^ group, row, spot

declareFields ''PlayerState

costSpot :: CubeLoc -> Field PlayerState ResourceSpot
costSpot (CubeLoc t r i) =
  playerTech  .> mapAt t .> techAlts .> listAt r .> techCost .> listAt i



emptyPlayerState :: PlayerState
emptyPlayerState =
  setField (costSpot (CubeLoc 0 0 1) .> spotResource) (Just Green) $
  setField (costSpot (CubeLoc 1 0 0) .> spotResource) (Just Purple) $
   flip (foldr playerGainTech) emptyPlayerBoard
   PlayerState
  { _playerBag       = bagAdd Orange
                     $ bagFromList [ r | r <- enumAll, r /= Gray ]
  , _playerAvailable = bagEmpty
  , _playerDiscarded = bagEmpty
  , _playerGems      = 0
  , _playerDevel     = Map.fromList [ (r,0) | r <- enumAll, r /= Gray ]
  , _playerTech      = Map.empty
  }


playerGainTech :: Tech -> PlayerState -> PlayerState
playerGainTech t = updField playerTech add
  where add mp = Map.insert (Map.size mp) t mp

