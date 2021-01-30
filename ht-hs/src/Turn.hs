{-# Language TemplateHaskell #-}
module Turn
  ( -- * Basics
    Turn
  , newTurn
  , currentPlayer

    -- * Actions
  , actionsDone
  , currentActionLimit

    -- * Gateways
  , usedGateways
  , useGateway

    -- * The "hand"
  , addWorkerToHand
  , removeWorkerFromHand
  , nextPickedUp
  , turnPlacing
  ) where

import Data.Maybe(listToMaybe)
import Data.Set(Set)
import qualified Data.Set as Set
import GHC.Generics

import qualified Data.Aeson as JS
import Data.Aeson ((.=),ToJSON(..))

import Common.Basics
import Common.Field

import Basics
import Stats
import Bonus

data Turn = Turn
  { turnCurrentPlayer   :: PlayerId
  , _actionsDone        :: Int
  , _currentActionLimit :: Int
  , turnUsedGateways    :: Set ProvinceId
  , _turnPlacing        :: Maybe BonusToken
  , turnPickedUp        :: [(Maybe ProvinceId,Worker)]
  } deriving (Show,Read,Generic)

declareFields ''Turn

newTurn :: PlayerId -> Int -> Turn
newTurn playerId actLvl =
  Turn
    { turnCurrentPlayer   = playerId
    , _actionsDone        = 0
    , _currentActionLimit = actionLimit actLvl
    , turnUsedGateways    = Set.empty
    , _turnPlacing        = Nothing
    , turnPickedUp        = []
    }

currentPlayer :: Turn -> PlayerId
currentPlayer = turnCurrentPlayer

usedGateways :: Turn -> Set ProvinceId
usedGateways = turnUsedGateways

useGateway :: ProvinceId -> Turn -> Turn
useGateway g = \t -> t { turnUsedGateways = Set.insert g (turnUsedGateways t) }

addWorkerToHand :: Maybe ProvinceId -> Worker -> Turn -> Turn
addWorkerToHand prov w = \t -> t { turnPickedUp = (prov,w) : turnPickedUp t }

removeWorkerFromHand :: Turn -> Turn
removeWorkerFromHand = \t -> t { turnPickedUp = init (turnPickedUp t) }

nextPickedUp :: Turn -> Maybe (Maybe ProvinceId,Worker)
nextPickedUp = listToMaybe  . reverse . turnPickedUp



--------------------------------------------------------------------------------

instance ToJSON Turn where
  toJSON t =
    JS.object
      [ "player"   .= currentPlayer t
      , "actDone"  .= getField actionsDone t
      , "actLimit" .= getField currentActionLimit t
      , "pickedUp" .= map snd (turnPickedUp t)
      , "placing"  .= getField turnPlacing t
      ]

