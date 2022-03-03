module Player where

import Data.Set(Set)
import qualified Data.Set as Set
import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.Field

import Basics

data Player = Player
  { _pGod         :: Set God
  , _pCardsInHand :: Set Card
  , _pCardsPlayed :: Set Card
  , _pPoints      :: Float  -- fractional points to resolve ties
  , _pFollowers   :: Int
  , _pWarriors    :: Int
  , _pGuardians   :: [Guardian]
  , _pUpgrades    :: Set Upgrade
  , _pActions     :: Int

  , _pRadiant     :: Int
  , _pCaptured    :: [TeamId]
  , _pUnderworld  :: Int
  , _p2Cards      :: Bool
  , _pTiebreaker  :: Bool
  } deriving (Generic,ToJSON)

newPlayer :: God -> Player
newPlayer g = Player
  { _pGod         = Set.singleton g
  , _pCardsInHand = Set.fromList allCards
  , _pCardsPlayed = mempty
  , _pFollowers   = 1
  , _pPoints      = 0
  , _pWarriors    = 6
  , _pGuardians   = []
  , _pUpgrades    = mempty
  , _pActions     = 2
  , _pRadiant     = case g of
                      Ra -> 3
                      _  -> 0
  , _pCaptured    = []
  , _pUnderworld  = case g of
                      Anubis -> 3
                      _      -> 0
  , _p2Cards      = case g of
                      Osiris -> True
                      _      -> False
  , _pTiebreaker  = False
  }

declareFields ''Player

