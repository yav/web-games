module Basics where

import Data.Map(Map)

import Common.Basics
import Common.Bag

data Profession = Farmer | Worker | Craftsman | Engineer | Investor
  deriving (Eq,Ord,Show)

data ShipType = Exploration | Trade
  deriving (Eq,Ord,Show)

data Resource   =
    Grain | Potatoes | Pigs | CoffeeBeans | SugarCange | Tobbaco | Cocoa
  | Bread | Sausage | CannedFood
  | Coffee | Beer | Schnapps | Rum | Chamapgne
  | Cigars | Chocolate
  | Cotton | Wool | CottonFabric | SawingMachine
  | Sails | WorkClothes | FurCoat
  | Caoutchouc | Timber | Coal | Brick | Steel
  | Glass | Brass
  | Soap | Windows | Goods | Glasses | Watch | Bulb | Gramophone
  | Dynamite | Gun | Artillery
  | SteamEngine | Bicycle | SteamCarriege
  | ShipToken ShipType
  | Specialist Profession

  -- Just for buildings
  | DockBuilding Int | ShipBuilding Ship
    deriving (Eq,Ord,Show)

type Cost = Bag Resource

data Loc = Loc Int Int
  deriving (Eq,Ord,Show)

data Ship = Ship
  { shipLevel :: Int
  , shipType  :: ShipType
  } deriving (Eq,Ord,Show)

newtype NewWorld = NewWorld [ Resource ]
data OldWorld = OldWorld  -- XXX: bonus
data Expedition = Expedition
data Goal = Goal
data Extra = Extra

--------------------------------------------------------------------------------
data Card = Card
  { cardCost :: Cost
  , cardBenefit :: Benefit
  }

data Benefit = Gain Int Resource
             | Upgrade3 [Profession]
             | Discard2
             | GainExpedition2
             | GainAction


--------------------------------------------------------------------------------
data Building = Building
  { cost        :: Maybe Cost
  , output      :: Resource
  , profession  :: Maybe Profession
  , _spots      :: [Maybe Profession]
  }

startBuilding :: Profession -> Resource -> Building
startBuilding prof out = Building
  { cost        = Nothing
  , output      = out
  , profession  = Just prof
  , _spots      = [ Nothing, Nothing ]
  }

building :: Profession -> [Resource] -> Resource -> Building
building prof co out = Building
  { cost = Just (bagFromList co)
  , output = out
  , profession = Just prof
  , _spots = [ Nothing, Nothing ]
  }

dock :: Int -> [Resource] -> Building
dock lvl co = Building
  { cost = Just (bagFromList co)
  , output = DockBuilding lvl
  , profession = Nothing
  , _spots = []
  }

ship :: [Resource] -> Int -> ShipType -> Building
ship co lvl ty = Building
  { cost = Just (bagFromList co)
  , output = ShipBuilding Ship { shipLevel = lvl, shipType = ty }
  , profession = Nothing
  , _spots = []
  }
--------------------------------------------------------------------------------


data PlayerState = PlayerState
  { _playerShips      :: [Ship]
  , _playerShipping   :: Bag ShipType   -- available resource tokens
  , _playerOldWorld   :: [OldWorld]     -- old world
  , _playerNewWorld   :: [NewWorld]     -- how many and which
  , _playerProduction :: Map Loc Building
  , _playerWorkers    :: Bag Profession
  , _playerGold       :: Int
  , _playerHand       :: [Card]
  , _playerCompleted  :: [Card]
  , _playerExpedition :: [Expedition]
  }

data Game = Game
  { _turnOrder      :: [PlayerId]
  , _players        :: Map PlayerId PlayerState
  , goals           :: [Goal]
  , freeAtions      :: [Extra]
  , _deck3          :: [Card]
  , _deck5          :: [Card]
  , _deck8          :: [Card]
  , _deckExpedition :: [Expedition]
  , _market         :: [(Building,Int)]
  , _supply         :: Bag Profession
  }
