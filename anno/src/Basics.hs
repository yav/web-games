module Basics where

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
  | ShipSpace ShipType
  | Specialist Profession
  | Dock Int
    deriving (Eq,Ord,Show)

type Cost = Bag Resource

data Loc = Loc Int Int
  deriving (Eq,Ord,Show)

data Ship = Ship
  { shipLevel :: Int
  , shipType  :: ShipType
  }

newtype NewWorld = NewWorld [ Resource ]
data OldWorld = OldWorld  -- XXX: bonus

data Building = Building
  { producerCost    :: Maybe Cost
  , producerOutput  :: Resource
  , producerWorker  :: Profession
  , _producerSpots  :: [Maybe Profession]
  }

data PlayerState = PlayerState
  { _playerShips    :: [Ship]
  , _playerShipping :: Bag ShipType
  , _playerOldWorld :: [OldWorld]     -- old world
  , _playerNewWorld :: [NewWorld]     -- how many and which
  }


