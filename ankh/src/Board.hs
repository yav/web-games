module Board where

import Data.Map(Map)
import Data.Set(Set)
import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Common.Field

import Hex
import Basics

data Terrain = Grass | Desert | Water
  deriving (Generic,ToJSON,Eq)

data BoardItem =
    Monument (Maybe TeamId) Monument
  | God TeamId
  | Warrior TeamId [Attr]
  | Guardian TeamId Guardian [Attr]
    deriving (Generic,ToJSON)

data Attr = Radiant | Facing Dir Dir
    deriving (Generic,ToJSON)


data Board = Board
  { terrain     :: Map Loc Terrain
  , _regions    :: Map Int (Set Loc)
  , _underworld :: Set Loc
  , _content    :: Map Loc BoardItem
  } deriving (Generic,ToJSON)

declareFields ''Board

-- XXX
newBoard :: Board
newBoard = Board
  { terrain = mempty
  , _regions = mempty
  , _underworld = mempty
  , _content = mempty
  }
