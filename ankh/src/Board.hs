module Board where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import GHC.Generics(Generic)
import Data.Aeson(ToJSON(..),(.=))
import qualified Data.Aeson as JS

import Common.Field

import Hex
import LocMap
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
  { terrain     :: LocMap Terrain
  , _regions    :: Map Int (Set Loc)
  , _underworld :: Set Loc
  , _content    :: LocMap BoardItem
  } deriving (Generic)

declareFields ''Board

instance ToJSON Board where
  toJSON b =
    JS.object [ "hexes"   .= lmMapWithKey info (terrain b)
              , "borders" .= es
              ]
    where
    es = allRegionBorders (Map.elems (_regions b))
    info l t = JS.object [ "terrain" .= if l `Set.member` _underworld b
                                           then "Underworld" else toJSON t
                         , "content" .= lmLookup l (_content b)
                         ]


-- XXX
newBoard :: Board
newBoard = Board
  { terrain = lmFromList (mk cols)
  , _regions = rs
  , _underworld = mempty
  , _content = mempty
  }
  where
  mk ds = [ (Loc x y, t)
          | ((x,ts),y0) <- zip (zip [ 0 .. ] ds) start
          , (y,t)       <- zip [ y0 .. ] (concat ts)
          ]


  x .* a = replicate x a
  start = [ 1, 0, -1, -1, -2, -2, -3, -3, -4, -4, -4 ]

  cols = [ [ 2 .* Grass, 6 .* Desert ]
         , [ 3 .* Grass, 6 .* Desert ]
         , [ 4 .* Grass, 1 .* Desert, 2 .* Water,  3 .* Desert ]
         , [ 4 .* Grass, 2 .* Desert, 1 .* Water,  2 .* Desert ]
         , [ 1 .* Water, 4 .* Grass,  1 .* Desert, 1 .* Water,  3 .* Grass ]
         , [ 1 .* Water, 8 .* Grass ]
         , [ 1 .* Water, 6 .* Grass, 3 .* Desert ]
         , [ 1 .* Water, 3 .* Grass, 5 .* Desert ]
         , [ 1 .* Water, 3 .* Grass, 3 .* Desert, 1 .* Water, 2 .* Desert ]
         , [ 1 .* Water, 2 .* Grass, 1 .* Water, 2 .* Desert, 1 .* Water, 2 .* Desert ]
         , [ 1 .* Water, 1 .* Grass, 1 .* Desert, 5 .* Water ]
         ]

  rs = Map.fromListWith Set.union
                  [ (r, Set.singleton l) | (l,r) <- mk sampleRs, True || r /= 0 ]

  sampleRs :: [[[Int]]]
  sampleRs = [ [ 8 .* 1 ]
             , [ 2 .* 3, 7 .* 1 ]
             , [ 3 .* 3, 2 .* 1, 2 .* 0, 3 .* 1 ]
             , [ 3 .* 3, 3 .* 1, 1 .* 0, 2 .* 1 ]
             , [ 1 .* 0, 3 .* 3, 2 .* 1, 1 .* 0, 3 .* 1 ]
             , [ 1 .* 0, 3 .* 3, 2 .* 1, 3 .* 2 ]
             , [ 1 .* 0, 3 .* 3, 6 .* 2 ]
             , [ 1 .* 0, 2 .* 3, 6 .* 2 ]
             , [ 1 .* 0, 2 .* 3, 4 .* 2, 1 .* 0, 2 .* 2 ]
             , [ 1 .* 0, 1 .* 3, 1 .* 2, 1 .* 0, 2 .* 2, 1 .* 0, 2 .* 2 ]
             , [ 1 .* 0, 2 .* 2, 5 .* 0 ]
             ]
