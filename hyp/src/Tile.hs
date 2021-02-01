module Tile where

import Data.Map(Map)
import qualified Data.Map as Map

import Common.Basics
import Common.Field

import Terrain
import Action
import RuinToken
import Resource


data TileName =
    TileNum Int
  | Capital
  | TNE (Maybe Resource)
  | TNW (Maybe Resource)
    deriving (Eq,Ord)


data Tile = Tile
  { tileNumber    :: TileName
  , tileTerrain   :: Terrain
  , _tileCities   :: Map Int City
  , _tileRuins    :: Map Int Ruin
  , _tileVisible  :: Bool
  }


data TileSpot =
    Empty
  | Ghost
  | Player PlayerId

data City = City
  { citySpot    :: TileSpot
  , cityActions :: Action
  }

data Ruin = Ruin
  { ruinSpot   :: TileSpot
  , ruinType   :: TokenType
  , ruinTokens :: [Token]
  }


declareFields ''Tile

defTile' :: TileName -> Terrain -> [City] -> [Ruin] -> Tile
defTile' name ter cs rs = Tile
  { tileNumber    = name
  , tileTerrain   = ter
  , _tileCities   = Map.fromList (zip [ 0 .. ] cs)
  , _tileRuins    = Map.fromList (zip [ 0 .. ] rs)
  , _tileVisible  = False
  }

defTile :: Int -> Terrain -> [City] -> [Ruin] -> Tile
defTile n = defTile' (TileNum n)

defCity :: Action -> City
defCity as = City { citySpot = Empty, cityActions = as }

defRuin :: TokenType -> Ruin
defRuin t = Ruin { ruinSpot = Empty, ruinType = t, ruinTokens = [] }


centralTiles :: [Tile]
centralTiles =
  [ defTile 31 Forest
      [ defCity $ Action [ DrawResource 1, Gem 1 ]
      , defCity $ Action [ Develop Same 2, Gem 1 ]
      ]
      []

  , defTile 32 Plains
      [ defCity $ If (LooseResource AnyNormal 1) [ Gem 2, Develop Same 2 ] ]
      [ defRuin Gold ]

  , defTile 33 Swamp
      [ defCity $ Action [ DrawResource 1, Develop Same 4 ] ]
      []

  , defTile 34 Mountain
      [ defCity $ Action [ Gem 1, Develop Same 3 ] ]
      [ defRuin Gold ]

  , defTile 35 Mountain
      [ defCity $ If (RemoveWorker 1) [ Gem 3 ] ]
      []

  , defTile 36 Forest
      [ defCity $ Action [ Gem 2 ] ]
      [ defRuin Gold, defRuin Gold ]
  ]


peripheralTiles :: [Tile]
peripheralTiles =
  [ defTile 1 Plains
      [ defCity $ Action [ Gem 1 ] ]
      []

  , defTile 2 Plains
      [ defCity $ Action [ Develop Same 2 ] ]
      []

  , defTile 3 Plains
      [ defCity $ Action [ DrawResource 1, Develop Same 2 ] ]
      []

  , defTile 4 Plains
      [ defCity $ Action [ Develop Same 3 ] ]
      []

  , defTile 5 Plains
      [ defCity $ Action [ Gem 1 ] ]
      []

  , defTile 6 Plains
      [ defCity $ Action [ DrawResource 1, Develop Same 2 ] ]
      []

  , defTile 7 Plains
      []
      [ defRuin Silver ]

  , defTile 8 Plains
      []
      [ defRuin Silver ]

  , defTile 9 Plains
      []
      [ defRuin Silver ]

  , defTile 10 Plains
      []
      [ defRuin Silver ]


  , defTile 11 Forest
      [ defCity $ Action [ Gem 1 ] ]
      []

  , defTile 12 Forest
      [ defCity $ Action [ DrawResource 1, Develop Same 2 ] ]
      []

  , defTile 13 Forest
      [ defCity $ Action [ Develop Same 3 ] ]
      []

  , defTile 14 Forest
      [ defCity $ Action [ Develop Same 2 ] ]
      [ defRuin Silver ]

  , defTile 15 Plains
      [ ]
      [ defRuin Silver ]

  , defTile 16 Forest
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , defTile 17 Swamp
      [ ]
      [ defRuin Silver ]

  , defTile 18 Swamp
      [ ]
      [ defRuin Silver ]

  , defTile 19 Mountain
      [ defCity $ Action [ Gem 1 ] ]
      []

  , defTile 20 Mountain
      [ ]
      [ defRuin Silver ]

  , defTile 21 Plains
      [ ]
      [ defRuin Silver ]

  , defTile 22 Mountain
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , defTile 23 Plains
      [ defCity $ Action [ Develop Same 4  ] ]
      []

  , defTile 24 Plains
      [ defCity $ Action [ Move 2 ] ]
      []

  , defTile 25 Plains
      [ defCity $ Action [ DrawResource 1, Move 1 ] ]
      []

  , defTile 26 Forest
      [ defCity $ Action [ Gem 1 ] ]
      []

  , defTile 27 Forest
      [ defCity $ Action [ Develop Same 3 ] ]
      [ defRuin Silver ]

  , defTile 28 Plains
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , defTile 29 Swamp
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , defTile 30 Mountain
      [ defCity $ Action [ Fly 1 ] ]
      [ ]
  ]


startTiles :: [Tile]
startTiles =
  [ defTile' Capital Plains
      [ defCity $ Action [ Move 1, Develop Any 1 ] ]
      []

  , defTile' (TNW Nothing) Plains
      []
      [ defRuin Bronze ]

  , defTile' (TNE Nothing ) Plains
      [ defCity $ Action [ Move 1 ] `Or` Action [ Develop Any 1 ] ]
      []


  , defTile' (TNW (Just Blue)) Swamp
      []
      [ defRuin Bronze ]

  , defTile' (TNE (Just Blue)) Plains
      [ defCity $ Action [ Move 1 ] `Or` Action [ DrawResource  1] ]
      []


  , defTile' (TNW (Just Purple)) Plains
      [ defCity $ Action [ Develop Any 1 ] ]
      []

  , defTile' (TNE (Just Purple)) Plains
      [ defCity $ Action [ Move 1 ] ]
      []


  , defTile' (TNW (Just Red)) Plains
      []
      [ defRuin Bronze ]

  , defTile' (TNE (Just Red)) Plains
      [ defCity $ Action [ Move 1 ] `Or` Action [ Attack 1 ] ]
      []


  , defTile' (TNW (Just Orange)) Mountain
      []
      [ defRuin Bronze, defRuin Bronze ]

  , defTile' (TNE (Just Orange)) Plains
      [ defCity $ Action [ Move 1 ] `Or` Action [ Develop Same 2 ] ]
      []


  , defTile' (TNW (Just Yellow)) Plains
      [ defCity $ Action [ Move 1 ] `Or` Action [ Develop Any 1 ] ]
      []

  , defTile' (TNE (Just Yellow)) Plains
      []
      [ defRuin Bronze ]


  , defTile' (TNW (Just Green)) Plains
      []
      [ defRuin Bronze ]

  , defTile' (TNE (Just Green)) Plains
      [ defCity $ Action [Move 2] `Or` Action [Develop Any 1 ] ]
      []
  ]

