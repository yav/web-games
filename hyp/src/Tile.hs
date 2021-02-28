module Tile where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import GHC.Generics(Generic)

import qualified Data.Aeson as JS
import Data.Aeson (ToJSON(..), FromJSON, (.=), ToJSONKey,
                                 genericToJSONKey, defaultJSONKeyOptions)

import Common.Basics
import Common.Field

import Terrain
import Action
import RuinToken
import Resource
import Bag


data TileName =
    TileNum Int
  | Capital
  | TNE (Maybe Resource)
  | TNW (Maybe Resource)
    deriving (Eq,Ord)

type CityId = Int
type RuinId = Int

data Tile = Tile
  { tileNumber    :: TileName
  , tileTerrain   :: Terrain
  , _tileCities   :: Map CityId City
  , _tileRuins    :: Map RuinId Ruin
  , _tileVisible  :: Bool
  , tilePlayers   :: Map PlayerId (Bag UnitType)
  }

data UnitType = FreeUnit | LockedUnit | Frotification
  deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance ToJSONKey UnitType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

data TileSpot =
    Empty
  | Ghost
  | Occupied PlayerId
    deriving (Generic,ToJSON)

data City = City
  { _citySpot    :: TileSpot
  , cityActions :: Action
  , cityCapital :: Maybe PlayerId
  } deriving (Generic, ToJSON)

data Ruin = Ruin
  { _ruinSpot   :: TileSpot
  , ruinType   :: TokenType
  , ruinTokens :: [Token]
  }
declareFields ''Tile
declareFields ''City
declareFields ''Ruin


playerUnits :: PlayerId -> Field Tile (Bag UnitType)
playerUnits p = Field
  { getField = Map.findWithDefault bagEmpty p . tilePlayers
  , setField = \b t -> t { tilePlayers =
                            if bagIsEmpty b
                              then Map.delete p (tilePlayers t)
                              else Map.insert p b (tilePlayers t)
                         }
  }


setCapital :: Maybe PlayerId -> Tile -> Tile
setCapital p = updField tileCities (fmap citySetCapital)
  where
  citySetCapital c = c { cityCapital = p }

isStartTile :: Tile -> Bool
isStartTile t =
  case tileNumber t of
    TileNum _ -> False
    _         -> True




instance ToJSON Tile where
  toJSON t = JS.object (if getField tileVisible t then vis else hidden)
    where
    hidden = [ "tileTerrain" .= ("Fog" :: Text) ]
    vis = [ "tileTerrain" .= tileTerrain t
          , "tileCities" .= getField tileCities t
          , "tileRuins" .= getField tileRuins t
          , "tilePlayers" .= tilePlayers t
          ]

instance ToJSON Ruin where
  toJSON r = JS.object
               [ "ruinSpot" .= getField ruinSpot r
               , "ruinTokens" .= length (ruinTokens r)
               ]


--------------------------------------------------------------------------------
defTile' :: TileName -> Terrain -> [City] -> [Ruin] -> Tile
defTile' name ter cs rs = Tile
  { tileNumber    = name
  , tileTerrain   = ter
  , _tileCities   = Map.fromList (zip [ 0 .. ] cs)
  , _tileRuins    = Map.fromList (zip [ 0 .. ] rs)
  , _tileVisible  = False
  , tilePlayers   = Map.empty
  }

defTile :: Int -> Terrain -> [City] -> [Ruin] -> Tile
defTile n = defTile' (TileNum n)

defCity :: Action -> City
defCity as = City { _citySpot   = Empty
                  , cityActions = as
                  , cityCapital = Nothing
                  }

defRuin :: TokenType -> Ruin
defRuin t = Ruin { _ruinSpot = Empty, ruinType = t, ruinTokens = [] }


centralTiles :: [Tile]
centralTiles =
  [ defTile 31 Forest
      [ defCity $ Action [ DrawResource, Gem ]
      , defCity $ Action [ Develop (Same 2), Gem ]
      ]
      []

  , defTile 32 Plains
      [ defCity $ If (LooseResource AnyNormal)
                                        [ Gem `Times` 2, Develop (Same 2) ] ]
      [ defRuin Gold ]

  , defTile 33 Swamp
      [ defCity $ Action [ DrawResource, Develop (Same 4) ] ]
      []

  , defTile 34 Mountain
      [ defCity $ Action [ Gem, Develop (Same 3) ] ]
      [ defRuin Gold ]

  , defTile 35 Mountain
      [ defCity $ If RemoveWorker [ Gem `Times` 3 ] ]
      []

  , defTile 36 Forest
      [ defCity $ Action [ Gem `Times` 2 ] ]
      [ defRuin Gold, defRuin Gold ]
  ]


peripheralTiles :: [Tile]
peripheralTiles =
  [ defTile 1 Plains
      [ defCity $ Action [ Gem ] ]
      []

  , defTile 2 Plains
      [ defCity $ Action [ Develop (Same 2) ] ]
      []

  , defTile 3 Plains
      [ defCity $ Action [ DrawResource, Develop (Same 2) ] ]
      []

  , defTile 4 Plains
      [ defCity $ Action [ Develop (Same 3) ] ]
      []

  , defTile 5 Plains
      [ defCity $ Action [ Gem ] ]
      []

  , defTile 6 Plains
      [ defCity $ Action [ DrawResource, Develop (Same 2) ] ]
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
      [ defCity $ Action [ Gem ] ]
      []

  , defTile 12 Forest
      [ defCity $ Action [ DrawResource, Develop (Same 2) ] ]
      []

  , defTile 13 Forest
      [ defCity $ Action [ Develop (Same 3) ] ]
      []

  , defTile 14 Forest
      [ defCity $ Action [ Develop (Same 2) ] ]
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
      [ defCity $ Action [ Gem ] ]
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
      [ defCity $ Action [ Develop (Same 4)  ] ]
      []

  , defTile 24 Plains
      [ defCity $ Action [ Move `Times` 2 ] ]
      []

  , defTile 25 Plains
      [ defCity $ Action [ DrawResource, Move ] ]
      []

  , defTile 26 Forest
      [ defCity $ Action [ Gem ] ]
      []

  , defTile 27 Forest
      [ defCity $ Action [ Develop (Same 3) ] ]
      [ defRuin Silver ]

  , defTile 28 Plains
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , defTile 29 Swamp
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , defTile 30 Mountain
      [ defCity $ Action [ Fly ] ]
      [ ]
  ]


startTiles :: [Tile]
startTiles =
  [ defTile' Capital Plains
      [ defCity $ Action [ Move, Develop Any ] ]
      []

  , defTile' (TNW Nothing) Plains
      []
      [ defRuin Bronze ]

  , defTile' (TNE Nothing ) Plains
      [ defCity $ Move `Or` Develop Any ]
      []


  , defTile' (TNW (Just Blue)) Swamp
      []
      [ defRuin Bronze ]

  , defTile' (TNE (Just Blue)) Plains
      [ defCity $ Move `Or` DrawResource ]
      []


  , defTile' (TNW (Just Purple)) Plains
      [ defCity $ Action [ Develop Any ] ]
      []

  , defTile' (TNE (Just Purple)) Plains
      [ defCity $ Action [ Move ] ]
      []


  , defTile' (TNW (Just Red)) Plains
      []
      [ defRuin Bronze ]

  , defTile' (TNE (Just Red)) Plains
      [ defCity $ Move `Or` Attack ]
      []


  , defTile' (TNW (Just Orange)) Mountain
      []
      [ defRuin Bronze, defRuin Bronze ]

  , defTile' (TNE (Just Orange)) Plains
      [ defCity $ Move `Or` Develop (Same 2) ]
      []


  , defTile' (TNW (Just Yellow)) Plains
      [ defCity $ Move `Or` Develop Any ]
      []

  , defTile' (TNE (Just Yellow)) Plains
      []
      [ defRuin Bronze ]


  , defTile' (TNW (Just Green)) Plains
      []
      [ defRuin Bronze ]

  , defTile' (TNE (Just Green)) Plains
      [ defCity $ (Move `Times` 2) `Or` Develop Any ]
      []
  ]

