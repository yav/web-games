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
  | TNE (Maybe Resource)  -- not really resources, just identifies start tiles
  | TNW (Maybe Resource)  -- not really resources, just identifies start tiles
    deriving (Eq,Ord)

data TileType = TileStartgin | TilePeripheral | TileCenter

type CityId = Int
type RuinId = Int

data Tile = Tile
  { tileNumber    :: TileName
  , tileType      :: TileType
  , tileTerrain   :: Terrain
  , _tileCities   :: Map CityId City
  , _tileRuins    :: Map RuinId Ruin
  , _tileVisible  :: Bool
  , tilePlayers   :: Map PlayerId PlayerUnits
  }

data PlayerUnits = PlayerUnits
  { _pUnits     :: Bag UnitType
  , _pHighlight :: Bool
  } deriving (Generic,ToJSON)

noUnits :: PlayerUnits
noUnits = PlayerUnits
  { _pUnits = bagEmpty
  , _pHighlight = False
  }

data UnitType = FreeUnit | LockedUnit | Fortification
  deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)

instance ToJSONKey UnitType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

data TileSpot =
    Empty
  | Ghost
  | Occupied PlayerId
    deriving (Generic,ToJSON)

data City = City
  { _citySpot   :: TileSpot
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
declareFields ''PlayerUnits

cityAt :: CityId -> Field Tile City
cityAt cityId = tileCities .> mapAt cityId

ruinAt :: CityId -> Field Tile Ruin
ruinAt ruinId = tileRuins .> mapAt ruinId

playerUnitsHiglight :: PlayerId -> Field Tile Bool
playerUnitsHiglight p = Field
  { getField = getField pHighlight . Map.findWithDefault noUnits p . tilePlayers
  , setField = \b t ->
    t { tilePlayers =
        let ps = tilePlayers t
        in case Map.lookup p ps of
             Nothing -> ps
             Just us -> Map.insert p (setField pHighlight b us) ps
      }
  }

playerUnits :: PlayerId -> Field Tile (Bag UnitType)
playerUnits p = Field
  { getField = getField pUnits . Map.findWithDefault noUnits p . tilePlayers
  , setField = \b t ->
      t { tilePlayers =
          let ps = tilePlayers t
          in if bagIsEmpty b
               then Map.delete p ps
               else let new = setField pUnits b
                            $ Map.findWithDefault noUnits p ps
                    in Map.insert p new ps
        }
  }

countWorkersOnTile :: PlayerId -> Tile -> Int
countWorkersOnTile pid t = free + fromMap citySpot tileCities +
                                  fromMap ruinSpot tileRuins
  where
  free = sum $ map snd $ bagToList $ getField (playerUnits pid) t

  fromMap g f =
    sum $ map (countTileSpot . getField g) $ Map.elems $ getField f t

  countTileSpot s =
    case s of
      Occupied x | x == pid -> 1
      _ -> 0


setCapital :: Maybe PlayerId -> Tile -> Tile
setCapital p = updField tileCities (fmap citySetCapital)
  where
  citySetCapital c = c { cityCapital = p }

isStartTile :: Tile -> Bool
isStartTile t =
  case tileNumber t of
    TileNum _ -> False
    _         -> True

-- | Check for opponents opponents oustide city
tileHasOpponents :: PlayerId -> Tile -> Bool
tileHasOpponents pid = any hasOpponent . Map.toList . tilePlayers
  where
  hasOpponent (pid',us) = pid /= pid' && (bagContains FreeUnit   bag > 0 ||
                                          bagContains LockedUnit bag > 0)
    where bag = getField pUnits us

-- | Pick a unit that can enter a city/ruin
enterUnit :: PlayerId -> Tile -> [ UnitType ]
enterUnit playerId tile
  | bagContains LockedUnit units > 0 = [ LockedUnit ]
  | bagContains FreeUnit   units > 0 = [ FreeUnit ]
  | otherwise                        = []
  where
  units = getField (playerUnits playerId) tile


tileEnterCities :: PlayerId -> Tile -> [ (CityId,UnitType) ]
tileEnterCities playerId tile =
  [ (cid,ty)
    | (cid,city) <- Map.toList (getField tileCities tile)
    , Empty      <- [getField citySpot city]
    , ty         <- enterUnit playerId tile
  ]

tileEnterRuins :: PlayerId -> Tile -> [ (RuinId,UnitType) ]
tileEnterRuins playerId tile =
  [ (rid,ty)
    | (rid,ruin) <- Map.toList (getField tileRuins tile)
    , not (null (ruinTokens ruin))
    , Empty      <- [getField ruinSpot ruin]
    , ty         <- enterUnit playerId tile
  ]

tileCanMoveFrom :: PlayerId -> Tile -> Bool
tileCanMoveFrom playerId tile = bagContains FreeUnit units > 0
  where
  units = getField (playerUnits playerId) tile

tileCanFlyFrom :: PlayerId -> Tile -> Bool
tileCanFlyFrom playerId tile = bagContains LockedUnit units > 0 ||
                               bagContains FreeUnit   units > 0
  where
  units = getField (playerUnits playerId) tile

tileHasLocked :: PlayerId -> Tile -> Int
tileHasLocked playerId =
  bagContains LockedUnit . getField (playerUnits playerId)

tileHasUnits :: PlayerId -> Tile -> Bool
tileHasUnits playerId tile =
  bagContains LockedUnit units > 0 ||
  bagContains FreeUnit   units > 0 ||
  any (isPresent . getField citySpot) cities ||
  any (isPresent . getField ruinSpot) ruins
  where
  units  = getField (playerUnits playerId) tile
  cities = getField tileCities tile
  ruins  = getField tileRuins tile
  isPresent r = case r of
                  Occupied p -> p == playerId
                  _          -> False


--------------------------------------------------------------------------------
-- Setup


--------------------------------------------------------------------------------

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
defTile' :: TileName -> TileType -> Terrain -> [City] -> [Ruin] -> Tile
defTile' name ty ter cs rs = Tile
  { tileNumber    = name
  , tileType      = ty
  , tileTerrain   = ter
  , _tileCities   = Map.fromList (zip [ 0 .. ] cs)
  , _tileRuins    = Map.fromList (zip [ 0 .. ] rs)
  , _tileVisible  = False
  , tilePlayers   = Map.empty
  }

defTile :: Int -> TileType -> Terrain -> [City] -> [Ruin] -> Tile
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
  let mkTile n = defTile n TileCenter
  in
  [ mkTile 31 Forest
      [ defCity $ Action [ DrawResource, Gem ]
      , defCity $ Action [ Develop (Same 2), Gem ]
      ]
      []

  , mkTile 32 Plains
      [ defCity $ If (LooseResource AnyNormal)
                                        [ Gem `Times` 2, Develop (Same 2) ] ]
      [ defRuin Gold ]

  , mkTile 33 Swamp
      [ defCity $ Action [ DrawResource, Develop (Same 4) ] ]
      []

  , mkTile 34 Mountain
      [ defCity $ Action [ Gem, Develop (Same 3) ] ]
      [ defRuin Gold ]

  , mkTile 35 Mountain
      [ defCity $ If RemoveWorker [ Gem `Times` 3 ] ]
      []

  , mkTile 36 Forest
      [ defCity $ Action [ Gem `Times` 2 ] ]
      [ defRuin Gold, defRuin Gold ]
  ]


peripheralTiles :: [Tile]
peripheralTiles =
  let mkTile n = defTile n TilePeripheral
  in
  [ mkTile 1 Plains
      [ defCity $ Action [ Gem ] ]
      []

  , mkTile 2 Plains
      [ defCity $ Action [ Develop (Same 2) ] ]
      []

  , mkTile 3 Plains
      [ defCity $ Action [ DrawResource, Develop (Same 2) ] ]
      []

  , mkTile 4 Plains
      [ defCity $ Action [ Develop (Same 3) ] ]
      []

  , mkTile 5 Plains
      [ defCity $ Action [ Gem ] ]
      []

  , mkTile 6 Plains
      [ defCity $ Action [ DrawResource, Develop (Same 2) ] ]
      []

  , mkTile 7 Plains
      []
      [ defRuin Silver ]

  , mkTile 8 Plains
      []
      [ defRuin Silver ]

  , mkTile 9 Plains
      []
      [ defRuin Silver ]

  , mkTile 10 Plains
      []
      [ defRuin Silver ]


  , mkTile 11 Forest
      [ defCity $ Action [ Gem ] ]
      []

  , mkTile 12 Forest
      [ defCity $ Action [ DrawResource, Develop (Same 2) ] ]
      []

  , mkTile 13 Forest
      [ defCity $ Action [ Develop (Same 3) ] ]
      []

  , mkTile 14 Forest
      [ defCity $ Action [ Develop (Same 2) ] ]
      [ defRuin Silver ]

  , mkTile 15 Plains
      [ ]
      [ defRuin Silver ]

  , mkTile 16 Forest
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , mkTile 17 Swamp
      [ ]
      [ defRuin Silver ]

  , mkTile 18 Swamp
      [ ]
      [ defRuin Silver ]

  , mkTile 19 Mountain
      [ defCity $ Action [ Gem ] ]
      []

  , mkTile 20 Mountain
      [ ]
      [ defRuin Silver ]

  , mkTile 21 Plains
      [ ]
      [ defRuin Silver ]

  , mkTile 22 Mountain
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , mkTile 23 Plains
      [ defCity $ Action [ Develop (Same 4)  ] ]
      []

  , mkTile 24 Plains
      [ defCity $ Action [ Move `Times` 2 ] ]
      []

  , mkTile 25 Plains
      [ defCity $ Action [ DrawResource, Move ] ]
      []

  , mkTile 26 Forest
      [ defCity $ Action [ Gem ] ]
      []

  , mkTile 27 Forest
      [ defCity $ Action [ Develop (Same 3) ] ]
      [ defRuin Silver ]

  , mkTile 28 Plains
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , mkTile 29 Swamp
      [ ]
      [ defRuin Silver, defRuin Silver ]

  , mkTile 30 Mountain
      [ defCity $ Action [ Fly ] ]
      [ ]
  ]


startTiles :: [Tile]
startTiles =
  let mkTile n = defTile' n TileStartgin
  in
  [ mkTile Capital Plains
      [ defCity $ Action [ Move, Develop Any ] ]
      []

  , mkTile (TNW Nothing) Plains
      []
      [ defRuin Bronze ]

  , mkTile (TNE Nothing ) Plains
      [ defCity $ Move `Or` Develop Any ]
      []


  , mkTile (TNW (Just Blue)) Swamp
      []
      [ defRuin Bronze ]

  , mkTile (TNE (Just Blue)) Plains
      [ defCity $ Move `Or` DrawResource ]
      []


  , mkTile (TNW (Just Purple)) Plains
      [ defCity $ Action [ Develop Any ] ]
      []

  , mkTile (TNE (Just Purple)) Plains
      [ defCity $ Action [ Move ] ]
      []


  , mkTile (TNW (Just Red)) Plains
      []
      [ defRuin Bronze ]

  , mkTile (TNE (Just Red)) Plains
      [ defCity $ Move `Or` Attack ]
      []


  , mkTile (TNW (Just Orange)) Mountain
      []
      [ defRuin Bronze, defRuin Bronze ]

  , mkTile (TNE (Just Orange)) Plains
      [ defCity $ Move `Or` Develop (Same 2) ]
      []


  , mkTile (TNW (Just Yellow)) Plains
      [ defCity $ Move `Or` Develop Any ]
      []

  , mkTile (TNE (Just Yellow)) Plains
      []
      [ defRuin Bronze ]


  , mkTile (TNW (Just Green)) Plains
      []
      [ defRuin Bronze ]

  , mkTile (TNE (Just Green)) Plains
      [ defCity $ (Move `Times` 2) `Or` Develop Any ]
      []
  ]

