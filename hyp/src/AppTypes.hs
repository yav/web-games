module AppTypes (module AppTypes, Input) where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List(mapAccumL)
import GHC.Generics

import Data.Aeson(ToJSON)

import Common.Basics
import Common.Utils(enumAll)
import Common.Field
import Common.RNG

import Bag
import Resource
import Geometry
import Tile
import Layout
import PlayerState
import Turn
import Tech
import Action
import RuinToken


data Update =
    PlaceCube PlayerId CubeLoc Resource
  | RemoveCube PlayerId CubeLoc
  | SetTurn Turn
  | ChangeSupply Resource Int

  | ChangeBag PlayerId BagName Resource Int
  | ChangeGems PlayerId Int
  | ChangeGhosts PlayerId Int
  | ChangeWorkers PlayerId Int
  | Capture PlayerId PlayerId   -- captured is 2nd
  | SetRuinToken PlayerId (Maybe Token)

  | Upgrade           PlayerId Resource Int
  | ResetUpgrade      PlayerId Resource

  | AddTech PlayerId TechId Tech

  | ChangeUnit PlayerId UnitType Loc Int
  | SetUnithighlight Loc PlayerId Bool

  | SetCity Loc CityId TileSpot
  | SetRuin Loc RuinId TileSpot
  | DropToken Loc RuinId

  | ChangeTile Loc Tile

  | SetMarket DeckName Market

  deriving (Generic,ToJSON)



data State = State
  { _gamePlayers  :: Map PlayerId PlayerState
  , _gameTurn     :: Turn
  , gameTurnOrder :: [PlayerId]
  , _gameEndOn    :: Maybe PlayerId
  , _gameBoard    :: Board
  , _gameSupply   :: Bag Resource
  , _gameMarkets  :: Map DeckName Market

  } deriving (Generic,ToJSON)

declareFields ''State

type Finished = State


initialState :: RNG -> Bool -> [PlayerId] -> State
initialState rng useFog ps = State
  { gameTurnOrder = ps
  , _gameEndOn = Nothing
  , _gamePlayers  = Map.fromList pstates
  , _gameBoard = brd
  , _gameTurn = newTurn (head ps)
  , _gameSupply = bagFromNumList [ (r,24) | r <- enumAll, r /= Gray ]
  , _gameMarkets = markets
  }
  where
  mkP r p = let (r1,r2) = splitRNG r
            in (r1, (p, emptyPlayerState r2))
  (marketRng,pstates) = mapAccumL mkP rng ps
  (markets,boardRng) = newMarkets marketRng
  brd = setupBoard boardRng useFog [ (Just p, Nothing) | p <- ps ]

playerState :: PlayerId -> Field State PlayerState
playerState p = gamePlayers .> mapAt p


currentPlayer :: State -> (PlayerId,PlayerState)
currentPlayer state = (playerId,player)
  where
  playerId = turnPlayer (getField gameTurn state)
  player   = getField (playerState playerId) state

--------------------------------------------------------------------------------

playerView :: PlayerId -> State -> State
playerView pid = updField gamePlayers (Map.mapWithKey hide)
  where
  hide x s = if x == pid then s else hideTokens s

playerUpdateView :: PlayerId -> Update -> Update
playerUpdateView pid upd =
  case upd of
    SetRuinToken pid' (Just t) | pid /= pid' ->
      SetRuinToken pid' (Just (hideToken t))
    _ -> upd


doUpdate :: Update -> State -> Either Finished State
doUpdate upd =
  case upd of
    PlaceCube playerId loc r ->
      Right . setField (playerState playerId .> costSpot loc .> spotResource)
                       (Just r)

    RemoveCube playerId loc ->
      Right . setField (playerState playerId .> costSpot loc .> spotResource)
                       Nothing

    ChangeBag playerId nm r n ->
      Right . updField (playerState playerId .> playerBag .> mapAt nm)
                       (bagChange n r)

    ChangeGems playerId n ->
      Right . updField (playerState playerId .> playerGems) (+n)

    ChangeGhosts playerId n ->
      Right . updField (playerState playerId .> playerGhosts) (+n)

    ChangeWorkers playerId n ->
      Right . updField (playerState playerId .> playerWorkers) (+n)

    Capture playerId capturedId ->
      Right . updField (playerState playerId .> playerCaptured)
                       (Set.insert capturedId)

    SetRuinToken playerId mb ->
      Right . setField (playerState playerId .> playerToken) mb

    SetTurn t ->
      Right . setField gameTurn t

    Upgrade playerId r n ->
      Right . updField (playerState playerId .> playerDevel .> mapAt r) (+n)

    ResetUpgrade playerId r ->
      Right . setField (playerState playerId .> playerDevel .> mapAt r) 0

    ChangeUnit playerId ty loc n ->
      Right . updField (gameBoard .> tileAt loc .> playerUnits playerId)
                       (bagChange n ty)

    SetUnithighlight loc p yes ->
      Right . setField (gameBoard .> tileAt loc .> playerUnitsHiglight p) yes

    SetCity loc cityId val ->
      Right . setField (gameBoard .> tileAt loc
                                  .> tileCities .> mapAt cityId .> citySpot)
                       val

    SetRuin loc ruinId val ->
      Right . setField (gameBoard .> tileAt loc
                                  .> tileRuins .> mapAt ruinId .> ruinSpot)
                       val

    DropToken loc ruinId ->
      Right . updField (gameBoard .> tileAt loc
                                  .> tileRuins .> mapAt ruinId .> ruinTokens)
                       (drop 1)

    ChangeTile loc t ->
      Right . setField (gameBoard .> tileAt loc) t

    ChangeSupply r n ->
      Right . updField gameSupply (bagChange n r)

    SetMarket d m ->
      Right . setField (gameMarkets .> mapAt d) m

    AddTech playerId techId tech ->
      Right . updField (playerState playerId .> playerTech)
                       (Map.insert techId tech)

