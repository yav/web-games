module AppTypes (module AppTypes, Input) where

import Data.Text(Text)
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
import Common.Bag

import Resource
import Geometry
import Tile
import Layout
import PlayerState
import Turn
import Tech
import Action
import RuinToken
import Log


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
  | SetRuinToken PlayerId [Token]
  | GainAchievement PlayerId Achievement

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
  | SetEndOn PlayerId
  | EndGame [FinalScore]

  | AddLog LogEvent

  deriving (Generic,ToJSON)

data FinalScore = FinalScore
  { fsPlayer :: PlayerId
  , fsPoints :: Map Text Int
  , fsScore  :: (Int,Int,Int,Int)
  , fsRank   :: Int
  } deriving (Generic,ToJSON)




data State = State
  { _gamePlayers  :: Map PlayerId PlayerState
  , _gameTurn     :: Turn
  , gameTurnOrder :: [PlayerId]
  , gameEnd       :: Int
  , _gameEndOn    :: Maybe PlayerId
  , _gameBoard    :: Board
  , _gameSupply   :: Bag Resource
  , _gameMarkets  :: Map DeckName Market
  , _gameFinished :: Maybe [FinalScore]
  , _gameLog      :: [LogEvent]
  } deriving (Generic,ToJSON)

declareFields ''State

type StateView = State

type Finished = State


initialState :: RNG -> Bool -> Int -> [PlayerId] -> State
initialState rng useFog len ps = State
  { gameTurnOrder = ps
  , gameEnd = len
  , _gameEndOn = Nothing
  , _gamePlayers  = Map.fromList pstates
  , _gameBoard = brd
  , _gameTurn = newTurn (head ps)
  , _gameSupply = bagFromNumList [ (r,24) | r <- enumAll, r /= Gray ]
  , _gameMarkets = markets
  , _gameFinished = Nothing
  , _gameLog = []
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

playerView :: PlayerId -> State -> StateView
playerView pid = updField gamePlayers (Map.mapWithKey hide)
  where
  hide x s = if x == pid then s else hideTokens s

type UpdateView = Update

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView pid upd =
  case upd of
    SetRuinToken pid' ts | pid /= pid' -> SetRuinToken pid' (map hideToken ts)
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

    GainAchievement playerId a ->
      Right . updField (playerState playerId .> playerAchievements)
                       (Set.insert a)

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
      Right . setField (gameBoard .> tileAt loc .> playerUnitsHighlight p) yes

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

    SetEndOn playerId ->
      Right . setField gameEndOn (Just playerId)

    AddLog ev ->
      Right . updField gameLog (ev:)

    EndGame fs ->
      Left . setField gameFinished (Just fs)
