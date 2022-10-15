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

finalState :: State -> Bool
finalState s = case getField gameFinished s of
                 Just {} -> True
                 Nothing -> False

doUpdate :: Update -> State -> State
doUpdate upd =
  case upd of
    PlaceCube playerId loc r ->
      setField (playerState playerId .> costSpot loc .> spotResource)
               (Just r)

    RemoveCube playerId loc ->
      setField (playerState playerId .> costSpot loc .> spotResource)
               Nothing

    ChangeBag playerId nm r n ->
      updField (playerState playerId .> playerBag .> mapAt nm)
               (bagChange n r)

    ChangeGems playerId n ->
      updField (playerState playerId .> playerGems) (+n)

    ChangeGhosts playerId n ->
      updField (playerState playerId .> playerGhosts) (+n)

    ChangeWorkers playerId n ->
      updField (playerState playerId .> playerWorkers) (+n)

    Capture playerId capturedId ->
      updField (playerState playerId .> playerCaptured)
               (Set.insert capturedId)

    SetRuinToken playerId mb ->
      setField (playerState playerId .> playerToken) mb

    GainAchievement playerId a ->
      updField (playerState playerId .> playerAchievements)
               (Set.insert a)

    SetTurn t ->
      setField gameTurn t

    Upgrade playerId r n ->
      updField (playerState playerId .> playerDevel .> mapAt r) (+n)

    ResetUpgrade playerId r ->
      setField (playerState playerId .> playerDevel .> mapAt r) 0

    ChangeUnit playerId ty loc n ->
      updField (gameBoard .> tileAt loc .> playerUnits playerId)
               (bagChange n ty)

    SetUnithighlight loc p yes ->
      setField (gameBoard .> tileAt loc .> playerUnitsHighlight p) yes

    SetCity loc cityId val ->
      setField (gameBoard .> tileAt loc
                          .> tileCities .> mapAt cityId .> citySpot)
               val

    SetRuin loc ruinId val ->
      setField (gameBoard .> tileAt loc
                          .> tileRuins .> mapAt ruinId .> ruinSpot)
               val

    DropToken loc ruinId ->
      updField (gameBoard .> tileAt loc
                          .> tileRuins .> mapAt ruinId .> ruinTokens)
               (drop 1)

    ChangeTile loc t ->
      setField (gameBoard .> tileAt loc) t

    ChangeSupply r n ->
      updField gameSupply (bagChange n r)

    SetMarket d m ->
      setField (gameMarkets .> mapAt d) m

    AddTech playerId techId tech ->
      updField (playerState playerId .> playerTech) (Map.insert techId tech)

    SetEndOn playerId ->
      setField gameEndOn (Just playerId)

    AddLog ev ->
      updField gameLog (ev:)

    EndGame fs ->
      setField gameFinished (Just fs)
