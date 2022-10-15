{-# Language TemplateHaskell #-}
module Game
  ( Game
  , initialGame
  , playerAfter
  , gameTurnOrder
  , gameCurrentPlayer
  , gamePlayer
  , gamePlayers
  , gameBoard
  , gameTokens
  , gameTurn
  , gameEndVPSpot
  , gameTokenRemaining
  , gameCompletedBonusRoute
  , gameIsFinished
  , GameUpdate(..)
  , doUpdate
  , computeScore
  ) where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe(isJust)
import GHC.Generics

import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as JS
import Data.Aeson ((.=),ToJSON(..))

import Common.Utils
import Common.Basics
import Common.RNG
import Common.Field

import Basics
import Stats
import Bonus
import Player
import Board
import Edge
import Node
import Turn
import Event

data GameUpdate =
    PlaceWorkerOnEdge EdgeId Int Worker
  | RemoveWorkerFromEdge EdgeId Int
  | EdgeRemoveBonus EdgeId
  | EdgeSetBonus EdgeId BonusToken
  | SetEndVPAt Level Worker
  | UseBonusToken PlayerId BonusToken

  | PlaceWorkerInOffice NodeId Worker
  | PlaceWorkerInAnnex NodeId Worker
  | SwapWorkers NodeId Int  -- swap the worker at the given spot and the prev

  | SetWorkerPreference Worker
  | ChangeAvailble Worker Int
  | ChangeUnavailable Worker Int
  | ChangeVP PlayerId Int
  | Upgrade PlayerId Stat
  | GainBonusToken PlayerId BonusToken

  | NewTurn Turn
  | ChangeDoneActions Int
  | ChangeActionLimit Int
  | AddWorkerToHand (Maybe ProvinceId) Worker
  | RemoveWorkerFromHand
  | UseGateway ProvinceId
  | DrawBonusToken
  | PlacingBonus (Maybe BonusToken)
  | AchieveBonusRoute PlayerId

  | Log Event
  | SetFull Int
  | EndGame

  deriving (Show,Generic)

data Game = Game
  { _gamePlayers  :: Map PlayerId Player
  , gameTurnOrder :: [PlayerId]
  , _gameTokens   :: [BonusToken]
  , _gameTokenRemaining :: Int
  , _gameBoard    :: Board
  , _gameLog      :: [Event]
  , _gameStatus   :: Turn
  , _gameFinished :: !(Maybe FinalScore)
  , _gameEndVPSpots :: Map Level Worker
  , _gameCompletedBonusRoute :: Set PlayerId
  } deriving (Show,Read,Generic)


newtype FinalScore = FinalScore Score
  deriving (Show,Read)

declareFields ''Game

gameIsFinished :: Game -> Bool
gameIsFinished s = case getField gameFinished s of
                     Just _ -> True
                     Nothing -> False



gamePlayer :: PlayerId -> Field Game Player
gamePlayer playerId = gamePlayers .> mapAt playerId

gameTurn :: Field Game Turn
gameTurn = gameStatus

gameCurrentPlayer :: Game -> PlayerId
gameCurrentPlayer = currentPlayer . getField gameTurn

gameEndVPSpot :: Level -> Game -> Maybe Worker
gameEndVPSpot lvl = getField (gameEndVPSpots .> mapAtMaybe lvl)

doUpdate :: GameUpdate -> Game -> Game
doUpdate upd =
  case upd of

    -- Player

    SetWorkerPreference Worker {..} ->
        (gamePlayer owner `updField` setWorkerPreference shape)

    ChangeAvailble Worker{..} n ->
        (gamePlayer owner `updField` changeWorker Active shape n)

    ChangeUnavailable Worker{..} n ->
        (gamePlayer owner `updField` changeWorker Passive shape n)

    ChangeVP playerId n ->
      (gamePlayer playerId `updField` addVP n)

    Upgrade playerId act ->
      (gamePlayer playerId `updField` levelUp act)

    GainBonusToken playerId bonus ->
      (gamePlayer playerId `updField` gainBonus bonus)

    UseBonusToken playerId bonus ->
      (gamePlayer playerId `updField` useBonus bonus)


    -- nodes
    PlaceWorkerInOffice nodeId worker ->
      (gameBoard .> boardNode nodeId `updField` nodeAddWorker worker)

    PlaceWorkerInAnnex nodeId worker ->
      (gameBoard .> boardNode nodeId `updField` nodeAddExtra worker)

    SwapWorkers nodeId spot ->
      (gameBoard .> boardNode nodeId `updField` nodeSwap spot)

    SetEndVPAt lvl worker ->
        (setField (gameEndVPSpots .> mapAtMaybe lvl) (Just worker))

    -- edges

    PlaceWorkerOnEdge edgeId spot w ->
        (gameBoard .> boardEdge edgeId `updField` edgeSetWorker spot (Just w))

    RemoveWorkerFromEdge edgeId spot ->
        (gameBoard .> boardEdge edgeId `updField` edgeSetWorker spot Nothing)

    EdgeRemoveBonus edgeId ->
        (gameBoard .> boardEdge edgeId `updField` edgeRemoveBonus)

    EdgeSetBonus edgeId bonus ->
        (gameBoard .> boardEdge edgeId `updField` edgeSetBonus bonus)

    SetFull _ -> id

    -- turn

    NewTurn turn -> setField gameTurn turn

    UseGateway g ->
      (gameTurn `updField` useGateway g)

    ChangeDoneActions n ->
      (gameTurn .> actionsDone `updField` (+n))

    ChangeActionLimit n ->
      (gameTurn .> currentActionLimit `updField` (+n))

    AddWorkerToHand prov w ->
      (gameTurn `updField` addWorkerToHand prov w)

    RemoveWorkerFromHand ->
      (gameTurn `updField` removeWorkerFromHand)

    DrawBonusToken ->
      (gameTokenRemaining `updField` subtract 1)

    PlacingBonus b ->
      (if isJust b then (gameTokens `updField` drop 1) else id)
      . (gameTurn .> turnPlacing   `setField` b)

    AchieveBonusRoute playerId ->
      (gameCompletedBonusRoute `updField` Set.insert playerId)

    -- events
    Log e ->
      (gameLog `updField` (e:))

    EndGame ->
      \g -> g { _gameFinished = Just (FinalScore (computeScore g)) }

playerAfter :: PlayerId -> Game -> PlayerId
playerAfter playerId state =
  case break (== playerId) (gameTurnOrder state) of
    (_, _ : next : _) -> next
    (next : _, _)     -> next
    _                 -> playerId -- shouldn't happen



initialGame :: RNG -> Board -> Set PlayerId -> Game
initialGame rng0 board playerIds =
  Game
    { _gamePlayers   = playerState
    , gameTurnOrder  = playerOrder
    , _gameTokens    = otherTokens
    , _gameTokenRemaining = length otherTokens
    , _gameBoard     = foldr addToken board startToks
    , _gameStatus    = newTurn firstPlayer (getLevel Actions firstPlayerState)
    , _gameFinished  = Nothing
    , _gameLog       = [ EvSay [ EvPlayer firstPlayer, "' turn" ]
                       , StartTurn
                       ]
    , _gameEndVPSpots= Map.empty
    , _gameCompletedBonusRoute = Set.empty
    }

  where
  (playerOrder,initialToks,otherTokens) =
    let (ps, rng1)    = shuffle (Set.toList playerIds) rng0
        (start, rng2) = shuffle startTokens rng1
        (toks,_)      = shuffle tokenList rng2
    in (ps,start,toks)

  startToks = zip initialToks (Set.toList (boardInitialTokens board))
  addToken (tok,loc) = updField (boardEdge loc) (edgeSetBonus tok)

  firstPlayer = head playerOrder
  firstPlayerState = playerState Map.! firstPlayer

  playerState =
    Map.fromList [ (p, initialPlayer i) | p <- playerOrder | i <- [ 0 .. ] ]


-- Score breakdown

computeScore :: Game -> Score
computeScore game =
  fmap complete
  $ endVPscore
  $ scoreCities board
  $ foldr (\p -> scoreProvince p board) fromPBoard
  $ Map.keys (boardProvinces board)
  where
  palyers = Map.toList (getField gamePlayers game)
  board   = getField gameBoard game

  fromPBoard = Map.unionsWith Map.union
             $ [ Map.insert "Network"
                   ( Map.singleton pid
                   $ networkSize pid board * keyPoints (getLevel Keys s)
                   )
                   (Map.singleton pid <$> scoreFromPlayerBoard s)
               | (pid,s) <- palyers
               ]

  endVPNodeName = nodeName
                $ head
                $ filter isEndVP
                $ Map.elems
                $ getField boardNodes board
  isEndVP n = GainEndGamePoints `elem` nodeActions n
  endVPscore =
    Map.insert endVPNodeName
    $ Map.fromListWith (+)
        [ (owner w, endVPTrack s)
        | (s,w) <- Map.toList (getField gameEndVPSpots game)
        ]

  zeros = Map.fromList [ (p,0) | p <- gameTurnOrder game ]
  complete mp = Map.union mp zeros

--------------------------------------------------------------------------------

instance ToJSON Game where
  toJSON g = JS.object
    [ "players" .=
        JS.object [ JS.fromText (jsKey pId) .= p
                  | (pId,p) <- Map.toList (getField gamePlayers g)
                  ]
    , "turnOrder" .= gameTurnOrder g
    , "board"     .= getField gameBoard g
    , "endVP"     .= jsMap (getField gameEndVPSpots g)
    , "log"       .= getField gameLog g
    , "tokens"    .= getField gameTokenRemaining g
    , "status"    .= case getField gameFinished g of
                       Nothing -> toJSON (getField gameStatus g)
                       Just f  -> toJSON f
    , "score"     .= computeScore g
    ]


instance ToJSON FinalScore where
  toJSON (FinalScore score) = JS.object
    [ "tag"   .= ("finished" :: Text)
    , "score" .= score
    ]

instance ToJSON GameUpdate



