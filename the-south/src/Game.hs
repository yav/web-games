module Game where

import GHC.Generics(Generic)
import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JS

import Common.Basics
import Common.Bag
import Common.Field
import Common.Utils(enumAll)
import Common.RNG

type Deck  = [Ancient]

data Game =
  Game
    { _gameDecks         :: [Deck]
    , _gameDiscard       :: Deck
    , _gamePlayers       :: Map PlayerId Player
    , _gameCurrentPlayer :: PlayerId
    , _gameOtherPlayer   :: PlayerId
    , gameLastPlayer     :: PlayerId
    , _gameStatus        :: GameStatus
    , _gameRNG           :: RNG
    } deriving (Generic, JS.ToJSON)

data GameStatus = InProgress | EndTie | EndWinner PlayerId
  deriving (Generic, JS.ToJSON)

data Player =
  Player
    { _playerHand   :: Bag Ancient
    , _playerPoints :: Int
    , _playerSalvos :: Int
    , _playerVault  :: Bag Ancient
    } deriving (Generic, JS.ToJSON)

data Ancient = Bombarder | HeapTrawler | StiltLoper | PhaseSaber | VaultBot
  deriving (Eq,Ord,Generic,JS.ToJSON,JS.ToJSONKey,Bounded,Enum)

declareFields ''Player
declareFields ''Game

allCards :: [Ancient]
allCards = [ c | a <- enumAll, c <- replicate 10 a ]

player :: PlayerId -> Field Game Player
player pid = gamePlayers .> mapAt pid


--------------------------------------------------------------------------------
-- Setup

newGame :: RNG -> PlayerId -> PlayerId -> Game
newGame rng0 p1 p2 =
  Game
    { _gameDecks         = [ d1, d2, d3 ]
    , _gameDiscard       = disc
    , _gamePlayers       = Map.fromList [ (p1, newPlayer p1Cards)
                                        , (p2, newPlayer p2Cards)
                                        ]
    , _gameCurrentPlayer = p1
    , _gameOtherPlayer   = p2
    , gameLastPlayer     = p2
    , _gameStatus        = InProgress
    , _gameRNG           = rng
    }
  where
  (cs,rng)        = shuffle allCards rng0


  deckSize = max 0 (length allCards - 5) `div` 3

  [p1Cards, p2Cards, d1, d2, d3, disc ] =
    takes [ 2, 3, deckSize, deckSize, deckSize ] cs

takes :: [Int] -> [a] -> [[a]]
takes nums as =
  case nums of
    []     -> [as]
    n : ns -> let (xs,ys) = splitAt n as
              in xs : takes ns ys



newPlayer :: [Ancient] -> Player
newPlayer cs =
  Player
    { _playerHand   = bagFromList cs
    , _playerPoints = 40
    , _playerSalvos = 0
    , _playerVault  = bagEmpty
    }

--------------------------------------------------------------------------------
-- View

data GameView = GameView
  { decks   :: [DeckView]
  , discard :: Int
  , players :: Map PlayerId PlayerView
  } deriving (Generic,JS.ToJSON)

gameView :: PlayerId -> Game -> GameView
gameView viewer state =
  GameView
    { decks   = map deckView (getField gameDecks state)
    , discard = viewField gameDiscard length state
    , players = Map.mapWithKey (playerView viewer state)
                               (getField gamePlayers state)
    }

data DeckView = Empty | Card Ancient Int
  deriving (Generic,JS.ToJSON)

deckView :: Deck -> DeckView
deckView ds =
  case ds of
    []       -> Empty
    c : rest -> Card c (length rest)

data PlayerView = PlayerView
  { hand      :: HandView
  , points    :: Int
  , salvos    :: Int
  , vault     :: Int
  , isCurrent :: Bool
  , isLast    :: Bool
  } deriving (Generic,JS.ToJSON)

playerView :: PlayerId -> Game -> PlayerId -> Player -> PlayerView
playerView viewer state viewee pstate =
  PlayerView
    { hand      = viewField playerHand
                  (if viewer == viewee
                     then Visible . bagToList
                     else Opaque  . bagSize
                  )
                  pstate
    , points    = getField playerPoints pstate
    , salvos    = getField playerSalvos pstate
    , vault     = viewField playerVault bagSize pstate
    , isCurrent = viewee == getField gameCurrentPlayer state
    , isLast    = viewee == gameLastPlayer state
    }

data HandView = Visible [Ancient] | Opaque Int
  deriving (Generic,JS.ToJSON)

type DeckId = Int


--------------------------------------------------------------------------------
-- Input

data Input =
    Deck Int
  | Hand Int
  | Text Int Text
  deriving (Eq,Ord,Show,Read,Generic,JS.ToJSON,JS.FromJSON)


