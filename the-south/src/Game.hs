module Game where

import GHC.Generics(Generic)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JS

import Common.Basics
import Common.RNG

data Ancient = Bombarder | HeapTrawler | StiltLoper | PhaseSaber | VaultBot
  deriving (Eq,Ord,Generic,JS.ToJSON,Bounded,Enum)

allCards :: [Ancient]
allCards = [ c | a <- [ minBound .. maxBound ], c <- replicate 10 a ]

data PlayerState =
  PlayerState
    { playerHand   :: [Ancient]
    , playerPoints :: Int
    , playerSalvos :: Int
    , playerVault  :: [Ancient]
    } deriving (Generic, JS.ToJSON)

type Deck = [Ancient]

data Game =
  Game
    { gameDecks         :: [Deck]
    , gameDiscard       :: Deck
    , gamePlayers       :: Map PlayerId PlayerState
    , gameCurrentPlayer :: PlayerId
    , gameLastPlayer    :: PlayerId
    , gameRNG           :: RNG
    } deriving (Generic, JS.ToJSON)

--------------------------------------------------------------------------------
-- Setup

newGame :: RNG -> PlayerId -> PlayerId -> Game
newGame rng0 p1 p2 =
  Game
    { gameDecks         = [ d1, d2, d3 ]
    , gameDiscard       = disc
    , gamePlayers       = Map.fromList [ (p1, newPlayer p1Cards)
                                       , (p2, newPlayer p2Cards)
                                       ]
    , gameCurrentPlayer = p1
    , gameLastPlayer    = p2
    , gameRNG           = rng
    }
  where
  (cs,rng)        = shuffle allCards rng0

  takes nums as =
    case nums of
      []     -> [as]
      n : ns -> let (xs,ys) = splitAt n as
                in xs : takes ns ys

  deckSize = max 0 (length allCards - 5) `div` 3

  [p1Cards, p2Cards, d1, d2, d3, disc ] =
    takes [ 2, 3, deckSize, deckSize, deckSize ] cs


newPlayer :: [Ancient] -> PlayerState
newPlayer cs =
  PlayerState
    { playerHand   = cs
    , playerPoints = 40
    , playerSalvos = 0
    , playerVault  = []
    }

--------------------------------------------------------------------------------

data GameView = GameView
  { decks   :: [DeckView]
  , players :: Map PlayerId PlayerView
  } deriving (Generic,JS.ToJSON)

gameView :: PlayerId -> Game -> GameView
gameView viewer state =
  GameView
    { decks   = map deckView (gameDecks state)
    , players = Map.mapWithKey (playerView viewer state) (gamePlayers state)
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

playerView :: PlayerId -> Game -> PlayerId -> PlayerState -> PlayerView
playerView viewer state viewee pstate =
  PlayerView
    { hand      = if viewer == viewee then Visible (playerHand pstate)
                                      else Opaque (length (playerHand pstate))
    , points    = playerPoints pstate
    , salvos    = playerSalvos pstate
    , vault     = length (playerVault pstate)
    , isCurrent = viewee == gameCurrentPlayer state
    , isLast    = viewee == gameLastPlayer state
    }

data HandView = Visible [Ancient] | Opaque Int
  deriving (Generic,JS.ToJSON)

--------------------------------------------------------------------------------
