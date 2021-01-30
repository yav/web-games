module Player
  ( 
    -- * Initialization
    Player
  , initialPlayer


    -- * Workers
  , getWorker
  , changeWorker
  , hireWorker
  , setWorkerPreference
  , getWorkerPreference

    -- * Bonuses
  , gainBonus
  , useBonus
  , getBonuses
  , getUsedBonuses

    -- * Stats
  , levelUp
  , getLevel
  , scoreFromPlayerBoard

    -- * Points
  , addVP
  , getVP
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Text(Text)
import GHC.Generics

import Data.Aeson(ToJSON)

import Common.Utils

import Basics
import Stats
import Bonus


data Player = Player
  { stats         :: Map Stat Level
  , available     :: Map WorkerType Int
  , unavailable   :: Map WorkerType Int
  , bonuses       :: [BonusToken]
  , spentBonuses  :: [BonusToken]
  , points        :: Int
  , preference    :: WorkerType
  } deriving (Show,Read,Generic)

zeroState :: Player
zeroState = Player
  { stats         = Map.fromList [ (stat,0) | stat <- enumAll ] -- XXX
  , available     = Map.fromList [ (w,0)    | w    <- enumAll ]
  , unavailable   = Map.fromList [ (w,0)    | w    <- enumAll ]
  , preference    = Cube
  , bonuses       = []
  , spentBonuses  = []
  , points        = 0
  }


-- 0 based turn order
initialPlayer :: Int -> Player
initialPlayer turnOrder =
     hireWorker Cube (turnOrder + 1)
  $  changeWorker Passive Cube 7
  $ foldr levelUp' zeroState enumAll
  where
  levelUp' stat = changeWorker Active (statWorker stat) 1 . levelUp stat


--------------------------------------------------------------------------------

-- | How many workers do we have of the requested type
getWorker :: WorkerHome -> WorkerType -> Player -> Int
getWorker home w =
  (Map.! w) .
  case home of
    Active  -> available
    Passive -> unavailable

-- | Change the number of workers of the given type
changeWorker :: WorkerHome -> WorkerType -> Int -> Player -> Player
changeWorker home w n =
  case home of
    Active  -> \s -> s { available   = Map.adjust (+n) w (available s) }
    Passive -> \s -> s { unavailable = Map.adjust (+n) w (unavailable s) }


-- | Move the given number of workers from unavailable to available.
hireWorker :: WorkerType -> Int -> Player -> Player
hireWorker w n = changeWorker Active w n . changeWorker Passive w (-n)

-- | Prefer placing this worker
getWorkerPreference :: Player -> WorkerType
getWorkerPreference = preference

-- | Set worker preference to this
setWorkerPreference :: WorkerType -> Player -> Player
setWorkerPreference wt = \s -> s { preference = wt }

--------------------------------------------------------------------------------

-- | Add a bonus token to the player.
gainBonus :: BonusToken -> Player -> Player
gainBonus b = \s -> s { bonuses = b : bonuses s }

-- | Mark a bonus token as spent.
useBonus :: BonusToken -> Player -> Player
useBonus b = \s ->
  s { bonuses      = List.delete b (bonuses s)
    , spentBonuses = b : spentBonuses s
    }

-- | Get available bonus tokens.
getBonuses :: Player -> [BonusToken]
getBonuses = bonuses

-- | Get spent bonus tokens.
getUsedBonuses :: Player -> [BonusToken]
getUsedBonuses = spentBonuses

--------------------------------------------------------------------------------

-- | Increase a player's state.  Note that this does not give the extra worker.
levelUp :: Stat -> Player -> Player
levelUp stat s = s { stats = Map.adjust (+1) stat (stats s) }

-- | Get the level of the specified stat
getLevel :: Stat -> Player -> Level
getLevel stat s = stats s Map.! stat

--------------------------------------------------------------------------------

-- | Add so many VP to the player.
addVP :: Int -> Player -> Player
addVP n = \s -> s { points = n + points s }

-- | Get the player's VP.
getVP :: Player -> Int
getVP = points

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
scoreFromPlayerBoard :: Player -> Map Text Int
scoreFromPlayerBoard player =
  Map.fromList
    [ ("VP",       points player)
    , ("Upgrades", sum [ maxStatePoints stat
                       | (stat,value) <- Map.toList (stats player)
                       , value == maxStat stat
                       ])
    , ("Bonuses", bonusPoints (length (getBonuses player ++
                                            getUsedBonuses player)))
    ]



instance ToJSON Player

