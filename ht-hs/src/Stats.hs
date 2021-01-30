module Stats where

import GHC.Generics
import qualified Data.Aeson as JS

import Common.Utils
import Basics

data Stat = Keys | Actions | Privilege | Movement | Hiring
  deriving (Eq,Ord,Show,Read,Generic,Bounded,Enum)

-- | Special actions associated with a node
data NodeAction = UpdgradeStat Stat | GainEndGamePoints
  deriving (Eq,Ord,Show,Read)


type Level = Int

maxStat :: Stat -> Level
maxStat s =
  case s of
    Keys      -> 5
    Actions   -> 6
    _         -> 4

statWorker :: Stat -> WorkerType
statWorker s =
  case s of
    Movement -> Disc
    _        -> Cube

maxStatePoints :: Stat -> Int
maxStatePoints s =
  case s of
    Keys -> 0
    _    -> 4

keyPoints :: Level -> Int
keyPoints level =
  case level of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 2
    4 -> 3
    _ -> 4

actionLimit :: Level -> Int
actionLimit level =
  case level of
    0 -> 0
    1 -> 2
    2 -> 3
    3 -> 3
    4 -> 4
    5 -> 4
    _ -> 5

movementLimit :: Level -> Int
movementLimit level =
  case level of
    0 -> 0
    1 -> 2
    2 -> 3
    3 -> 4
    _ -> 5

hireLimit :: Level -> Maybe Int
hireLimit level =
  case level of
    0 -> Just 0
    1 -> Just 3
    2 -> Just 5
    3 -> Just 7
    _ -> Nothing

endVPTrack :: Level -> Int
endVPTrack priv =
  case priv of
    1 -> 7
    2 -> 8
    3 -> 9
    4 -> 11
    _ -> 0

--------------------------------------------------------------------------------
instance JSKey Stat where
  jsKey stat =
    case stat of
      Keys      -> "Keys"
      Actions   -> "Actions"
      Privilege -> "Privilege"
      Movement  -> "Movement"
      Hiring    -> "Hiring"

instance JS.ToJSONKey Stat where toJSONKey = jsDeriveKey
instance JS.ToJSON    Stat
instance JS.FromJSON  Stat

