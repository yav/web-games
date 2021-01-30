module Event where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Aeson(ToJSON)
import GHC.Generics(Generic)
import Data.String(IsString(..))

import Basics
import Stats
import Bonus

data EventElement =
    EvText Text
  | EvInt Int
  | EvPlayer PlayerId
  | EvWorker Worker
  | EvEdge EdgeId (Maybe Int)   -- spot on edge
  | EvNode NodeId (Maybe Int)
  | EvBonus BonusToken
  | EvStat Stat
    deriving (Show,Read,Generic)

instance IsString EventElement where
  fromString = EvText . Text.pack

-- User readable events describing the game flow
data Event =
    StartTurn
  | EndTurn
  | StartAction
  | EndAction
  | EvSay [EventElement]
    deriving (Show,Read,Generic)

instance ToJSON EventElement
instance ToJSON Event

