module Question where

import Data.Text(Text)
import Data.Aeson(ToJSON,FromJSON)
import GHC.Generics

import Basics
import Bonus
import Stats

data Choice =
    ChSetPreference WorkerType
  | ChActiveWorker WorkerType
  | ChPassiveWorker WorkerType
  | ChUpgrade Stat
  | ChBonusToken BonusToken

  | ChEdgeEmpty EdgeId Int WorkerType
  | ChEdgeFull  EdgeId Int (Maybe WorkerType) Worker
  | ChEdge      EdgeId

  | ChNodeEmpty NodeId WorkerType
  | ChNodeAnnex NodeId WorkerType
  | ChNodeFull  NodeId Int
  | ChNodeUpgrade NodeId Stat
  | ChEndVPSpot Level

  | ChDone Text
    deriving (Eq,Ord,Show,Read,Generic)

instance FromJSON Choice
instance ToJSON Choice

