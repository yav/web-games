module Action where

import Data.Text(Text)
import Data.Aeson(ToJSON,ToJSONKey)
import GHC.Generics(Generic)

import Common.Field

import Resource

data BasicAction =
    Move | Fly
  | PlaceWorker | CloneWorker | RemoveWorker
  | Attack      | RangedAttack
  | Fortify
  | Develop DevelopConstratint
  | Gem
  | GainTech
  | DrawResource
  | ReturnResource
  | SwapResource ResourceReq ResourceReq

  | GainResource ResourceReq
  | LooseResource ResourceReq
  | LooseGem
  | LooseDevelop
  | Spy

  | Neighbours BasicAction
  | Times BasicAction Int
    deriving (Generic,ToJSON,ToJSONKey,Eq,Ord)

data DevelopConstratint = Same Int | Different Int | Any
    deriving (Generic,ToJSON,Eq,Ord)


data Action =
    If BasicAction [BasicAction]
  | Action [BasicAction]
  | Or BasicAction BasicAction
    deriving (Generic,ToJSON)

data Event =
    GainAttack
  | GainMove
  | GainWorker
  | GainGem
  | GainDevelop
  | StartTurn
    deriving (Generic,ToJSON)


-- Bonus actions do not trigger the effect again
data ContinuousAciton =
    On Event BasicAction
  | UseMoveAsFly
  | UseWorkerAsClone
    deriving (Generic,ToJSON)


data Tech = Tech
  { techName      :: Text
  , techVP        :: Int
  , _techAlts     :: [TechAlt]
  } deriving (Generic,ToJSON)

data TechAlt = TechAlt { _techCost   :: ResourceCost
                       , techBenefit :: TechBenefit }
  deriving (Generic,ToJSON)

data TechBenefit =
    OneTime Action
  | Continuous ContinuousAciton
    deriving (Generic,ToJSON)

declareFields ''Tech
declareFields ''TechAlt


