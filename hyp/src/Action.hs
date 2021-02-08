module Action where

import Data.Aeson(ToJSON)
import GHC.Generics(Generic)

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
    deriving (Generic,ToJSON)

data DevelopConstratint = Same Int | Different Int | Any
    deriving (Generic,ToJSON)


data Action =
    If BasicAction [BasicAction]
  | Action [BasicAction]
  | Or Action Action
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



