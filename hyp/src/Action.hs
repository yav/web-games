module Action where

import Data.Aeson(ToJSON)
import GHC.Generics(Generic)

import Resource

data BasicAction =
    Move Int | Fly Int
  | PlaceWorker Int | CloneWorker Int | RemoveWorker Int
  | Attack Int  | RangedAttack Int
  | Fortify Int
  | Develop DevelopConstratint Int
  | Gem Int   -- may be -ve, but only +ve triggers cont. effect
  | GainTech Int
  | DrawResource Int
  | SwapResource ResourceReq ResourceReq

  | GainResource ResourceReq Int
  | LooseResource ResourceReq Int
  | ReturnResource Int
  | Spy Int

  | Neighbours BasicAction
    deriving (Generic,ToJSON)

data DevelopConstratint = Same | Different | Any
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



