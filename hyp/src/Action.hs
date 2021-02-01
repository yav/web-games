module Action where

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

data DevelopConstratint = Same | Different | Any


data Action =
    If BasicAction [BasicAction]
  | Action [BasicAction]
  | Or Action Action

data Event =
    GainAttack
  | GainMove
  | GainWorker
  | GainGem
  | GainDevelop
  | StartTurn


-- Bonus actions do not trigger the effect again
data ContinuousAciton =
    On Event BasicAction
  | UseMoveAsFly
  | UseWorkerAsClone

