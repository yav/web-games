module Action where

import Data.Text(Text)
import Data.Aeson(FromJSON,ToJSON,ToJSONKey)
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
    deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON,ToJSONKey)

data DevelopConstratint = Same Int | Different Int | Any
    deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)


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

contModifyAction :: ContinuousAciton -> Action -> Action
contModifyAction cont act =
  case (cont,act) of
    (On ev a, Action as) ->
      let has r = if any (is r) as then Action (a:as) else act
      in case ev of
           GainAttack  -> has (== Attack)
           GainMove    -> has (== Move)
           GainWorker  -> has (== PlaceWorker)
           GainGem     -> has (== Gem)
           GainDevelop -> has develop
           StartTurn   -> act
    _ -> act
  where
  develop d = case d of
                Develop {} -> True
                _          -> False
  is p ba =
    case ba of
      Times b n | n > 0 -> is p b
      _ -> p ba

