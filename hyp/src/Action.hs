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

{-
freeSpots :: Tech -> -> [(Int,Int,ResourceReq)]
freeSpots as =
  case filter hasCubes opts of
    []        -> [ (n,s,r) | (n,a) <- opts, (s,r) <- free a ]
    (n,a) : _ -> [ (n,s,r) | (s,r) <- free a ]
  where
  free           = costFreeSpots . getField baCost
  opts           = zip [0..] as
  hasCubes (_,a) = not (null (costFullSpots (getField baCost a)))

pbFreeSpots :: Map GroupName [BoardAction] -> [(GroupName,Int,Int,ResourceReq)]
pbFreeSpots mp = [ (g,n,s,r) | (g,a) <- Map.toList mp, (n,s,r) <- agFreeSpots a]
-}


