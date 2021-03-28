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

techHasCubes :: Tech -> Bool
techHasCubes = any altHasCubes . getField techAlts
  where altHasCubes = not . null . costFullSpots . getField techCost

isContinuous :: TechBenefit -> Bool
isContinuous b =
  case b of
    Continuous {} -> True
    _             -> False

techReturnSpots :: Tech -> [(Int,Int)]
techReturnSpots tech =
  [ (altId,spot)
  | (altId,alt) <- [0..] `zip` getField techAlts tech
  , spot <- altReturnSpots alt
  ]
  where
  altReturnSpots alt =
    let cost = getField techCost alt
    in case costFreeSpots cost of
         [] | techAltHas ReturnResource alt -> []
         _ -> map fst (costFullSpots cost)

techRemoveSpots :: ResourceReq -> Tech -> [(Int,Int)]
techRemoveSpots req tech =
  [ (altId,spot)
  | (altId,alt) <- [0..] `zip` getField techAlts tech
  , spot <- altRemoveSpots alt
  ]
  where
  altRemoveSpots alt =
    [ i | (i,r) <- costFullSpots (getField techCost alt), matches req r ]



techHas :: BasicAction -> Tech -> Bool
techHas act = any (techAltHas act) . getField techAlts

techAltHas :: BasicAction -> TechAlt -> Bool
techAltHas act alt =
  case techBenefit alt of
    OneTime y -> actHas y
    _         -> False

  where
  actHas x =
    case x of
      Action as -> any actBasic as
      Or a b    -> actBasic a || actBasic b
      If a bs   -> any actBasic (a:bs)

  actBasic a =
    case a of
      Times b _ -> actBasic b
      _         -> act == a



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


clickable :: BasicAction -> Bool
clickable ba =
  case ba of
    PlaceWorker       -> True
    CloneWorker       -> True
    RangedAttack      -> True
    Fortify           -> True
    Develop {}        -> True
    GainTech          -> True
    DrawResource      -> True
    ReturnResource    -> True
    SwapResource {}   -> True
    GainResource {}   -> True
    Spy               -> True

    Gem               -> False
    LooseResource {}  -> False
    LooseGem          -> False
    LooseDevelop      -> False
    RemoveWorker      -> False
    Neighbours {}     -> False
    Move              -> False
    Fly               -> False
    Attack            -> False
    Times x _         -> clickable x





autoExecute :: BasicAction -> Bool
autoExecute ba =
  case ba of

    Gem               -> True
    LooseResource {}  -> True
    LooseGem          -> True
    LooseDevelop      -> True
    RemoveWorker      -> True
    Neighbours {}     -> True

    Move              -> False
    Fly               -> False
    PlaceWorker       -> False
    CloneWorker       -> False
    Attack            -> False
    RangedAttack      -> False
    Fortify           -> False
    Develop {}        -> False
    GainTech          -> False
    DrawResource      -> False
    ReturnResource    -> False
    SwapResource {}   -> False
    GainResource {}   -> False
    Spy               -> False

    Times x _         -> autoExecute x


