module BoardActions where

import GHC.Generics(Generic)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Aeson(ToJSON)

import Common.Field

import Resource
import Action

type GroupName    = Resource
data BoardAction  = BoardAction { _baCost :: ResourceCost, baBenefit :: Action }
  deriving (Generic,ToJSON)

declareFields ''BoardAction


agFreeSpots :: [BoardAction] -> [(Int,Int,ResourceReq)]
agFreeSpots as =
  case filter hasCubes opts of
    []        -> [ (n,s,r) | (n,a) <- opts, (s,r) <- free a ]
    (n,a) : _ -> [ (n,s,r) | (s,r) <- free a ]
  where
  free           = costFreeSpots . getField baCost
  opts           = zip [0..] as
  hasCubes (_,a) = not (null (costFullSpots (getField baCost a)))

pbFreeSpots :: Map GroupName [BoardAction] -> [(GroupName,Int,Int,ResourceReq)]
pbFreeSpots mp = [ (g,n,s,r) | (g,a) <- Map.toList mp, (n,s,r) <- agFreeSpots a]



--------------------------------------------------------------------------------

emptyPlayerBoard :: Map GroupName [BoardAction]
emptyPlayerBoard = Map.fromList
  [ def Green
      [ [Exact Green, AnyNormal ]    ~> Action [ Move `Times` 2 ]
      , [Exact Green, Exact Purple ] ~> Action [ Move, PlaceWorker ]
      ]

  , def Red
      [ [ Exact Red, AnyNormal ]   ~> Or (Action [ Attack ])
                                         (Action [ Fortify `Times` 2 ])
      , [ Exact Red, Exact Green ] ~> Action [ Attack, Move ]
      ]

  , def Purple
      [ [ Exact Purple, AnyNormal ] ~> Action [ PlaceWorker, Fortify ]
      , [ Exact Purple, Exact Red ] ~> Action [ PlaceWorker, Attack ]
      ]

  , def Orange
      [ [ Exact Orange, AnyNormal ]  ~> Action [ Develop (Different 2) ]
      , [ Exact Orange, Exact Blue ] ~> Action [ Develop (Same 2) ]
      ]

  , def Yellow
      [ [ Exact Yellow, AnyNormal ]    ~> Action [ Gem ]
      , [ Exact Yellow, Exact Orange ] ~> Action [ Gem, Develop Any ]
      ]

  , def Blue
      [ [ Exact Blue, AnyNormal, AnyNormal ]    ~> Action [ GainTech ]
      , [ Exact Blue, Exact Yellow, AnyNormal ] ~> Action [ GainTech, Gem ]
      ]
  ]
  where
  a ~> b = BoardAction (map emptySpot a) b
  def x as = (x,as)

