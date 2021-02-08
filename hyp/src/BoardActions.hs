module BoardActions where

import GHC.Generics(Generic)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Aeson(ToJSON)

import Resource
import Action

type GroupName    = Resource
data BoardAction  = BoardAction { baCost :: ResourceCost, baBenefit :: Action }
  deriving (Generic,ToJSON)

baFreeSpots :: BoardAction -> [(Int,ResourceReq)]
baFreeSpots = costFreeSpots . baCost

baFullSpots :: BoardAction -> [ (Int,Resource) ]
baFullSpots = costFullSpots . baCost

baSetResource :: Int -> Maybe Resource -> BoardAction -> BoardAction
baSetResource n r b = b { baCost = costSetResource n r (baCost b) }


agFreeSpots :: [BoardAction] -> [(Int,Int,ResourceReq)]
agFreeSpots as =
  case filter hasCubes opts of
    []        -> [ (n,s,r) | (n,a) <- opts, (s,r) <- baFreeSpots a ]
    (n,a) : _ -> [ (n,s,r) | (s,r) <- baFreeSpots a ]
  where
  opts           = zip [0..] as
  hasCubes (_,a) = not (null (baFullSpots a))

asSetResouce :: Int -> Int -> Maybe Resource -> [BoardAction] -> [BoardAction]
asSetResouce n s r as =
  case splitAt n as of
    (xs,y:ys) -> xs ++ baSetResource s r y : ys
    _         -> as

pbFreeSpots :: Map GroupName [BoardAction] -> [(GroupName,Int,Int,ResourceReq)]
pbFreeSpots mp = [ (g,n,s,r) | (g,a) <- Map.toList mp, (n,s,r) <- agFreeSpots a]

pbSetResource :: GroupName -> Int -> Int -> Maybe Resource ->
                 Map GroupName [BoardAction] -> Map GroupName [BoardAction]
pbSetResource g n s r = Map.adjust (asSetResouce n s r) g


--------------------------------------------------------------------------------

playerBoard :: Map GroupName [BoardAction]
playerBoard = Map.fromList
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

