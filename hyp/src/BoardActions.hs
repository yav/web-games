module BoardActions where

import Data.Map(Map)
import qualified Data.Map as Map

import Resource
import Action

type GroupName    = Resource
data BoardAction  = BoardAction [ResourceSpot] Action



playerBoard :: Map GroupName [BoardAction]
playerBoard = Map.fromList
  [ def Green
      [ [Exact Green, AnyNormal ]    ~> Action [ Move 2 ]
      , [Exact Green, Exact Purple ] ~> Action [ Move 1, PlaceWorker 1 ]
      ]

  , def Red
      [ [ Exact Red, AnyNormal ]   ~> Or (Action [ Attack 1 ])
                                         (Action [ Fortify 2 ])
      , [ Exact Red, Exact Green ] ~> Action [ Attack 1, Move 1 ]
      ]

  , def Purple
      [ [ Exact Purple, AnyNormal ] ~> Action [ PlaceWorker 1, Fortify 1 ]
      , [ Exact Purple, Exact Red ] ~> Action [ PlaceWorker 1, Attack 1 ]
      ]

  , def Orange
      [ [ Exact Orange, AnyNormal ]  ~> Action [ Develop Different 2 ]
      , [ Exact Orange, Exact Blue ] ~> Action [ Develop Same 2 ]
      ]

  , def Yellow
      [ [ Exact Yellow, AnyNormal ]    ~> Action [ Gem 1 ]
      , [ Exact Yellow, Exact Orange ] ~> Action [ Gem 1, Develop Any 1 ]
      ]

  , def Blue
      [ [ Exact Blue, AnyNormal, AnyNormal ]    ~> Action [ GainTech 1 ]
      , [ Exact Blue, Exact Yellow, AnyNormal ] ~> Action [ GainTech 1, Gem 1 ]
      ]
  ]
  where
  a ~> b = BoardAction (map emptySpot a) b
  def x as = (x,as)

