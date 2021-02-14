module BoardActions where

import Resource
import Action


--------------------------------------------------------------------------------

emptyPlayerBoard :: [Tech]
emptyPlayerBoard =
  [ def "Exploration"
      [ [Exact Green, AnyNormal ]    ~> Action [ Move `Times` 2 ]
      , [Exact Green, Exact Purple ] ~> Action [ Move, PlaceWorker ]
      ]

  , def "Millitary"
      [ [ Exact Red, AnyNormal ]   ~> Or (Action [ Attack ])
                                         (Action [ Fortify `Times` 2 ])
      , [ Exact Red, Exact Green ] ~> Action [ Attack, Move ]
      ]

  , def "Population"
      [ [ Exact Purple, AnyNormal ] ~> Action [ PlaceWorker, Fortify ]
      , [ Exact Purple, Exact Red ] ~> Action [ PlaceWorker, Attack ]
      ]

  , def "Engineering"
      [ [ Exact Orange, AnyNormal ]  ~> Action [ Develop (Different 2) ]
      , [ Exact Orange, Exact Blue ] ~> Action [ Develop (Same 2) ]
      ]

  , def "Commerce"
      [ [ Exact Yellow, AnyNormal ]    ~> Action [ Gem ]
      , [ Exact Yellow, Exact Orange ] ~> Action [ Gem, Develop Any ]
      ]

  , def "Research"
      [ [ Exact Blue, AnyNormal, AnyNormal ]    ~> Action [ GainTech ]
      , [ Exact Blue, Exact Yellow, AnyNormal ] ~> Action [ GainTech, Gem ]
      ]
  ]
  where
  a ~> b = TechAlt { _techCost = map emptySpot a
                   , techBenefit = OneTime b }
  def x as = Tech { techName = x
                  , techVP   = 0
                  , _techAlts = as
                  }

