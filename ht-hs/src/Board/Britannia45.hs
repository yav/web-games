{-# Language OverloadedStrings #-}
module Board.Britannia45 where

import Board.Builder

board :: Board
board = buildBoard
  BoardBuilder
    { name = "britannia_45"
    , maxFull = 8
    , bonusRoute = ("Oxford","York")
    , nodes =
        [ NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Newcastle"
                  , initNodeActions = [ UpdgradeStat Hiring ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 3 }
                      ]
                  }
            , provinces = [ "Scotland" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Dunbar"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 4 }
                      ]
                  }
            , provinces = [ "Scotland" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Nottingham"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "York"
                  , initNodeActions = [ UpdgradeStat Keys ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 2 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Richmond"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Isle of Man"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 3 }
                      ]
                  }
            , provinces = [ "Scotland" , "Wales" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Chester"
                  , initNodeActions = [ UpdgradeStat Actions ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 4 }
                      ]
                  }
            , provinces = [ "Wales" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Glasgow"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      ]
                  }
            , provinces = [ "Scotland" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Falkirk"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 4 }
                      ]
                  }
            , provinces = [ "Scotland" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Pembroke"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 4 }
                      ]
                  }
            , provinces = [ "Wales" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Salisbury"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Conway"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      ]
                  }
            , provinces = [ "Wales" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Edinburgh"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 3 }
                      ]
                  }
            , provinces = [ "Scotland" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Carlisle"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 4 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 4 }
                      ]
                  }
            , provinces = [ "Scotland" ]
            , gateway = [ "Scotland" ]
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Montgomery"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      ]
                  }
            , provinces = [ "Wales" ]
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Ipswich"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 2 , spotRequires = Require Cube , spotPrivilege = 2 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Coventry"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Norwich"
                  , initNodeActions = [ UpdgradeStat Movement ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Durham"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Lancaster"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Calais"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Canterbury"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 3 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Oxford"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "London"
                  , initNodeActions = [ UpdgradeStat Privilege ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 3 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 4 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 4 }
                      ]
                  }
            , provinces = []
            , gateway = [ "Wales" , "Scotland" ]
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Hereford"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 4 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Southampton"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 4 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Cardiff"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 3 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 4 }
                      ]
                  }
            , provinces = [ "Wales" ]
            , gateway = [ "Wales" ]
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Cambridge"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 4 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Plymouth"
                  , initNodeActions = [ GainEndGamePoints ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 2 , spotRequires = Require Disc , spotPrivilege = 2 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Bristol"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 2 , spotRequires = Require Cube , spotPrivilege = 2 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        ]
    , edges =
        [ EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "London"
            , to = "Cambridge"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "London"
            , to = "Oxford"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "London"
            , to = "Canterbury"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , Require Disc , AnyWorker ]
                  , initEdgeBonus = Just BonusMove2
                  }
            , province = Nothing
            , from = "Canterbury"
            , to = "Calais"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , Require Disc , Require Disc ]
                  , initEdgeBonus = Just BonusPlace2
                  }
            , province = Nothing
            , from = "Calais"
            , to = "Southampton"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ] , initEdgeBonus = Nothing }
            , province = Nothing
            , from = "Salisbury"
            , to = "Southampton"
            , startBonus = True
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Salisbury"
            , to = "Plymouth"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Salisbury"
            , to = "Bristol"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Salisbury"
            , to = "Oxford"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Oxford"
            , to = "Cardiff"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Coventry"
            , to = "Cardiff"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Coventry"
            , to = "Hereford"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Coventry"
            , to = "Cambridge"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Coventry"
            , to = "Nottingham"
            , startBonus = True
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Cambridge"
            , to = "Ipswich"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Cambridge"
            , to = "Norwich"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Nottingham"
            , to = "Norwich"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Nottingham"
            , to = "York"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Durham"
            , to = "York"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Durham"
            , to = "Lancaster"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Durham"
            , to = "Carlisle"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Durham"
            , to = "Newcastle"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Hereford"
            , to = "Chester"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ] , initEdgeBonus = Nothing }
            , province = Nothing
            , from = "Hereford"
            , to = "Lancaster"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Richmond"
            , to = "Carlisle"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ] , initEdgeBonus = Nothing }
            , province = Nothing
            , from = "Richmond"
            , to = "Lancaster"
            , startBonus = True
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ Require Disc , Require Disc , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Chester"
            , to = "Carlisle"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Just "Wales"
            , from = "Montgomery"
            , to = "Cardiff"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Just "Wales"
            , from = "Montgomery"
            , to = "Pembroke"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Just "Wales"
            , from = "Montgomery"
            , to = "Conway"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Just "Wales"
            , from = "Montgomery"
            , to = "Hereford"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Just "Wales"
            , from = "Cardiff"
            , to = "Pembroke"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Just "Wales"
            , from = "Conway"
            , to = "Chester"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ Require Disc , Require Disc , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Just "Wales"
            , from = "Conway"
            , to = "Isle of Man"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , Require Disc , Require Disc ]
                  , initEdgeBonus = Nothing
                  }
            , province = Just "Scotland"
            , from = "Carlisle"
            , to = "Isle of Man"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ] , initEdgeBonus = Nothing }
            , province = Just "Scotland"
            , from = "Carlisle"
            , to = "Falkirk"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ] , initEdgeBonus = Nothing }
            , province = Just "Scotland"
            , from = "Glasgow"
            , to = "Falkirk"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ] , initEdgeBonus = Nothing }
            , province = Just "Scotland"
            , from = "Glasgow"
            , to = "Edinburgh"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ] , initEdgeBonus = Nothing }
            , province = Just "Scotland"
            , from = "Dunbar"
            , to = "Edinburgh"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Just "Scotland"
            , from = "Dunbar"
            , to = "Newcastle"
            , startBonus = False
            }
        ]
    }
