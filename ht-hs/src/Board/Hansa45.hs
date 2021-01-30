module Board.Hansa45 where

import Board.Builder


board :: Board
board = buildBoard
  BoardBuilder
    { name = "ht_45"
    , maxFull = 10
    , bonusRoute = ("Arnheim","Stendal")
    , nodes =
        [ NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Magdeburg"
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
                  { initNodeName = "Stendal"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
                      , NodeSpot
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
                  { initNodeName = "Coellen"
                  , initNodeActions = [ GainEndGamePoints ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 1 , spotRequires = Require Cube , spotPrivilege = 1 }
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
                  { initNodeName = "Lüneburg"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 2 }
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
                  { initNodeName = "Quedlinburg"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 2 }
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
                  { initNodeName = "Goslar"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
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
                  { initNodeName = "Halle"
                  , initNodeActions = [ UpdgradeStat Keys ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 1 , spotRequires = Require Cube , spotPrivilege = 1 }
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
                  { initNodeName = "Münster"
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
                  { initNodeName = "Göttingen"
                  , initNodeActions = [ UpdgradeStat Actions ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
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
                  { initNodeName = "Minden"
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
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Stade"
                  , initNodeActions = [ UpdgradeStat Privilege ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Arnheim"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
                      , NodeSpot
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
                  { initNodeName = "Emden"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
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
                  { initNodeName = "Gröningen"
                  , initNodeActions = [ UpdgradeStat Movement ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 1 , spotRequires = Require Cube , spotPrivilege = 1 }
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
                  { initNodeName = "Bremen"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
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
                  { initNodeName = "Hildesheim"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
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
                  { initNodeName = "Kampen"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 2 }
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
                  { initNodeName = "Dortmund"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
                      , NodeSpot
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
                  { initNodeName = "Perleberg"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 3 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 4 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Osnabrück"
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
                  { initNodeName = "Brunswick"
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
                  { initNodeName = "Lübeck"
                  , initNodeActions = [ UpdgradeStat Hiring ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 1 , spotRequires = Require Cube , spotPrivilege = 1 }
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
                  { initNodeName = "Paderborn"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 4 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Duisburg"
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
                  { initNodeName = "Hamburg"
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
                  { initNodeName = "Warburg"
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
                  { initNodeName = "Hannover"
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
        ]
    , edges =
        [ EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Hamburg"
            , to = "Lübeck"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Hamburg"
            , to = "Lüneburg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Hamburg"
            , to = "Bremen"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Hamburg"
            , to = "Stade"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Emden"
            , to = "Gröningen"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Emden"
            , to = "Osnabrück"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Emden"
            , to = "Stade"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Kampen"
            , to = "Osnabrück"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Bremen"
            , to = "Osnabrück"
            , startBonus = True
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Bremen"
            , to = "Minden"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Bremen"
            , to = "Hannover"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Minden"
            , to = "Hannover"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Minden"
            , to = "Brunswick"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Minden"
            , to = "Paderborn"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Minden"
            , to = "Münster"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Arnheim"
            , to = "Kampen"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Arnheim"
            , to = "Münster"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Arnheim"
            , to = "Duisburg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Duisburg"
            , to = "Dortmund"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Paderborn"
            , to = "Dortmund"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Paderborn"
            , to = "Warburg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Paderborn"
            , to = "Hildesheim"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Lüneburg"
            , to = "Hannover"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Lüneburg"
            , to = "Perleberg"
            , startBonus = True
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Stendal"
            , to = "Perleberg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Stendal"
            , to = "Magdeburg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Stendal"
            , to = "Brunswick"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Warburg"
            , to = "Coellen"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Warburg"
            , to = "Göttingen"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Quedlinburg"
            , to = "Göttingen"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Quedlinburg"
            , to = "Halle"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Quedlinburg"
            , to = "Goslar"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Goslar"
            , to = "Magdeburg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Goslar"
            , to = "Hildesheim"
            , startBonus = True
            }
        ]
    }
