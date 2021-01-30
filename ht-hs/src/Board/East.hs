module Board.East where
import Board.Builder

board :: Board
board = buildBoard
  BoardBuilder
    { name = "east"
    , maxFull = 10
    , bonusRoute = ("Lübeck","Danzig")
    , nodes =
        [ NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Berlin-Cölln"
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
                  { initNodeName = "Königsberg"
                  , initNodeActions = [ GainEndGamePoints ]
                  , initNodeSpots =
                      [ NodeSpot
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
                  { initNodeName = "Belgard"
                  , initNodeActions = []
                  , initNodeSpots = []
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Halle"
                  , initNodeActions = []
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
                  { initNodeName = "Braunsberg"
                  , initNodeActions = []
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
                  { initNodeName = "Tangermünde"
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
                  { initNodeName = "Elbing"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
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
                  { initNodeName = "Frankfurt"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
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
                  { initNodeName = "Allenstein"
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
                  { initNodeName = "Malmö"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
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
                  { initNodeName = "Anklam"
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
                  { initNodeName = "Stralsund"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
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
                  { initNodeName = "Perleberg"
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
                  { initNodeName = "Visby"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 2 }
                      ]
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Kulm"
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
                  { initNodeName = "Krakau"
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
                  { initNodeName = "Wismar"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 2 }
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
                  { initNodeName = "Stettin"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
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
                  { initNodeName = "Magdeburg"
                  , initNodeActions = [ UpdgradeStat Privilege ]
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
                  { initNodeName = "Dresden"
                  , initNodeActions = [ UpdgradeStat Keys ]
                  , initNodeSpots = []
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Havelberg"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 1 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
                          { spotVP = 0 , spotRequires = Require Disc , spotPrivilege = 1 }
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
                  { initNodeName = "Waren"
                  , initNodeActions = [ UpdgradeStat Hiring , UpdgradeStat Actions ]
                  , initNodeSpots = []
                  }
            , provinces = []
            , gateway = []
            }
        , NodeBuilder
            { nodeInit =
                InitNode
                  { initNodeName = "Breslau"
                  , initNodeActions = [ UpdgradeStat Movement ]
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 0 , spotRequires = Require Cube , spotPrivilege = 1 }
                      , NodeSpot
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
                  { initNodeName = "Thorn"
                  , initNodeActions = []
                  , initNodeSpots =
                      [ NodeSpot
                          { spotVP = 1 , spotRequires = Require Cube , spotPrivilege = 1 }
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
                  { initNodeName = "Wittenberg"
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
                  { initNodeName = "Lübeck"
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
                  { initNodeName = "Brandenburg"
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
                  { initNodeName = "Danzig"
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
        ]
    , edges =
        [ EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Krakau"
            , to = "Thorn"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Krakau"
            , to = "Breslau"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Allenstein"
            , to = "Braunsberg"
            , startBonus = True
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Braunsberg"
            , to = "Danzig"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Danzig"
            , to = "Königsberg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Danzig"
            , to = "Belgard"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots =
                      [ AnyWorker , AnyWorker , AnyWorker , Require Disc ]
                  , initEdgeBonus = Just BonusReuse2
                  }
            , province = Nothing
            , from = "Danzig"
            , to = "Malmö"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Wismar"
            , to = "Lübeck"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , Require Disc , Require Disc ]
                  , initEdgeBonus = Just BonusMove2
                  }
            , province = Nothing
            , from = "Wismar"
            , to = "Stralsund"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Waren"
            , to = "Wismar"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Waren"
            , to = "Havelberg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Havelberg"
            , to = "Perleberg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Brandenburg"
            , to = "Tangermünde"
            , startBonus = True
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Brandenburg"
            , to = "Berlin-Cölln"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Brandenburg"
            , to = "Stettin"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Magdeburg"
            , to = "Halle"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Halle"
            , to = "Wittenberg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Halle"
            , to = "Dresden"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Halle"
            , to = "Brandenburg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Dresden"
            , to = "Krakau"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Breslau"
            , to = "Frankfurt"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Frankfurt"
            , to = "Stettin"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Stettin"
            , to = "Kulm"
            , startBonus = True
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Stettin"
            , to = "Anklam"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Stettin"
            , to = "Waren"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Thorn"
            , to = "Allenstein"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Kulm"
            , to = "Elbing"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Kulm"
            , to = "Belgard"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Elbing"
            , to = "Braunsberg"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , AnyWorker , AnyWorker ]
                  , initEdgeBonus = Nothing
                  }
            , province = Nothing
            , from = "Belgard"
            , to = "Anklam"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , Require Disc , AnyWorker ]
                  , initEdgeBonus = Just BonusBuildInGreen
                  }
            , province = Nothing
            , from = "Malmö"
            , to = "Visby"
            , startBonus = False
            }
        , EdgeBuilder
            { initEdge =
                InitEdge
                  { initEdgeSpots = [ AnyWorker , Require Disc , Require Disc ]
                  , initEdgeBonus = Just BonusGainPrivilege
                  }
            , province = Nothing
            , from = "Stralsund"
            , to = "Malmö"
            , startBonus = False
            }
        ]
    }
