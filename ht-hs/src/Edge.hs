module Edge
  ( -- * Construction
    Edge
  , edge
  , InitEdge(..)

    -- * Manipulation
  , edgeSetWorker
  , edgeRemoveWorkers
  , edgeRemoveBonus
  , edgeSetBonus

    -- * Queries
  , edgeWorkers
  , edgeFreeSpots
  , edgeRequires
  , edgeBonusSpot
  , edgeReadyFor
  , BonusSpot(..)
  ) where

import qualified Data.Map as Map
import GHC.Generics

import qualified Data.Aeson as JS
import Data.Aeson ((.=))

import Common.Utils

import Basics
import Bonus

data EdgeSpot = EdgeSpot
  { edgeSpotType    :: RequireWorker
  , edgeSpotWorker  :: Maybe Worker
  } deriving (Show,Read,Generic)

setSpotWorker :: Maybe Worker -> EdgeSpot -> EdgeSpot
setSpotWorker mb = \e -> e { edgeSpotWorker = mb }

edgeSpot :: RequireWorker -> EdgeSpot
edgeSpot w = EdgeSpot { edgeSpotWorker = Nothing, edgeSpotType = w }

-- | Information about the bonus spot associated with an edge.
data BonusSpot =
    FixedBonus FixedBonus
  | Bonus BonusToken
  | NoBonus
    deriving (Eq,Show,Read,Generic)

-- | Information associated with an edge on the map.
data Edge = Edge
  { edgeSpots :: [EdgeSpot]
  , edgeBonus :: BonusSpot
  } deriving (Show,Read,Generic)

-- | Information about the bonus spot on the edge.
edgeBonusSpot :: Edge -> BonusSpot
edgeBonusSpot = edgeBonus

-- | Remove the bonus from this edge.
edgeRemoveBonus :: Edge -> Edge
edgeRemoveBonus ed = case edgeBonus ed of
                       Bonus _ -> ed { edgeBonus = NoBonus }
                       _       -> ed

-- | Place the bonus token in the bonus spot of the edge.
edgeSetBonus :: BonusToken -> Edge -> Edge
edgeSetBonus b ed = case edgeBonus ed of
                      NoBonus -> ed { edgeBonus = Bonus b }
                      _       -> ed

-- | Set the worker on the edge spot with the given index.
edgeSetWorker :: Int -> Maybe Worker -> Edge -> Edge
edgeSetWorker n w e =
  case splitAt n (edgeSpots e) of
    (as,b:bs) -> e { edgeSpots = as ++ b { edgeSpotWorker = w } : bs }
    _         -> e

-- | Get the requiments for the various spots on the edge.
edgeRequires :: Edge -> [RequireWorker]
edgeRequires = map edgeSpotType . edgeSpots

-- | Compute the various types of free spots on the edge.
-- We only return one free spot per requirement type.
edgeFreeSpots :: Edge -> [(RequireWorker,Int)]
edgeFreeSpots = Map.toList . Map.unions . zipWith getFree [ 0 .. ] . edgeSpots
  where
  getFree n spot = case edgeSpotWorker spot of
                     Nothing -> Map.singleton (edgeSpotType spot) n
                     Just _  -> Map.empty

-- | Get all workers on the edge.
edgeWorkers :: Edge -> [(Int, RequireWorker, Worker)]
edgeWorkers e =
  [ (n,edgeSpotType spot, w) | (n,spot) <- zip [0..] (edgeSpots e)
                             , Just w   <- [ edgeSpotWorker spot ]
                             ]

edgeReadyFor :: PlayerId -> Edge -> Bool
edgeReadyFor playerId = all ok . edgeSpots
  where ok spot = case edgeSpotWorker spot of
                    Just worker -> owner worker == playerId
                    Nothing     -> False

-- | Remove all workers from an edge.
edgeRemoveWorkers :: Edge -> Edge
edgeRemoveWorkers ed =
                 ed { edgeSpots = map (setSpotWorker Nothing) (edgeSpots ed) }


-- | Desription of an empty edge
data InitEdge =
  InitEdge
    { initEdgeBonus :: Maybe FixedBonus
    , initEdgeSpots :: [RequireWorker]
    }

-- | A blank edge with the given requirements.
edge :: InitEdge -> Edge
edge InitEdge { initEdgeBonus, initEdgeSpots } =
  Edge
    { edgeSpots = map edgeSpot initEdgeSpots
    , edgeBonus = case initEdgeBonus of
                    Nothing    -> NoBonus
                    Just bonus -> FixedBonus bonus
    }


--------------------------------------------------------------------------------


instance JS.ToJSON Edge where
  toJSON e = JS.object (optionalBonus [ "workers" .= JS.object workerFields ])
     where
     optionalBonus others =
       case edgeBonus e of
         Bonus b -> ("bonus" .= b) : others
         FixedBonus b -> ("bonus" .= b) : others
         _       -> others

     keys = [ 0 :: Int .. ]

     workerFields =
       [ showText i .= w
       | (i, EdgeSpot { edgeSpotWorker = Just w }) <- keys `zip` edgeSpots e
       ]


