module Geometry
  ( -- * Building
    Geometry
  , geoEmpty
  , geoConnect

    -- * Queries
  , geoEdgeNodes
  , geoHasPath
  , geoLargestComponent
  , geoNodeEdges
  , geoEdgeNeighbours
  , ConsiderEdge(..)
  ) where

import           Data.IntMap(IntMap)
import qualified Data.IntMap as Map
import qualified Data.Set as Set
import           Data.List(nub,mapAccumL,maximumBy)
import           Data.Either(partitionEithers)

import Basics

-- | A geometry desribes the relation between nodes and edges.
data Geometry = Geometry
  { nodeNeighbours :: IntMap {-NodeId-} [(EdgeId,NodeId)]
  , edgeNeighbours :: IntMap {-EdgeId-} (NodeId,NodeId)
  } deriving (Read,Show)

-- | A geometry with no nodes or edges.
geoEmpty :: Geometry
geoEmpty = Geometry
  { nodeNeighbours = Map.empty
  , edgeNeighbours = Map.empty
  }

-- | Connect two nodes with an edge.
geoConnect :: NodeId -> EdgeId -> NodeId -> Geometry -> Geometry
geoConnect from via to Geometry { nodeNeighbours, edgeNeighbours } =
  Geometry { nodeNeighbours = Map.insertWith (++) from [(via,to)]
                            $ Map.insertWith (++) to   [(via,from)]
                              nodeNeighbours
           , edgeNeighbours = Map.insert via (from,to) edgeNeighbours
           }

-- | Get the nodes associated with an edge.
-- Assumes the edge is in the graph.
geoEdgeNodes :: EdgeId -> Geometry -> (NodeId,NodeId)
geoEdgeNodes edgeId Geometry { edgeNeighbours } = edgeNeighbours Map.! edgeId

-- | Edges connects to the given node
geoNodeEdges :: NodeId -> Geometry -> [EdgeId]
geoNodeEdges nodeId geo = map fst (geoNodeNeighbours nodeId geo)

geoNodeNeighbours :: NodeId -> Geometry -> [(EdgeId,NodeId)]
geoNodeNeighbours nodeId Geometry { nodeNeighbours } =
  Map.findWithDefault [] nodeId nodeNeighbours

-- | How to treat edges when searching for neighbouring edges.
data ConsiderEdge =
    EdgeUsable [Int] -- ^ We look for such edges
  | EdgeFull         -- ^ We may follow these edges
  | EdgeDisabled     -- ^ These edges are to be ignored
  deriving Eq


-- | Find closest usable neighbouring edges, without using disabled edges.
-- Edges would be disabled if they are in a province that is not accessible.
geoEdgeNeighbours ::
  Geometry -> (EdgeId -> ConsiderEdge) -> EdgeId -> [(EdgeId,[Int])]
geoEdgeNeighbours geo consider edgeId =
  case Map.lookup edgeId (edgeNeighbours geo) of
    Just (a,b) -> search (Set.singleton edgeId) [a,b]
    Nothing    -> []
  where
  search visited cities =
    let opts = [ case status of
                   EdgeUsable xs -> Right (via,xs)
                   _             -> Left (via,to)
               | city <- cities
               , (via,to) <- geoNodeNeighbours city geo
               , let status = consider via
               , not (via `Set.member` visited) && status /= EdgeDisabled
               ]
    in case partitionEithers opts of
         ([],[]) -> []
         (others,[]) ->
            let (newVisited,newCities) = unzip others
            in search (foldr Set.insert visited newVisited) newCities
         (_,es) -> nub es


-- | Check if there is a path from one node to another, only using the
-- selected nodes.
geoHasPath :: Geometry -> (NodeId -> Bool) -> NodeId -> NodeId -> Bool
geoHasPath geo usable from to = search Set.empty [from]
  where
  search visited todo =
    case todo of
      [] -> False
      nodeId : more
        | not (nodeId `Set.member` visited) && usable nodeId ->
          nodeId == to ||
          search (Set.insert nodeId visited)
                 (map snd (geoNodeNeighbours nodeId geo) ++ more)
        | otherwise -> search visited more


type Reps = IntMap NodeId

-- | Compute the size of the largest connected component, when only
-- considering the nodes selected by the predicate.
geoLargestComponent :: Geometry -> (NodeId -> Bool) -> Int
geoLargestComponent Geometry { edgeNeighbours, nodeNeighbours } belongs =
     largest
   $ Map.toList
   $ Map.fromListWith (+)
   $ snd
   $ mapAccumL count classes
   $ Map.keys nodeNeighbours

  where
  find :: NodeId -> Reps -> (NodeId,Reps)
  find r reps =
    case Map.lookup r reps of
      Just r1 -> let (r2,reps1) = find r1 reps
                 in (r2,Map.insert r r2 reps1)
      Nothing -> (r,reps)

  union :: NodeId -> NodeId -> Reps -> Reps
  union x y reps =
    let (rx,reps1) = find x reps
        (ry,reps2) = find y reps1
    in if rx == ry then reps2 else Map.insert rx ry reps2

  maybeUnion :: (NodeId,NodeId) -> Reps -> Reps
  maybeUnion (n1,n2) reps =
    if belongs n1 && belongs n2
      then union n1 n2 reps
      else reps

  classes :: Reps
  classes = foldr maybeUnion Map.empty (Map.elems edgeNeighbours)

  count :: Reps -> NodeId -> (Reps,(NodeId,Int))
  count reps n = (reps1, (r, if belongs n then 1 else 0))
    where (r,reps1) = find n reps

  cmp (_,x) (_,y) = compare x y

  largest xs = if null xs then 0 else snd (maximumBy cmp xs)


