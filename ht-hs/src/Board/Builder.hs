module Board.Builder
  ( module Board.Builder
  , module Node
  , module Edge
  , module Basics
  , module Bonus
  , module Stats
  , module Board
  ) where

import Data.Text(Text)
import Data.Maybe(mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad(msum)

import Basics
import Bonus
import Stats
import Node
import Edge
import Geometry
import Board


data NodeBuilder = NodeBuilder
  { nodeInit :: InitNode
  , provinces :: [Text]
  , gateway :: [Text]
  }

data EdgeBuilder = EdgeBuilder
  { province :: Maybe Text
  , from :: Text
  , to :: Text
  , startBonus :: Bool
  , initEdge :: InitEdge
  }

data BoardBuilder = BoardBuilder
  { nodes       :: [NodeBuilder]
  , edges       :: [EdgeBuilder]
  , maxFull     :: Int
  , name        :: Text
  , bonusRoute  :: (Text,Text)
  }

buildBoard :: BoardBuilder -> Board
buildBoard builder =
  Board
    { boardName = name builder
    , boardMaxFull = maxFull builder
    , _boardNodes = Map.fromList[ (i, node n) | i <- [ 0 .. ] | n <- nodeInits ]
    , _boardEdges = Map.fromList[ (i, edge e) | i <- [ 0 .. ] | e <- edgeInits ]
    , boardGeometry = foldr addCon geoEmpty (zip [ 0 .. ] (edges builder))
    , boardEdgeProvince = Map.fromList
        [ (i, getProvince t) | (i,Just t) <- [ 0 .. ]
                                    `zip` map province (edges builder) ]
    , boardCapital = msum (map isBoardCapital (nodes builder))

    , boardProvinces = foldr addProvs capitals (nodes builder)
    , boardInitialTokens =
      Set.fromList [ i | (i,e) <- [ 0 .. ] `zip` edges builder, startBonus e ]
    , boardBonusRoute =
        case bonusRoute builder of
          (a,b) -> (getNodeId a, getNodeId b)
    }
  where
  nodeInits = map nodeInit (nodes builder)
  edgeInits = map initEdge (edges builder)

  nodeNameToId = Map.fromList
                    [ (initNodeName n,i) | i <- [ 0 .. ] | n <- nodeInits ]
  getNodeId x = nodeNameToId Map.! x

  provinceMap = Map.fromList
              $ (`zip` [ 0 .. ])
              $ Set.toList
              $ Set.fromList
              $ mapMaybe province (edges builder)

  getProvince x = provinceMap Map.! x


  capitals = Map.fromList (mapMaybe isProvinceCapital (nodes builder))

  isProvinceCapital n =
    case gateway n of
      [i] | i `elem` provinces n ->
          Just (getProvince i, Province
                                  { provinceName = i
                                  , provinceCapital =
                                        getNodeId (initNodeName (nodeInit n))
                                  , provinceNodes = Set.empty
                                  })
      _ -> Nothing

  isBoardCapital n =
    case gateway n of
      _ : _ | null (provinces n) -> Just (getNodeId (initNodeName (nodeInit n)))
      _ -> Nothing


  addCon (eid,e) = geoConnect (getNodeId (from e)) eid (getNodeId (to e))

  addProvs n mp = foldr (addProv (getNodeId (initNodeName (nodeInit n))))
                        mp (provinces n)

  addProv nid p =
    Map.adjust
    (\prov -> prov { provinceNodes = Set.insert nid (provinceNodes prov) })
    (getProvince p)


