module FinalScore where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Common.Field
import Common.Basics
import Common.Bag

import PlayerState
import Action
import Resource
import Geometry
import Tile

type Points = Map Text Int

pointsFromPlayer :: PlayerState -> Points
pointsFromPlayer player =
  Map.fromList
    [ ("Gems", getField playerGems player)
    , ("Ghosts", case getField playerGhosts player of
                   n | n < 1 -> 0
                     | n < 2 -> 1
                     | n < 3 -> 3
                     | n < 4 -> 6
                     | otherwise -> 3 + n)
    , ("Captured", Set.size (getField playerCaptured player))
    , ("Cubes", countCubes)
    , ("Achievements", 2 * Set.size (getField playerAchievements player))
    , ("Technologies",
        sum [ techVP t | t <- Map.elems (getField playerTech player) ])
    ]
  where
  countCubes = sum [ n | b <- Map.elems (getField playerBag player)
                       , (a,n) <- bagToNumList b, a /= Gray ]
             + sum [ 1 | t <- Map.elems (getField playerTech player)
                       , a <- getField techAlts t
                       , (_,r) <- costFullSpots (getField techCost a)
                       , r /= Gray
                       ]


pointFromControl :: Board -> Map PlayerId (Int,Int) -- points, how many
pointFromControl board =
  Map.fromListWith add
    [ ( p
      , ( case tileNumber tile of
            TileNum _ -> if loc == origin then 3 else 2
            _ -> 1
        , 1
        )
      )
    | (loc,tile) <- Map.toList (getField boardMap board)
    , Just p <- [tileControl tile]
    ]
  where
  add (x,y) (a,b) = (x+a,y+b)


