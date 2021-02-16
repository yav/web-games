module Layout (setupBoard) where

import Common.Utils(enumAll)
import Common.Field
import Common.RNG(RNG,shuffle)
import Common.Basics(PlayerId)


import Resource
import Geometry
import Tile


setupBoard :: RNG -> Bool -> [(Maybe PlayerId,Maybe Resource)] -> Board
setupBoard rng0 useFog rs = foldr placePlayer core' (startLocs `zip` rs')
  where
  n = length rs

  makeVis = setField tileVisible (not useFog)

  (central : _, rng1) = shuffle (map makeVis centralTiles) rng0
  (peripheral,rng2)   = shuffle (map makeVis peripheralTiles) rng1
  (always,sometimes)  = splitAt 6 peripheral

  -- XXX: shuffle and place tokens + ghosts


  extraSpots          = [ path [W,NW], path [E,SE] ]

  path                = foldl (\p d -> neighbour d p) origin

  center              = placeTile origin central emptyBoard
  addPeripheral (d,t) = placeTile (neighbour d origin) t
  core                = foldr addPeripheral center (zip enumAll always)
  core'
    | n == 5    = foldr (uncurry placeTile) core (zip extraSpots sometimes)
    | otherwise = core

  rs' = rs ++ repeat (Nothing,Nothing)    -- so we can test with 1 player


  placePlayer ((d,p),(pl,r)) = placeStart pl (path p) d r

  startLocs
{-
    - - B P
 A - + - A
P B - -
-}
    | n <= 2  = [ (NE, [W,W,SW]), (SW, [E, E, NE]) ]

{-
          B P
   P A - - A
    B - + -
       - -
        A B
         P
-}
    | n == 3  = [ (E,  [W,W,NW])
                , (SW, [E,NE,NE])
                , (NW, [SW,SE,SE])
                ]
{-
  P A   B P
   B - - A
    - + -
   A - - B
  P B   A P
-}
    | n == 4  = [ (E,  [W,NW,NW])
                , (SW, [E,NE,NE])
                , (W,  [E,SE,SE])
                , (NE, [W,SW,SW])
                ]

{-
      P
     B A
    - - - B P
   A - + - A
  P B - - -
     A B A B
      P   P
-}
    | n == 5  = [ (SE, [NW,NW,NE])
                , (SW, [E,E,NE])
                , (NW, [SE,SE,SE])
                , (NW, [SW,SW,SE])
                , (NE, [W,W,SW])
                ]

{-
      P
 P A B A
  B - - B P
 A - + - A
P B - - B
   A B A P
    P
-}
    | otherwise = [ (E,  [NW,NW,W])
                  , (SE, [NE,NE,NW])
                  , (SW, [E,E,NE])
                  , (W,  [SE,SE,E])
                  , (NW, [SW,SW,SE])
                  , (NE, [W,W,SW])
                  ]
