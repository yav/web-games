-- | Can use this to check propoertis of actions
module Properties where

import qualified Data.Map as Map

import Common.Field

import Action
import Tech
import Tile
import RuinToken

allTech :: [Tech]
allTech = deck1 ++ deck2 ++ deck3 ++ deck4

allTokens :: [Token]
allTokens = bronzeTokens ++ silverTokens ++ goldTokens

allTiles :: [Tile]
allTiles = startTiles ++ peripheralTiles ++ centralTiles

allCities :: [City]
allCities = [ c | t <- allTiles, c <- Map.elems (getField tileCities t) ]

allActions :: [Action]
allActions =
  map tokenAction allTokens ++
  map cityActions allCities ++
  [ act
  | t <- allTech, a <- getField techAlts t, OneTime act <- [ techBenefit a ]
  ]

