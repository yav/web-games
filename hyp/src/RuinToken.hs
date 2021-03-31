module RuinToken where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Aeson(ToJSON(..),(.=))
import qualified Data.Aeson as JS
import GHC.Generics(Generic)

import Common.RNG
import Common.Utils(enumAll)


import Resource
import Action

data TokenType = Bronze | Silver | Gold
  deriving (Eq,Ord,Show,Generic,ToJSON)

startTokens :: TokenType -> Int
startTokens ty =
  case ty of
    Bronze -> 2
    Silver -> 2
    Gold   -> 3

data Token = Token
  { tokenType    :: TokenType
  , tokenAction  :: Action
  , tokenVisible :: Bool
  }

hideToken :: Token -> Token
hideToken t = t { tokenVisible = False }

instance ToJSON Token where
  toJSON t
    | tokenVisible t = JS.object [ "tokenType" .= tokenType t
                                 , "tokenAction" .= tokenAction t
                                 ]
    | otherwise = toJSON (tokenType t)

type RuinTokens = Map TokenType [Token]

shuffleTokens :: RNG -> RuinTokens
shuffleTokens r0 = Map.fromList [ (Bronze,b), (Silver,s), (Gold,g) ]
  where
  (b,r1) = shuffle bronzeTokens r0
  (s,r2) = shuffle silverTokens r1
  (g,_)  = shuffle goldTokens   r2


tokenList :: TokenType -> [Action] -> [Token]
tokenList ty as =
  [ Token { tokenType = ty, tokenAction = a, tokenVisible = True } | a <- as ]

bronzeTokens :: [Token]
bronzeTokens =
  tokenList Bronze $
  concatMap (replicate 2)
    [ Action [ Move `Times` 3 ]
    , Action [ Move `Times` 2 ]
    , Action [ Develop (Same 3) ]
    , Action [ Develop (Same 2) ]
    , Action [ Develop Any `Times` 2 ]
    , Action [ Gem ]
    , Action [ SwapResource AnyNormal AnyNormal ]
    , Action [ DrawResource `Times` 2 ]
    , Action [ Develop Any, DrawResource ]
    , Action [ Develop (Same 2), DrawResource ]
    ]

silverTokens :: [Token]
silverTokens =
  tokenList Silver $
  concatMap (replicate 2)
    [ Action [ GainResource (Exact r) | r <- enumAll, isNormal r ] ] ++
  concatMap (replicate 4)
    [ Action [ PlaceWorker ]
    , Action [ Gem ]
    , Action [ DrawResource `Times` 3 ]
    , Action [ SwapResource (Exact Gray) AnyNormal ]
    , Action [ Develop (Same 3) ]
    , Action [ Develop Any `Times` 3 ]
    , Action [ Develop (Same 2), DrawResource `Times` 2 ]
    ]

goldTokens :: [Token]
goldTokens =
  tokenList Gold $
  concatMap (replicate 2)
    [ Action [ Develop (Same 4) ]
    , Action [ Gem `Times` 2 ]
    , Action [ Gem, PlaceWorker ]
    , Action [ GainResource AnyNormal ]
    , Action [ PlaceWorker, Develop Any `Times` 3 ]
    , If RemoveWorker [ Gem `Times` 3 ]
    ]

