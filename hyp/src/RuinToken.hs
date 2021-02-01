module RuinToken where

import Common.Utils(enumAll)

import Resource
import Action

data TokenType = Bronze | Silver | Gold

startTokens :: TokenType -> Int
startTokens ty =
  case ty of
    Bronze -> 2
    Silver -> 2
    Gold   -> 3

data Token = Token
  { tokenType   :: TokenType
  , tokenAction :: Action
  }

tokenList :: TokenType -> [Action] -> [Token]
tokenList ty as = [ Token { tokenType = ty, tokenAction = a } | a <- as ]

bronzeTokens :: [Token]
bronzeTokens =
  tokenList Bronze $
  concatMap (replicate 2)
    [ Action [ Move 3    ]
    , Action [ Move 2    ]
    , Action [ Develop Same 3 ]
    , Action [ Develop Same 2 ]
    , Action [ Develop Any 2 ]
    , Action [ Gem 1 ]
    , Action [ SwapResource AnyNormal AnyNormal ]
    , Action [ DrawResource 2 ]
    , Action [ Develop Any 1, DrawResource 1 ]
    , Action [ Develop Same 2, DrawResource 1 ]
    ]

silverTokens :: [Token]
silverTokens =
  tokenList Silver $
  concatMap (replicate 2)
    [ Action [ GainResource (Exact r) 1 | r <- enumAll, isNormal r ] ] ++
  concatMap (replicate 4)
    [ Action [ PlaceWorker 1 ]
    , Action [ Gem 1 ]
    , Action [ DrawResource 3 ]
    , Action [ SwapResource (Exact Gray) AnyNormal ]
    , Action [ Develop Same 3 ]
    , Action [ Develop Any  3 ]
    , Action [ Develop Same 2, DrawResource 2 ]
    ]

goldTokens :: [Token]
goldTokens =
  tokenList Gold $
  concatMap (replicate 2)
    [ Action [ Develop Same 4 ]
    , Action [ Gem 2 ]
    , Action [ Gem 1, PlaceWorker 1 ]
    , Action [ GainResource AnyNormal 1 ]
    , Action [ PlaceWorker 1, Develop Any 3 ]
    , If (RemoveWorker 1) [ Gem 3 ]
    ]

