module Main where

import Common.Interact
import Common.CallJS(jsHandlers)
import AppTypes
import Game(newGame)

main :: IO ()
main = startApp App
  { appOptions = []
  , appColors = [ "red", "blue" ]
  , appJS = $(jsHandlers [ ''Update, ''Input ])
  , appInitialState = \rng _opts ps ->
      case ps of
        [p1,p2] -> Right (newGame rng p1 p2)
        _       -> Left "This game needs exactly 2 players"
  , appStart = pure ()
  }
