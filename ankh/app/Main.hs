module Main where

import Common.Main
import Common.Interact(OutMsg)
import Common.CallJS(jsHandlers)
import AppTypes

import Basics
import Game


main :: IO ()
main = startApp App
  { appOptions = []
  , appColors = [ "red", "green", "blue", "yellow" ]
  , appJS = $(jsHandlers [ ''OutMsg, ''Update, ''Input ])
  , appInitialState = \_rng _opts ps -> Right (newGame (ps `zip` repeat Ra))
  , appStart = pure ()
  }
