module Main where

import Common.Main
import AppTypes

main :: IO ()
main = startApp App
  { appOptions = []
  , appColors = [ "red", "green", "blue", "yellow" ]
  , appJS = ""
  , appInitialState = \_rng _opts _ps -> Right State
  , appStart = pure ()
  }
