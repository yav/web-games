module Main where

import Common.Interact
import Common.CallJS(jsHandlers)
import AppTypes

main :: IO ()
main = startApp App
  { appOptions = []
  , appColors = [ "red", "green", "blue", "yellow" ]
  , appJS = $(jsHandlers [ ''Update, ''Input ])
  , appInitialState = \_rng _opts ps ->
      case ps of
        [p] -> Right (State p 0)
        _   -> Left "We need exactly 1 playerm, see --help"
  , appStart = gameLoop
  }

gameLoop :: Interact ()
gameLoop =
  do State p n <- getState
     what <- choose p "What should we do next"
                [ (Inc, "Increment"), (Dec, "Decrement") ]
     update $ SetState
            $ State p
              case what of
                Inc -> n + 1
                Dec -> n - 1
     gameLoop
