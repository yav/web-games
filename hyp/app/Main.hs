module Main where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import System.Console.GetOpt

import Common.Basics
import Common.Server
import Common.CallJS
import Common.Interact

import AppTypes
import Action

main :: IO ()
main =
  newServer options \_opts ->
    pure
      ( BS8.pack $(jsHandlers [ ''OutMsg, ''Update, ''Input, ''BasicAction ])
      , let players = [ PlayerId "One" ]
        in startGame GameInfo
              { gPlayers = Set.fromList players
              , gState   = initialState players
              , gInit    = pure ()
              , gSave    = \_m -> ""
              }
              []
      )



--------------------------------------------------------------------------------
data Options = Options

instance Semigroup Options where
  _ <> _ = Options

instance Monoid Options where
  mempty = Options


options :: [ OptDescr Options ]
options = []
