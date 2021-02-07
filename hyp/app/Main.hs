module Main where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import System.Console.GetOpt
import System.Random.TF(newTFGen)

import Common.Basics
import Common.Server
import Common.CallJS
import Common.Interact

import AppTypes
import Action
import Resource

main :: IO ()
main =
  newServer options \opts ->
    do seed <- newTFGen
       pure
         ( BS8.pack $(jsHandlers [ ''OutMsg, ''Update, ''Input
                                 , ''BasicAction ])
         , let ps = map PlayerId (players opts)
           in startGame GameInfo
                 { gPlayers = Set.fromList ps
                 , gState   = initialState seed (useFog opts) ps
                 , gInit    = pure ()
                 , gSave    = \_m -> ""
                 }
                 []
         )



--------------------------------------------------------------------------------
data Options = Options
  { players :: [Text]
  , useFog :: Bool
  }

instance Semigroup Options where
  sOld <> sNew = sNew { players = players sNew ++ players sOld
                      , useFog  = useFog sOld && useFog sNew
                      }

instance Monoid Options where
  mempty = Options { players = [], useFog = True }


options :: [ OptDescr Options ]
options =
  [ Option [] ["player"]
    (ReqArg (\b -> mempty { players = [Text.pack b] }) "NAME")
    "Add a player"
  , Option [] ["no-fog"]
    (NoArg mempty { useFog = False })
    "No fog of war"
  ]
