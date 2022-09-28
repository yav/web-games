module Main where

import Common.CallJS(jsHandlers)
import Common.Interact
import Common.Options

import AppTypes
import Action
import Play

main :: IO ()
main = startApp App
  { appOptions = [ oFog, oLen ]
  , appColors = [ "red", "yellow", "blue", "orange", "purple", "green" ]
  , appJS = $(jsHandlers [ ''OutMsg, ''Update, ''Input, ''BasicAction ])
  , appInitialState =  \rng opts ps ->
      Right $ initialState  rng (getFog opts) (fromEnum (getLen opts) + 1) ps
  , appStart = setup
  }

data GameLen = Short | Medium | Long
  deriving (Eq,Ord,Enum,Bounded,Read,Show)

(oFog, getFog) = flag "fog" (Just True) "Use for-of-war"
(oLen, getLen) = option "length" (Just Short) "Game length"



