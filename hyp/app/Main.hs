module Main where

import Data.Text(Text)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set

import Common.Basics
import Common.RNG
import Common.Server
import Common.CallJS
import Common.Interact
import Common.Options

import AppTypes
import Action
import Play

main :: IO ()
main =
  newServer (optionsGetOpt options) \opts' ->
    let opts = opts' <> optionsDefaults options
    in
    begin <$> case getOptLoad opts of
                Just file -> read <$> readFile file
                Nothing   -> do seed <- newRNGSeed
                                pure Save { seed = seed
                                          , moves = []
                                          , opts = opts }

begin :: Save -> (ByteString, InteractState)
begin Save { .. } =
  ( cols
     <> BS8.pack $(jsHandlers [ ''OutMsg, ''Update, ''Input
                          , ''BasicAction ])
  , startGame GameInfo
          { gPlayers = Set.fromList ps
          , gState   = initialState rng (getFog opts)
                                        (fromEnum (getLen opts)) ps
          , gInit    = setup
          , gSave    = \m -> show Save { moves = m, .. }
          }
          moves
  )

  where
  (ps,cols)  = getOptPlayers colors opts
  rng        = seedRNG seed

--------------------------------------------------------------------------------
data Save = Save
  { seed  :: Seed
  , moves :: [WithPlayer Input]
  , opts  :: Options
  } deriving (Read,Show)

--------------------------------------------------------------------------------
-- | default color order
colors :: [Text]
colors = [ "red", "yellow", "blue", "orange", "purple", "green" ]

data GameLen = Short | Medium | Long
  deriving (Eq,Ord,Enum,Bounded,Read,Show)

options :: [Option]
options = [ oFog, oLen ]

(oFog, getFog) = flag "fog" (Just True) "Use for-of-war"
(oLen, getLen) = option "length" (Just Short) "Game length"



