module Main(main) where

import Data.Text(Text)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Common.CallJS
import Common.Server
import Common.RNG
import Common.Options

import Actions(nextAction)
import Basics
import Board.Index

import Game
import Event(Event,EventElement)
import Question(Choice)
import Common.Interact

main :: IO ()
main =
  newServer (optionsGetOpt options) \opts ->
    begin =<< case getOptLoad opts of
                Nothing ->
                  do seed <- newRNGSeed
                     pure Save { moves = [], .. }
                Just file ->
                  do txt <- readFile file
                     case reads txt of
                       [(s,"")] -> pure s
                       _ -> fail "Failed to load save"


options :: [Option]
options = [ oBoard ]

oBoard :: Option
getBoard :: Options -> Board
(oBoard,getBoard) = option "board" (Just Base) "Board for the game"

data Board = Base | East | Britannia
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

colors :: [Text]
colors = [ "red", "yellow", "blue", "purple", "green" ]



data Save = Save
  { seed  :: Seed
  , opts  :: Options
  , moves :: [WithPlayer Choice]
  } deriving (Read,Show)

begin :: Save -> IO (ByteString, InteractState)
begin Save { .. } =
  case Map.lookup boardName boards of
    Just board
      | pnum > 0 ->
        do let str = $(jsHandlers [ ''EventElement,
                                    ''OutMsg,
                                    ''GameUpdate, ''Choice, ''Event])
           pure ( cols <> BS8.pack str
                , startGame GameInfo
                              { gPlayers = plays
                              , gState = initialGame rng board plays
                              , gInit = nextAction
                              , gSave = \m -> show Save { moves = m, .. }
                              } moves
                )
        | otherwise -> fail "need at least 1 player"
    Nothing -> fail "invalid board"

  where
  (ps,cols)  = getOptPlayers colors opts
  plays      = Set.fromList ps
  pnum       = length ps
  rng        = seedRNG seed

  boardName =
    Text.pack
    case getBoard opts of
      Base      -> if pnum < 4 then "ht_23" else "ht_45"
      East      -> "east"
      Britannia -> if pnum < 4 then "britannia_23" else "britannia_45"

