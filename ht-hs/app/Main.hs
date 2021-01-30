module Main(main) where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.Random.TF(mkSeedUnix,seedTFGen)
import Data.Word(Word64)

import Common.CallJS
import Common.Server

import Actions(nextAction)
import Basics
import Board.Index

import Game
import Event(Event,EventElement)
import Question(Choice)
import Common.Interact

import System.Console.GetOpt

main :: IO ()
main =
  newServer options \opts ->
    if not (null (load opts)) then
      do txt <- readFile (load opts)
         case reads txt of
           [(s,"")] -> begin s
           _ -> fail "Failed to load save"
    else
      do seed <- mkSeedUnix
         let moves = []
             args = board opts : players opts
         begin Save { .. }


data Options = Options
  { load     :: FilePath
  , board    :: String
  , players  :: [String]
  } deriving Show

instance Semigroup Options where
  oNew <> oOld = oNew { players = players oNew ++ players oOld }

instance Monoid Options where
  mempty = Options { load = "", board = "", players = [] }

options :: [ OptDescr Options ]
options =
  [ Option [] ["board"]
    (ReqArg (\b -> mempty { board = b }) "BOARD")
    "Use this board"

  , Option [] ["player"]
    (ReqArg (\b -> mempty { players = [b] }) "NAME")
    "Add a player"

  , Option [] ["load"]
    (ReqArg (\s -> mempty { load = s }) "FILE")
    "Load the given save game"
  ]




data Save = Save
  { seed  :: (Word64, Word64, Word64, Word64)
  , args  :: [String]
  , moves :: [WithPlayer Choice]
  } deriving (Read,Show)

begin :: Save -> IO (ByteString, InteractState)
begin Save { .. } =
  case args of
    b : ps ->
      case Map.lookup (Text.pack b) boards of
        Just board ->
          do let rng = seedTFGen seed
                 mkP = PlayerId . Text.pack
                 players = Set.fromList (map mkP ps)
                 str = $(jsHandlers [ ''EventElement,
                                      ''OutMsg,
                                      ''GameUpdate, ''Choice, ''Event])
             pure ( BS8.pack str
                  , startGame GameInfo
                                { gPlayers = players
                                , gState = initialGame rng board players
                                , gInit = nextAction
                                , gSave = \m -> show Save { moves = m, .. }
                                } moves
                  )
        Nothing -> fail $ unlines $ "unknown board"
                                  : map Text.unpack (Map.keys boards)

    _ -> fail "usage: board_name player1_name player2_name ..."
