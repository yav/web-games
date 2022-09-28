module Main(main) where

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Common.CallJS
import Common.Options

import Actions(nextAction)
import Board.Index

import Game
import Event(Event,EventElement)
import Question(Choice)
import Common.Interact

main :: IO ()
main = startApp App
  { appOptions = [ oBoard ]
  , appColors  = [ "red", "yellow", "blue", "purple", "green" ]
  , appJS      = $(jsHandlers [ ''EventElement,
                                ''OutMsg,
                                ''GameUpdate, ''Choice, ''Event])
  , appInitialState = \rng opts ps ->
      let pnum = length ps
          boardName =
            Text.pack
            case getBoard opts of
              Base      -> if pnum < 4 then "ht_23" else "ht_45"
              East      -> "east"
              Britannia -> if pnum < 4 then "britannia_23" else "britannia_45"
      in
      case Map.lookup boardName boards of
        Just board
          | pnum > 0  -> Right (initialGame rng board (Set.fromList ps))
          | otherwise -> Left "need at least 1 player"
        Nothing -> Left "invalid board"
  , appStart = nextAction
  }

oBoard :: Option
getBoard :: Options -> Board
(oBoard,getBoard) = option "board" (Just Base) "Board for the game"

data Board = Base | East | Britannia
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

