module Board.Index where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text(Text)

import Board(Board)
import qualified Board.Hansa3       as HT3
import qualified Board.Hansa45      as HT45
import qualified Board.East         as ES
import qualified Board.Britannia3   as BT3
import qualified Board.Britannia45  as BT45

boards :: Map Text Board
boards = Map.fromList
  [ "ht_23"        ~> HT3.board
  , "ht_45"        ~> HT45.board
  , "east"         ~> ES.board
  , "britannia_23" ~> BT3.board
  , "britannia_45" ~> BT45.board
  ]
  where
  (~>) = (,)



