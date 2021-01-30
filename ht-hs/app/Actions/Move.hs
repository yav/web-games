module Actions.Move where

import Data.Maybe(isNothing)

import Stats
import Player
import Turn

import Actions.Common



tryMove :: PlayerOptions
tryMove state =
  do (turn,playerState) <- startAction state
     let playerId = currentPlayer turn
         limit    = movementLimit (getLevel Movement playerState)

     (ch,h,act) <- normalMovePieces playerId limit False (== playerId)
                            (\fromP toP -> isNothing toP || fromP == toP) state
     pure (ch,h,doAction act)
