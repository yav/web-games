module Actions where

import Control.Monad(guard,when)
import qualified Data.Map as Map

import Common.Utils
import Common.Interact
import Common.Field

import Basics
import Stats
import Player
import Question
import Game
import Turn
import Event
import Board

import Actions.Common
import Actions.Place
import Actions.Move
import Actions.Hire
import Actions.Complete
import Actions.Bonus

nextAction :: Interact ()
nextAction =
  do state <- getState
     end   <- checkEndGame
     if end
        then do save
                update (Log EndTurn)
                update (Log StartTurn)
                evLog [ "Game Over" ]
                update EndGame

        else do let normalOpts = tryPlace state ++ tryMove state ++
                                 tryHire state ++ tryCompleteEdge state ++
                                  [ a | b <- enumAll, a <- bonusAction b state ]
                askInputs "Choose action"
                            (tryEndTurn (null normalOpts) state ++ normalOpts)
                nextAction

nextTurn :: Interact ()
nextTurn =
  do state <- getState
     let turn = getField gameTurn state
         curP = currentPlayer turn
         nextPlayerId = playerAfter curP state
         actLvl = getLevel Actions (getField (gamePlayer nextPlayerId) state)
     update (Log EndTurn)
     update (Log StartTurn)
     evLog [ EvPlayer nextPlayerId, "'s turn" ]
     save
     update (NewTurn (newTurn nextPlayerId actLvl))

tryEndTurn :: Bool -> PlayerOptions
tryEndTurn forceEnd state =
  do let turn = getField gameTurn state
     guard (forceEnd ||
            getField actionsDone turn == getField currentActionLimit turn)
     pure (currentPlayer turn :-> ChDone "End Turn", "End turn",
              do replaceTokens
                 nextTurn)

-- XXX: handle case when there is no place for binus token
replaceTokens :: Interact ()
replaceTokens =
  do n <- view (getField gameTokenRemaining)
     ts <- view (getField gameTokens)
     when (n < length ts)
       do p <- view gameCurrentPlayer
          spots <- view (tokenSpots . getField gameBoard)
          let t = head ts
          update (PlacingBonus (Just t))
          ~(ChEdge edgeId) <- choose p "Place a new bonus token"
                                        [ (spot,"Place token here")
                                                              | spot <- spots ]
          update (PlacingBonus Nothing)
          update (EdgeSetBonus edgeId t)
          evLog [ "Placed ", EvBonus t, " on ", EvEdge edgeId Nothing ]
          replaceTokens

checkEndGame :: Interact Bool
checkEndGame =
  do board  <- view (getField gameBoard)
     tokens <- view (getField gameTokenRemaining)
     ps     <- view (Map.elems . getField gamePlayers)
     let endFull   = countFull board >= boardMaxFull board
         endTokens = tokens < 0
         endPlayer = any ((>= 20) . getVP) ps
     pure (endFull || endTokens || endPlayer)



