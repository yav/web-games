module Actions.Hire where

import qualified Data.Text as Text

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

import Actions.Common


tryHire :: PlayerOptions
tryHire state0 =
  do (turn,playerState) <- startAction state0
     let player     = currentPlayer turn
         limit      = hireLimit (getLevel Hiring playerState)
         limTxt     = case limit of
                        Nothing -> "Hire all"
                        Just l  -> "Hire 1/" <> Text.pack (show l)
         question t = (player :-> ChPassiveWorker t,
                                  limTxt, hireFirst limit t)
     [ question t | t <- enumAll, getWorker Passive t playerState > 0 ]
  where
  hireFirst mbLimit ch =
    doAction
    case mbLimit of
      Nothing    -> hireAll
      Just limit -> hire 1 ch >> doHire 2 limit

  hireAll =
    do (_,cubes,discs) <- getWorkers
       hire cubes Cube
       hire discs Disc

  hire n t =
    do playerId <- view (currentPlayer . getField gameTurn)
       let w = Worker { owner = playerId, shape = t }
       update (ChangeUnavailable w (-n))
       update (ChangeAvailble    w n)
       evLog [ "Hired ", EvInt n, " ", EvWorker w ]

  doHire hiring limit
    | hiring > limit = pure ()
    | otherwise =
      do (playerId,cubes,discs) <- getWorkers
         let todo = 1 + limit - hiring
         if | cubes == 0 -> hire (min todo discs) Disc
            | discs == 0 -> hire (min todo cubes) Cube
            | cubes + discs <= limit -> hireAll
            | otherwise ->
              do let lab = mconcat ["Hire ", showText hiring, "/",
                                                              showText limit ]
                 x <- choose playerId "Hire what type of worker?"
                                  [ (ChPassiveWorker t, lab) | t <- enumAll]
                 let ChPassiveWorker ch = x
                 hire 1 ch
                 doHire (1+hiring) limit

  getWorkers =
    do game <- getState
       let playerId = gameCurrentPlayer game
           player   = getField (gamePlayer playerId) game
           cubes    = getWorker Passive Cube player
           discs    = getWorker Passive Disc player
       pure (playerId,cubes,discs)


