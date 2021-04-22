module Actions.Place where

import qualified Data.Map as Map

import Common.Utils
import Common.Interact
import Common.Field

import Basics
import Player
import Board
import Question
import Game
import Turn
import Event

import Actions.Common

tryPlace :: PlayerOptions
tryPlace state =
  do (turn,playerState) <- startAction state
     let board        = getField gameBoard state
         player       = currentPlayer turn

         gateways     = accessibleProvinces player (usedGateways turn) board
         accessible   = maybe True (`Map.member` gateways)
         gatewayFor edgeId =
                        (`Map.lookup` gateways) =<< edgeProvince edgeId board

         totWorkers   = sum (map (\t -> getWorker Active t playerState) enumAll)
         canReplace w = owner w /= player &&
                        totWorkers > replacementCost (shape w)


         (getFree,ambig1) =
            placeSpots Active playerState "Place worker"
                       (\t -> freeSpots board accessible t)

         (getFull,ambig2) =
           placeSpots Active playerState "Replace worker"
                      (\t -> replaceSpots board accessible t canReplace)

     (ch,help) <- [ (player :-> c,help)
                  | (c,help) <- changePref playerState (ambig1 || ambig2)
                             ++ getFree ++ getFull
                  ]
     pure (ch, help, handleChoice gatewayFor ch)

  where
  handleChoice gatewayFor (pid :-> ch) =
    case ch of
      ChSetPreference t ->
        do let w = Worker { owner = pid, shape = t }
           update (SetWorkerPreference w)

      ChEdgeEmpty edgeId spot workerT ->
        doAction
        do let w = Worker { owner = pid, shape = workerT }
           update (ChangeAvailble w (-1))
           case gatewayFor edgeId of
             Just g -> update (UseGateway g)
             _      -> pure ()
           update (PlaceWorkerOnEdge edgeId spot w)
           evLog ["Placed ", EvWorker w, " on " , EvEdge edgeId (Just spot)]

      ChEdgeFull edgeId spot ~(Just workerT) worker ->
        doAction
        do board <- view (getField gameBoard)
           update (RemoveWorkerFromEdge edgeId spot)
           update (AddWorkerToHand (edgeProvince edgeId board) worker)
           let ourWorker = Worker { owner = pid, shape = workerT }
           update (ChangeAvailble ourWorker (-1))
           update (PlaceWorkerOnEdge edgeId spot ourWorker)
           replaceFee pid 1 (replacementCost (shape worker))
           evLog [ "Replaced ", EvWorker worker, " with ",
                   EvWorker ourWorker, " on ", EvEdge edgeId (Just spot) ]
           otherPlayerMoveAndPlace edgeId worker

      _ -> pure ()

  replaceFee playerId doing total
    | doing > total = pure ()
    | otherwise =
      do playerState <- view (getField (gamePlayer playerId))
         let cubes = getWorker Active Cube playerState
             discs = getWorker Active Disc playerState
             todo  = 1 + total - doing
             disable n t =
               do let w = Worker { owner = playerId, shape = t }
                  update (ChangeAvailble w (-n))
                  update (ChangeUnavailable w n)
                  evLog [ "Retired ", EvInt n, " ", EvWorker w ]

         if | cubes == 0 -> disable todo Disc
            | discs == 0 -> disable todo Cube
            | cubes + discs == todo ->
              do disable cubes Cube
                 disable discs Disc
            | otherwise ->
              do ~(ChActiveWorker ch) <-
                     choose playerId "Choose a worker to retire"
                       [ ( ChActiveWorker t
                         , "Replace cost " <> showText doing <> "/"
                                           <> showText total
                         ) | t <- enumAll ]
                 disable 1 ch
                 replaceFee playerId (doing + 1) total

  placeOpts edgeId workerT =
    do board <- view (getField gameBoard)
       let accessible newEdgeProv =
             case newEdgeProv of
                Nothing -> True
                Just p  -> edgeProvince edgeId board == Just p
       pure (replaceTargets board accessible edgeId workerT)



  otherPlayerMoveAndPlace edgeId worker =
    do tgts <- placeOpts edgeId (shape worker)
       ~(ChEdgeEmpty tgtEdgeId spot _) <-
            choose (owner worker)
              "Choose location for the displaced worker"
              [ (ch, "Move replaced worker") | ch <- tgts ]
       update RemoveWorkerFromHand
       update (PlaceWorkerOnEdge tgtEdgeId spot worker)
       evLog [ "Moved ", EvWorker worker, " to ", EvEdge tgtEdgeId (Just spot) ]
       placeExtra Passive (owner worker) edgeId
                                  1 (replacementCost (shape worker))


  placeExtra home playerId edgeId placing total
    | placing > total = pure ()
    | otherwise =
      do playerState <- view (getField (gamePlayer playerId))
         board       <- view (getField gameBoard)
         let accessible newEdgeProv =
               case newEdgeProv of
                  Nothing -> True
                  Just p  -> edgeProvince edgeId board == Just p

             (opts,ambig) = placeSpots home playerState placeHelp
                                (replaceTargets board accessible edgeId)
             allOpts = (ChDone "Done", "Don't place additional workers")
                     : changePref playerState ambig
                    ++ opts

         case (opts,home) of
           ([],Passive) -> placeExtra Active playerId edgeId placing total
           ([],Active)  -> pure () -- XXX: allow to move
           _ -> do ch <- choose playerId
                                      "Bonus worker placement"
                                      allOpts
                   case ch of
                     ChDone _ -> pure ()

                     ChSetPreference t ->
                       do let w = Worker { owner = playerId, shape = t }
                          update (SetWorkerPreference w)
                          placeExtra home playerId edgeId placing total

                     ~(ChEdgeEmpty eId spot t) ->
                       do let w = Worker { owner = playerId, shape = t }
                          update case home of
                                   Active  -> ChangeAvailble w (-1)
                                   Passive -> ChangeUnavailable w (-1)
                          update (PlaceWorkerOnEdge eId spot w)
                          evLog [ "Placed ", EvWorker w,
                                  " on ", EvEdge eId (Just spot) ]
                          placeExtra home playerId edgeId (placing+1) total


    where
    placeHelp =
      let which = case home of
                    Active  -> "available"
                    Passive -> "unavailable"
      in "Place " <> which <> " bonus worker " <>
                         showText placing <> "/" <> showText total



