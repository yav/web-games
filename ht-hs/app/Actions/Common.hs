module Actions.Common where

import Data.Text(Text)
import qualified Data.Map as Map
import Control.Monad(guard,when)

import Common.Utils
import Common.Interact
import Common.Field

import Basics
import Stats
import Player
import Question
import Game
import Turn
import Board
import Event

type PlayerChoice  = (WithPlayer Choice, Text, Interact ())
type PlayerOptions = Game -> [PlayerChoice]

startAction :: Game -> [(Turn,Player)]
startAction state =
  do let turn = getField gameTurn state
         playerState = getField (gamePlayer (currentPlayer turn)) state
     guard (getField actionsDone turn < getField currentActionLimit turn)
     pure (turn, playerState)

doAction :: Interact () -> Interact ()
doAction act =
  do update (Log StartAction)
     act
     update (ChangeDoneActions 1)
     update (Log EndAction)

evLog :: [EventElement] -> Interact ()
evLog = update . Log . EvSay


doUpgrade :: PlayerId -> Player -> Stat -> Interact ()
doUpgrade playerId player stat =
  do update (Upgrade playerId stat)
     let worker = Worker { owner = playerId, shape = statWorker stat }
     update (ChangeAvailble worker 1)
     evLog [ "Upgraded ", EvStat stat ]
     when (stat == Actions)
       do let lvl = getLevel Actions player
              diff = actionLimit (lvl+1) - actionLimit lvl
          when (diff > 0) $
             update (ChangeActionLimit diff)


placeSpots ::
  WorkerHome ->
  Player ->
  Text ->
  (WorkerType -> [Choice]) ->
  ( [(Choice,Text)], Bool )
placeSpots home playerState help getSpots =
  ( map disambig spots
  , any isAmbig  spots
  )
  where
  workerPref = getWorkerPreference playerState

  disambig xs =
    case xs of
      [a] -> snd a
      _   -> head [ b | (t,b) <- xs, t == workerPref ]

  isAmbig xs =
    case xs of
      _ : _ : _ -> True
      _         -> False

  spots =
    Map.elems $
    Map.fromListWith (++)
    do t <- enumAll
       guard (getWorker home t playerState > 0)
       x <- getSpots t
       key <- case x of
                ChEdgeEmpty e s _  -> [Right (e,s)]
                ChEdgeFull e s _ _ -> [Right (e,s)]
                ChNodeEmpty i _    -> [Left i]
                _ -> []
       pure (key, [(t,(x,help))])



changePref :: Player -> Bool -> [(Choice,Text)]
changePref playerState yes =
  do guard yes
     let otherT = otherType (getWorkerPreference playerState)
         help   = "Change preference to " <> jsKey otherT
     pure (ChSetPreference otherT, help)


normalMovePieces ::
  PlayerId -> Int -> Bool ->
  (PlayerId -> Bool) ->
  (Maybe ProvinceId -> Maybe ProvinceId -> Bool) ->
  Game ->
  [(WithPlayer Choice,Text,Interact())]
normalMovePieces playerId limit stopEarly canPickUp canPutDown =
  \s -> allPickupOpts (getField gameBoard s) 1
  where
  pickSpots board n =
    [ ( playerId :-> ch
      , "Move worker " <> showText n <> "/" <> showText limit
      , doPickUp (n::Int) edgeId spot w
      ) | ch@(ChEdgeFull edgeId spot _ w) <- moveFromSpots board canPickUp
    ]

  allPickupOpts board n =
    case pickSpots board n of
      [] -> []
      opts
        | stopEarly || n > 1 ->
              (playerId :-> ChDone "Done", "No more moves", putDown)
            : opts
        | otherwise -> opts


  pickUp n
    | n > limit = putDown
    | otherwise =
      do board <- view (getField gameBoard)
         case allPickupOpts board n of
           []   -> putDown
           opts -> askInputs "Choose worker to pick up" opts

  doPickUp n edgeId spot w =
    do update (RemoveWorkerFromEdge edgeId spot)
       prov <- view (edgeProvince edgeId . getField gameBoard)
       update (AddWorkerToHand prov w)
       evLog [ "Picked up ", EvWorker w, " from ", EvEdge edgeId (Just spot) ]
       pickUp (n+1)



  putDown =
    do board <- view (getField gameBoard)
       mb    <- view (nextPickedUp . getField gameTurn)
       case mb of
         Nothing -> pure ()
         Just (fromProv,w) ->
           askInputs "Choose location to put down worker"
             [ (playerId :-> ch, "New worker location", doPutDown w edgeId spot)
             | ch@(ChEdgeEmpty edgeId spot _) <-
                         freeSpots board (canPutDown fromProv) (shape w)
             ]

  doPutDown w edgeId spot =
    do update RemoveWorkerFromHand
       update (PlaceWorkerOnEdge edgeId spot w)
       evLog [ "Moved ", EvWorker w, " to ", EvEdge edgeId (Just spot) ]
       putDown






