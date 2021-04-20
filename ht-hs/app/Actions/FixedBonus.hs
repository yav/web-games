module Actions.FixedBonus where

import Control.Monad(when,unless)
import Data.Maybe(isJust)

import Common.Basics
import Common.Field
import Common.Interact

import Actions.Common
import Basics
import Question
import Game
import Board
import Event
import Bonus
import Stats
import Player
import Edge

doFixedBonus :: Bool -> PlayerId -> EdgeId -> FixedBonus -> Interact ()
doFixedBonus early playerId edgeId bonus =
  case bonus of
    BonusPlace2         -> when early $ doPlaceInProvince playerId 1 2
    BonusMove2          -> when (not early) $ bonusMove2 playerId
    BonusGainPrivilege  -> when early $ bonusGainPrivilage playerId
    BonusBuildInGreen   -> when early $ bonusBuildInGreen playerId
    BonusReuse2         -> when early $ bonusReuse2 playerId edgeId


doPlaceInProvince :: PlayerId -> Int -> Int -> Interact ()
doPlaceInProvince playerId placing limit
  | placing > limit = pure ()
  | otherwise =
    do player <- view (getField (gamePlayer playerId))
       board  <- view (getField gameBoard)
       let (opts,ambig) = placeSpots Active player "Place worker"
                                                        (freeSpots board isJust)
           allOpts = (ChDone "Done", "Don't place additional workers")
                   : changePref player ambig ++ opts
       case opts of
         [] -> pure ()
         _  -> do ch <- choose playerId "Choose worker location" allOpts
                  case ch of
                    ChDone {} -> pure()
                    ChSetPreference t ->
                       do let w = Worker { owner = playerId, shape = t }
                          update (SetWorkerPreference w)
                          doPlaceInProvince playerId placing limit
                    ~(ChEdgeEmpty edgeId spot t) ->
                        do let w = Worker { owner = playerId, shape = t }
                           update (ChangeAvailble w (-1))
                           update (PlaceWorkerOnEdge edgeId spot w)
                           evLog ["Placed ", EvWorker w, " on "
                                              , EvEdge edgeId (Just spot)]
                           doPlaceInProvince playerId (placing+1) limit

bonusMove2 :: PlayerId -> Interact ()
bonusMove2 playerId =
  askInputs "Choose workers to move" .
                normalMovePieces playerId 2 True (const True) (==) =<< getState


bonusGainPrivilage :: PlayerId -> Interact ()
bonusGainPrivilage playerId =
  do priv <- view (getLevel Privilege . getField (gamePlayer playerId))
     unless (priv >= maxStat Privilege) $
       update (Upgrade playerId Privilege)

bonusReuse2 :: PlayerId -> EdgeId -> Interact ()
bonusReuse2 playerId edgeId = pickingUp 1
  where
  limit = 2 :: Int
  pickingUp n
    | n > limit = pure ()
    | otherwise =
      do ws <- view (edgeWorkers . getField (gameBoard .> boardEdge edgeId))
         prov <- view (edgeProvince edgeId . getField gameBoard)
         let giveUp = ( playerId :-> ChDone "Done"
                      , "Don't relocating additional workers"
                      , pure ()
                      )
             mkOpt (spot,_,w) =
                ( playerId :-> ChEdgeFull edgeId spot Nothing w
                , "Relocate worker"
                , do update (RemoveWorkerFromEdge edgeId spot)
                     update (AddWorkerToHand prov w)
                     board <- view (getField gameBoard)
                     loc <- choose playerId
                              "Choose where to move worker"
                              [ (ch,"New worker location")
                              | ch <- freeSpots board (== prov) (shape w)
                              ]
                     let ChEdgeEmpty tgtEdge tgtSpot _ = loc
                     update RemoveWorkerFromHand
                     update (PlaceWorkerOnEdge tgtEdge tgtSpot w)
                     pickingUp (n+1)
                )
         askInputs "Choose workers to reuse" (giveUp : map mkOpt ws)

bonusBuildInGreen :: PlayerId -> Interact ()
bonusBuildInGreen playerId =
  do player <- view (getField (gamePlayer playerId))
     board  <- view (getField gameBoard)
     case placeSpots Active player "Place bonus worker" (greenCities board) of
       ([],_) -> pure ()
       (opts,ambig) ->
          do ch <- choose playerId "Choose new office location"
                                 $ (ChDone "Done", "Don't use bonus")
                                   : changePref player ambig ++ opts
             case ch of
               ChDone {} -> pure ()
               ChSetPreference wt ->
                  do let w = Worker { owner = playerId, shape = wt }
                     update (SetWorkerPreference w)
                     bonusBuildInGreen playerId
               ~(ChNodeEmpty nodeId wt) ->
                  do let w = Worker { owner = playerId, shape = wt }
                     update (ChangeAvailble w (-1))
                     update (PlaceWorkerInOffice nodeId w)
                     evLog [ EvWorker w, " established an office in ",
                                                        EvNode nodeId Nothing ]


