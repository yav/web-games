module Actions.Bonus where

import Control.Monad(guard,unless)

import Common.Utils
import Common.Basics
import Common.Field
import Common.Interact

import Basics
import Question
import Bonus
import Player
import Game
import Event
import Board
import Stats

import Actions.Common


bonusAction :: BonusToken -> PlayerOptions
bonusAction b state =
  do let playerId = gameCurrentPlayer state
         player   = getField (gamePlayer playerId) state
         board    = getField gameBoard state
         opt act  = ( playerId :-> ChBonusToken b, "Use Bonus Token"
                    , do update (Log StartAction)
                         evLog ["Used ", EvBonus b]
                         act :: Interact ()
                         update (UseBonusToken playerId b)
                         update (Log EndAction)
                    )
     guard (b `elem` getBonuses player)
     case b of
       BonusAct3 -> [ opt (update (ChangeActionLimit 3)) ]
       BonusAct4 -> [ opt (update (ChangeActionLimit 4)) ]
       BonusUpgrade ->
         do let stats = [ s | s <- enumAll, getLevel s player < maxStat s ]
            guard (not (null stats))
            pure $ opt $ askInputs "Choose what to upgrade"
                                     [ ( playerId :-> ChUpgrade stat
                                     , "Upgrade " <> jsKey stat
                                     , doUpgrade playerId player stat
                                     ) | stat <- stats
              ]

       BonusSwap ->
         [ ( playerId :-> q
           , "Move back using bonus token"
           , do update (Log StartAction)
                update (UseBonusToken playerId b)
                update (SwapWorkers nodeId spot)
                evLog ["Used ", EvBonus b, " on ", EvNode nodeId (Just spot)]
                update (Log EndAction)
           )
         | q@(ChNodeFull nodeId spot) <- swappableOffices playerId board
         ]

       BonusMove ->
         do let spots = moveFromSpots board (/= playerId)
            guard (not (null spots))
            pure $ opt $ doMove 1 3
         where
         doMove n l
           | n > l = pure ()
           | otherwise =
             do board <- view (getField gameBoard)
                let spots = moveFromSpots board (/= playerId)
                unless (null spots) $
                  askInputs "Choose workers to move"
                              [ ( playerId :-> q
                              , "Pick up " <> showText n <> "/" <> showText l
                              , doPickup n l q
                              ) | q <- spots ]

         doPickup n l ~(ChEdgeFull edgeId spot _ worker) =
           do update (RemoveWorkerFromEdge edgeId spot)
              board <- view (getField gameBoard)
              let prov = edgeProvince edgeId board
              update (AddWorkerToHand prov worker)
              evLog [ "Picked-up ", EvWorker worker, " from "
                    , EvEdge edgeId (Just spot) ]
              let spots = freeSpots board (== prov) (shape worker)
              askInputs "Choose workers to move"
                          [ ( playerId :-> q
                          , "Put down " <> showText n <> "/" <> showText l
                          , doPutDown n l worker q
                          ) | q <- spots ]

         doPutDown n l worker ~(ChEdgeEmpty edgeId spot _) =
           do update RemoveWorkerFromHand
              update (PlaceWorkerOnEdge edgeId spot worker)
              evLog ["Moved ", EvWorker worker, " to ",
                                              EvEdge edgeId (Just spot)]
              doMove (n+1 :: Int) l


       BonusExtra -> []
