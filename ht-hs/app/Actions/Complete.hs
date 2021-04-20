module Actions.Complete (tryCompleteEdge) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad(guard,msum,forM_,when,unless)
import Data.Text(Text)
import Data.Maybe(maybeToList,isNothing)

import Common.Utils
import Common.Interact
import Common.Field

import Basics
import Stats
import Bonus
import Player
import Board
import Geometry
import Edge
import Node
import Question
import Game
import Turn
import Event

import Actions.Common
import Actions.FixedBonus


tryCompleteEdge :: PlayerOptions
tryCompleteEdge state =
  resolveAmbig
  do (turn,playerState) <- startAction state
     let playerId = currentPlayer turn
         board    = getField gameBoard state
     (edgeId,edgeInfo) <- fullEdgesFor playerId board
     tryJustComplete edgeId playerId ++
       do (nodeId,nodeInfo) <- getEdgeNodes edgeId state
          concat [ tryAnnex edgeId edgeInfo nodeId nodeInfo
                                                          playerId playerState
                 , tryOffice nodeId nodeInfo playerId playerState
                                                              edgeId edgeInfo
                 , tryAction state nodeId nodeInfo edgeId edgeInfo
                                                           playerId playerState
                 ]

type ChoiceWithEdge = (WithPlayer Choice, Text, EdgeId, Interact ())

resolveAmbig :: [ChoiceWithEdge] -> [PlayerChoice]
resolveAmbig = Map.elems . fmap resolve . Map.fromListWith (++) . map toMap
  where
  toMap i@(q,_,_,_) = (q,[i])

  resolve opts =
    case opts of
      [(q,h,_,a)] -> (q,h,a)
      _ ->
         let (q,h,_,_) = head opts
         in (q, h, askInputs "Choose route to complete"
                               [ ( playerAnnot q :-> ChEdge e
                               , "Complete this route"
                               , a
                               ) | (_,_,e,a) <- opts ])



getEdgeNodes :: EdgeId -> Game -> [(NodeId,Node)]
getEdgeNodes edgeId s =
  let board = getField gameBoard s
      (x,y) = geoEdgeNodes edgeId (boardGeometry board)
  in [ (n, getField (boardNode n) board) | n <- [x,y] ]



--------------------------------------------------------------------------------

giveVPs :: EdgeId -> Interact ()
giveVPs edgeId =
  do nodes <- view (getEdgeNodes edgeId)
     forM_ nodes \(_,nodeInfo) ->
       case nodeControlledBy nodeInfo of
         Just playerId ->
           do update (ChangeVP playerId 1)
              evLog [EvPlayer playerId, " gained ", EvInt 1, " VP"]
         Nothing -> pure ()

activateBonus :: Bool -> EdgeId -> PlayerId -> Interact ()
activateBonus early edgeId playerId =
  do ed <- view (getField (gameBoard .> boardEdge edgeId))
     case edgeBonusSpot ed of
       NoBonus -> pure ()
       Bonus token ->
          when early
          do update (EdgeRemoveBonus edgeId)
             update (GainBonusToken playerId token)
             update DrawBonusToken
       FixedBonus bf -> doFixedBonus early playerId edgeId bf

returnWorkers :: EdgeId -> Interact ()
returnWorkers edgeId =
  do workers <- view (edgeWorkers . getField (gameBoard .> boardEdge edgeId))
     forM_ workers \(spotId,_,w) ->
       do update (RemoveWorkerFromEdge edgeId spotId)
          update (ChangeUnavailable w 1)

completeAction :: EdgeId -> PlayerId -> Interact () -> Interact ()
completeAction edgeId playerId act =
  doAction
  do evLog [ "Completed ", EvEdge edgeId Nothing ]
     giveVPs edgeId
     act
     activateBonus True edgeId playerId
     returnWorkers edgeId
     activateBonus False edgeId playerId

--------------------------------------------------------------------------------

-- | Complete an edge without extra benefits
tryJustComplete :: EdgeId -> PlayerId -> [ChoiceWithEdge]
tryJustComplete edgeId playerId =
  [ ( playerId :-> ChEdge edgeId
    , "Complete with NO office/action"
    , edgeId
    , completeAction edgeId playerId (pure ())
    )
  ]

-- | Complete an edge and build and office
tryOffice ::
  NodeId -> Node ->
  PlayerId -> Player ->
  EdgeId -> Edge ->
  [ ChoiceWithEdge ]
tryOffice nodeId nodeInfo playerId playerState edgeId edgeInfo =
  do spot <- maybeToList (nodeNextFree nodeInfo)
     guard (spotPrivilege spot <= getLevel Privilege playerState)
     (edgeSpotId,worker) <- maybeToList
              $ msum $ map (suitableWorkerFor spot) $ edgeWorkers edgeInfo
     pure ( owner worker :-> ChNodeEmpty nodeId (shape worker)
          , "Build office"
          , edgeId
          , completeAction edgeId playerId
            do fullBefore <- view (countFull . getField gameBoard)
               update (RemoveWorkerFromEdge edgeId edgeSpotId)
               update (PlaceWorkerInOffice nodeId worker)
               evLog [ EvWorker worker, " established office in ",
                                                    EvNode nodeId Nothing ]
               let vp = spotVP spot
               when (vp > 0)
                  do update (ChangeVP playerId vp)
                     evLog [ EvPlayer playerId, " gained ", EvInt vp, " VP" ]
               checkCompleteBonusRoute playerId
               fullAfter <- view (countFull . getField gameBoard)
               when (fullAfter > fullBefore) (update (SetFull fullAfter))
          )
  where
  suitableWorkerFor spot (i,_,w)
    | accepts (spotRequires spot) (shape w) = Just (i,w)
    | otherwise = Nothing


-- | Complete an edge and build an annex using a bonus token
tryAnnex ::
  EdgeId -> Edge ->
  NodeId -> Node ->
  PlayerId -> Player ->
  [ChoiceWithEdge]
tryAnnex edgeId edgeInfo nodeId nodeInfo playerId player =
  do guard (nodeAcceptsAnnex nodeInfo && BonusExtra `elem` getBonuses player)
     -- build annex with a cube, unless all workers are discs
     let ws = edgeWorkers edgeInfo
         isCube (_,_,w)  = shape w == Cube
         (spot,_,worker) = case filter isCube ws of
                             t : _ -> t
                             []    -> head ws

     pure ( playerId :-> ChNodeAnnex nodeId (shape worker)
          , "Use bonus to build annex"
          , edgeId
          , completeAction edgeId playerId
            do update (UseBonusToken playerId BonusExtra)
               update (RemoveWorkerFromEdge edgeId spot)
               update (PlaceWorkerInAnnex nodeId worker)
               evLog [ "Used ", EvBonus BonusExtra, " to build ",
                       EvWorker worker, " annex in ", EvNode nodeId Nothing ]
               checkCompleteBonusRoute playerId
          )

checkCompleteBonusRoute :: PlayerId -> Interact ()
checkCompleteBonusRoute playerId =
  do achieved <- view (getField gameCompletedBonusRoute)
     unless (playerId `Set.member` achieved)
       do done <- view (hasRouteBonus playerId . getField gameBoard)
          let pos = Set.size achieved
          when (done && pos < 3)
            do update (AchieveBonusRoute playerId)
               let pts = case pos of
                           0 -> 7
                           1 -> 4
                           2 -> 2
                           _ -> 0
               update (ChangeVP playerId pts)
               (a,b) <- view (boardBonusRoute . getField gameBoard)
               evLog [ "Gained ", EvInt pts, " VP for connecting ",
                       EvNode a Nothing, " with ", EvNode b Nothing ]

-- | Complete an edge and use a special action
tryAction ::
  Game ->
  NodeId -> Node ->
  EdgeId -> Edge ->
  PlayerId -> Player ->
  [ChoiceWithEdge]
tryAction state nodeId nodeInfo edgeId edgeInfo playerId player =
  concatMap actOpts (nodeActions nodeInfo)
  where
  actOpts act =
    case act of
      UpdgradeStat stat
        | getLevel stat player < maxStat stat ->
          [ ( playerId :-> ChNodeUpgrade nodeId stat
            , "Upgrade " <> jsKey stat
            , edgeId
            , completeAction edgeId playerId (doUpgrade playerId player stat)
            )
          ]
        | otherwise -> []

      GainEndGamePoints ->
        do lvl <- [ 1 .. getLevel Privilege player ]
           guard (isNothing (gameEndVPSpot lvl state))

           let ws = edgeWorkers edgeInfo
               isDisc (_,_,w) = shape w == Disc
           case break isDisc ws of
             (_,[]) -> []
             (_,(spot,_,worker) : _) ->
                pure ( playerId :-> ChEndVPSpot lvl
                     , "Gain end game VP"
                     , edgeId
                     , completeAction edgeId playerId
                       do update (RemoveWorkerFromEdge edgeId spot)
                          update (SetEndVPAt lvl worker)
                          evLog [ "Placed ", EvWorker worker, " on "
                                , EvNode nodeId Nothing
                                , " ", EvInt (endVPTrack lvl), " VP"
                                ]
                     )


