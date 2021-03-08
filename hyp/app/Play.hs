module Play where

import Data.Text(Text)
import Control.Monad(forM_,replicateM_)
import qualified Data.Map as Map
import Data.Maybe(isJust)

import Common.Basics
import Common.Utils(enumAll)
import Common.Field
import Common.Interact

import Bag
import Turn
import AppTypes
import Action
import PlayerState
import Resource
import Geometry
import Tile

import BasicAction
import Common


setup :: Interact ()
setup =
  do state <- getState
     forM_ (gameTurnOrder state) \p ->
       do sequence_ [ replicateM_ 3 (doGainCube p c) | c <- enumAll ] -- XXX: test
          replicateM_ 3 (doDrawCube p)
     startTurn


startTurn :: Interact ()
startTurn =
  do -- XXX: start of turn actions + remove fortify + refill to 3 if needed
     state <- getState
     let playerId = turnPlayer (getField gameTurn state)
     let n = countWorkers playerId (getField gameBoard state)
     replicateM_ (3 - n) (doPlaceWorkerOnCapital playerId)
     takeTurn

endGame :: Interact ()
endGame = pure ()

endTurn :: Interact ()
endTurn =
  do state <- getState
     let curP = turnPlayer (getField gameTurn state)
         newP = case dropWhile (/= curP) (gameTurnOrder state) of
                  _ : a : _ -> a
                  _     -> head (gameTurnOrder state)
     case getField gameEndOn state of
       Just p | p == newP -> endGame
       _ -> update (SetTurn (newTurn newP))
     startTurn

takeTurn :: Interact ()
takeTurn =
  do state <- getState
     let opts = actEndTurn   state ++
                actPlaceCube state ++
                actUseAction state ++
                actEnterCity state
     askInputs opts

type Opts = State -> [ (WithPlayer Input, Text, Interact ()) ]


actPlaceCube :: Opts
actPlaceCube state =
  [ ( playerId :-> AskCubeLoc spot
    , "Place a cube"
    , do r <- askResouce spot
         update (ChangeBag playerId BagReady r (-1))
         update (PlaceCube playerId spot r)
         checkGainBenefit playerId spot
         takeTurn
    ) | spot <- placeSpots player
  ]
  where
  (playerId,player) = currentPlayer state

  askResouce spot =
     case spotRequires (getField (costSpot spot) player) of
       Exact r -> pure r
       AnyNormal ->
         case opts of
           [r] -> pure r
           _   -> do ~(AskReady r) <- choose playerId
                                [ (AskReady r, "Place on wild") | r <- opts ]
                     pure r
         where
         avail = getField (playerBag .> mapAt BagReady) player
         opts  = filter (/= Gray) (map fst (bagToList avail))


actEnterCity :: Opts
actEnterCity state =
  [ ( playerId :-> AskCity loc cityId
    , "Enter city"
    , do let city = getField (tileAt loc .> cityAt cityId) board
         update (ChangeUnit playerId unit loc (-1))
         update (SetCity loc cityId (Occupied playerId))
         doGainBenefit playerId (cityActions city)
         takeTurn
    ) | (loc,cityId,unit) <- enterCityLocs playerId board
  ]
  where
  (playerId,_) = currentPlayer state
  board        = getField gameBoard state


-- XXX: move as fly
-- XXX: move fly
actMove :: Opts
actMove state =
  [ ( playerId :-> AskUnit from
    , "Move unit"
    , doMove from tos
    )
  | (from,tos) <- moveLocs playerId movePts board
  ]
  where
  (playerId,_)  = currentPlayer state
  board         = getField gameBoard state
  ready         = getField (gameTurn .> turnReady) state
  movePts       = bagContains Move ready

  doMove :: Loc -> [(Int,Loc)] -> Interact ()
  doMove from tos =
    do (cost,to) <-
          case tos of
               [t] -> pure t
               _ -> do -- XXX: mark from
                       ~(AskMap loc (Times Move n)) <- choose playerId
                          [ (AskMap to (Times Move cost)
                            , "Move to here") | (cost,to) <- tos ]
                       pure (n,loc)

       turn <- view (getField gameTurn)
       update (SetTurn (turnRemoveReadyN cost Move turn))
       update (ChangeUnit playerId FreeUnit from (-1))
       tileTo <- view (getField (gameBoard .> tileAt to))
       let unit = if tileHasOpponents playerId tileTo
                                          then LockedUnit else FreeUnit
       update (ChangeUnit playerId unit to 1)
       takeTurn


actEndTurn :: Opts
actEndTurn state =
  [ ( playerId :-> AskButton "End Turn"
    , "End Turn"
    , do discardReady
         drawNew
         endTurn
    )
  ]
  where
  (playerId,player) = currentPlayer state

  discardReady =
    forM_ (bagToList (getField (playerBag .> mapAt BagReady) player)) \(r,n) ->
      replicateM_ n
      do update (ChangeBag playerId BagReady   r (-1))
         update (ChangeBag playerId BagDiscard r 1)

  drawNew =
    do have <- doDrawCube playerId
       if have
          then replicateM_ 2 (doDrawCube playerId)
          else doReset playerId


actUseAction :: Opts
actUseAction state =
  [ ( playerId :-> AskReadyAction b
    , "Use acton"
    , performBasicAction playerId b
    )
  | b <- map fst (bagToList (getField turnReady turn))
      -- XXX: not all actions are activated though the action menu
      -- (e.g., attack by clicking on the map, same for move, fly)

  ] ++
  [ ( playerId :-> AskIfAction n
    , "Use action"
    , performIfAction playerId n
    )
  | n <- zipWith const [ 0.. ] (getField turnIfs turn)
  -- XXX: Only enabled
  ] ++
  [ ( playerId :-> AskOrActionLeft n
    , "Use action"
    , performOrAction playerId (Left n)
    ) | n <- ors
  ] ++
  [ ( playerId :-> AskOrActionRight n
    , "Use action"
    , performOrAction playerId (Right n)
    ) | n <- ors
  ]
  where
  (playerId,_) = currentPlayer state
  turn         = getField gameTurn state
  ors          = zipWith const [ 0.. ] (getField turnOrs turn)


performBasicAction :: PlayerId -> BasicAction -> Interact ()
performBasicAction playerId b =
  do doBasicAction playerId b
     takeTurn

performIfAction :: PlayerId -> Int -> Interact ()
performIfAction playerId n =
  do (x,xs,turn) <- turnGetIf n . getField gameTurn <$> getState
     update (SetTurn turn)
     doGainBenefit playerId (Action [x])
     doGainBenefit playerId (Action xs)
     takeTurn

performOrAction :: PlayerId -> Either Int Int -> Interact ()
performOrAction playerId which =
  do (x,turn) <- turnGetOr which . getField gameTurn <$> getState
     update (SetTurn turn)
     doGainBenefit playerId (Action [x])
     takeTurn


--------------------------------------------------------------------------------


doReset :: PlayerId -> Interact ()
doReset playerId =
  do player <- getField (playerState playerId) <$> getState

     -- discards to bag
     let discarded = bagToList (getField (playerBag .> mapAt BagDiscard) player)
     forM_ discarded \(r,n) ->
        replicateM_ n
        do update (ChangeBag playerId BagDiscard r (-1))
           update (ChangeBag playerId BagSource r 1)

     -- from non-continus cards
     let ok b = case b of
                  Continuous {} -> False
                  OneTime {}    -> True
     forM_ (Map.toList (getField playerTech player)) \(tid,t) ->
       forM_ (fullSpots ok tid t) \(spot,r) ->
         do update (RemoveCube playerId spot)
            update (ChangeBag playerId BagSource r 1)

     -- XXX: ask for cont.
     -- XXX: reactivate map

     replicateM_ 3 (doDrawCube playerId)


doGainCube :: PlayerId -> Resource -> Interact ()
doGainCube pid r = update (ChangeBag pid BagSource r 1)

doDrawCube :: PlayerId -> Interact Bool
doDrawCube playerId =
  do state <- getState
     let player = getField (playerState playerId) state
     case cubeToDraw player of
       Nothing -> pure False
       Just (r,p1) ->
         do localUpdate_ (setField (playerState playerId) p1)
            update (ChangeBag playerId BagSource r (-1))
            update (ChangeBag playerId BagReady  r ( 1))
            pure True

checkGainBenefit :: PlayerId -> CubeLoc -> Interact ()
checkGainBenefit playerId lastCube =
  do state <- getState
     let player = getField (playerState playerId) state
         alt = getField (techAltFor lastCube) player
     case techBenefit alt of
       OneTime a
         | all (isJust . getField spotResource) (getField techCost alt) ->
           doGainBenefit playerId a
       _ -> pure ()

doGainBenefit :: PlayerId -> Action -> Interact ()
doGainBenefit playerId a =
  do state <- getState
     let player = getField (playerState playerId) state
         a'     = foldr contModifyAction a (continuousBenefits player)
         (now,t1) = turnAutoExecute $ turnAddAction a' $ getField gameTurn state
     update (SetTurn t1)
     mapM_ (doBasicAction playerId) now
                        -- here we assume the order does not matter

