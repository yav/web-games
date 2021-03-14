module BasicAction where

import Control.Monad(replicateM_,void)
import Data.List(delete)

import Common.Basics
import Common.Field
import Common.Utils
import Common.Interact

import Bag
import Resource
import Geometry
import Action
import Turn
import PlayerState
import AppTypes

import Common

doBasicAction :: PlayerId -> BasicAction -> Interact ()
doBasicAction playerId ba =
  case ba of
    Move -> pure ()
    Fly -> pure ()
    Attack -> pure ()

    CloneWorker -> doSimple ba $ doCloneWorker playerId
    PlaceWorker -> doSimple ba $ doPlaceWorker playerId
    RangedAttack -> todo
    Fortify -> todo
    Develop ctr -> doSimple ba $ doUpgrade playerId ctr

    GainTech -> todo
    DrawResource -> doSimple ba $ void $ doDrawCube playerId
    ReturnResource -> doSimple ba $ doReturnResource playerId
    SwapResource _ _ -> todo
    GainResource r -> doSimple ba $ doGainResource playerId r
    Spy -> todo

    -- these are auto activated so no need to remove
    LooseResource _ -> todo
    Gem -> doGainGem playerId
    LooseGem -> update (ChangeGems playerId (-1))
    LooseDevelop -> todo
    RemoveWorker -> todo
    Neighbours ba' -> todo

    Times ba' n -> replicateM_ n (doBasicAction playerId ba') -- hm

  where
  todo = pure ()


doSimple :: BasicAction -> Interact () -> Interact ()
doSimple ba m =
  do m
     t <- view (getField gameTurn)
     update (SetTurn (turnRemoveReady ba t))


doCloneWorker :: PlayerId -> Interact ()
doCloneWorker playerId =
  do board <- view (getField gameBoard)
     workers <- view (getField (playerState playerId .> playerWorkers))
     let locs = cloneLocs playerId board
     case locs of
       _ : _ | workers > 0 -> askWhere locs
       _ -> pure ()
  where
  askWhere locs =
    do l <- case locs of
              [l] -> pure l
              _   -> do ~(AskMap l _) <- choose playerId
                            [ (AskMap l CloneWorker,"Place unite") | l <- locs ]
                        pure l
       doPlaceWorkerOn playerId l

doPlaceWorker :: PlayerId -> Interact ()
doPlaceWorker playerId =
  do cont <- view (continuousBenefits . getField (playerState playerId))
     if not (null [ () | UseWorkerAsClone <- cont ])
       then doCloneWorker playerId
       else doPlaceWorkerOnCapital playerId

doUpgrade :: PlayerId -> DevelopConstratint -> Interact ()
doUpgrade playerId ctr =
  case ctr of
    Same n      -> upgrade1 nonGray n >> pure ()
    Any         -> upgrade1 nonGray 1 >> pure ()
    Different n -> upgradeDiff (filter (/= Gray) enumAll) n

  where
  nonGray = filter (/= Gray) enumAll

  upgrade1 opts n =
    do ~(AskUpgrade r) <-
          choose playerId [ (AskUpgrade r, "Upgrade resource") | r <- opts ]
       have <- view (getField (playerState playerId .> playerDevel .> mapAt r))
       let n' = min (6 - have) n
       update (Upgrade playerId r n')
       pure r

  upgradeDiff opts n
    | not (null opts) && n > 0 =
      do r <- upgrade1 opts 1
         upgradeDiff (delete r opts) (n-1)
    | otherwise = pure ()


doGainResource :: PlayerId -> ResourceReq -> Interact ()
doGainResource playerId req =
  do sup <- view (getField gameSupply)
     case req of
      Exact r -> doGainCube playerId r
      AnyNormal ->
        case map fst (bagToList sup) of
          []  -> pure ()
          [r] -> doGainCube playerId r
          rs  ->
             do ~(AskSupply r) <-
                    choose playerId [ (AskSupply r, "Gain cube") | r <- rs ]
                doGainCube playerId r

doReturnResource :: PlayerId -> Interact ()
doReturnResource playerId =
  do player <- view (getField (playerState playerId))
     let bags        = getField playerBag player
         resources b = map fst $ bagToList $ getField (mapAt b) bags
         ready       = map AskReady (resources BagReady)
         discard     = map AskDiscard (resources BagDiscard)
         fromTech    = map AskCubeLoc (returnSpots player)
         questions   = zip (discard ++ ready ++ fromTech)
                           (repeat "Return resource")
     mb <- chooseMaybe playerId questions
     case mb of
       Nothing -> pure ()
       Just act ->
         case act of
           AskCubeLoc loc ->
             case getField (costSpot loc .> spotResource) player of
               Just r ->
                 do update (RemoveCube playerId loc)
                    update (ChangeBag playerId BagSource r 1)
               Nothing -> pure () -- bug

           AskReady r ->
             do update (ChangeBag playerId BagReady   r (-1))
                update (ChangeBag playerId BagSource  r 1)

           ~(AskDiscard r) ->
             do update (ChangeBag playerId BagDiscard r (-1))
                update (ChangeBag playerId BagSource  r 1)

