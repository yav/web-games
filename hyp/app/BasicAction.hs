module BasicAction where

import Control.Monad(replicateM_,void,unless)
import Data.List(delete)
import qualified Data.Map as Map

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
import Tech

import Common

doBasicAction :: PlayerId -> BasicAction -> Interact ()
doBasicAction playerId ba =
  case ba of
    Move   -> pure ()
    Fly    -> pure ()
    Attack -> pure ()

    CloneWorker         -> doSimple ba $ doCloneWorker playerId
    PlaceWorker         -> doSimple ba $ doPlaceWorker playerId
    RangedAttack        -> todo
    Fortify             -> todo
    Develop ctr         -> doSimple ba $ doUpgrade playerId ctr

    GainTech            -> doSimple ba $ doGainTech playerId True
    DrawResource        -> doSimple ba $ void $ doDrawCube playerId
    ReturnResource      -> doSimple ba $ doReturnResource playerId
    SwapResource r1 r2  -> doSimple ba $ doSwapResource playerId r1 r2
    GainResource r      -> doSimple ba $ doGainResource playerId r
    Spy                 -> todo

    -- these are auto activated so no need to remove
    LooseResource r     -> doRemoveResource playerId r
    Gem                 -> doGainGem playerId
    LooseGem            -> update (ChangeGems playerId (-1))
    LooseDevelop        -> todo
    LooseWorker         -> todo

    Neighbours ba'      -> todo

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
  doRemoveCube playerId Nothing
  \_ r -> update (ChangeBag playerId BagSource r 1)

doRemoveResource :: PlayerId -> ResourceReq -> Interact ()
doRemoveResource playerId req =
  doRemoveCube playerId (Just req)
  \_ r -> update (ChangeSupply r 1)

doSwapResource :: PlayerId -> ResourceReq -> ResourceReq -> Interact ()
doSwapResource playerId inT outT =
  doRemoveCube playerId (Just inT)
  \gloc rIn ->
    do unless (rIn == Gray) (update (ChangeSupply rIn 1))
       rOut <- case outT of
                 Exact r ->
                   do yes <- haveInSupply r
                      pure (if yes then r else rIn)
                 AnyNormal ->
                   do let opt r  = (AskSupply r, "Replace with this")
                      opts <- view (map (opt . fst) . bagToList
                                                    . getField gameSupply)
                      mb <- chooseMaybe playerId opts
                      case mb of
                        Nothing             -> pure rIn
                        Just ~(AskSupply r) ->
                           do update (ChangeSupply r (-1))
                              pure r

       case gloc of
         OnTech loc -> update (PlaceCube playerId loc rOut)
         InBag b    -> update (ChangeBag playerId b rOut 1)

doGainTech :: PlayerId -> Bool -> Interact ()
doGainTech playerId withReset =
  do market <- view (getField gameMarkets)
     let resetOpts d
           | withReset = [ (AskMarketDeck d, "Reset technologies") ]
           | otherwise = []
         optsFor (d,m) =
           resetOpts d ++
           [ (AskMarketItem d n, "Gain tech")
           | (n,_) <- zip [ 0 .. ] (getField marketOffer m)
           ]

     ch <- choose playerId (concatMap optsFor (Map.toList market))
     case ch of
       AskMarketDeck d ->
         case Map.lookup d market of
           Just m1 -> do update (SetMarket d (resetMarket m1))
                         doGainTech playerId False
           Nothing -> pure ()
       ~(AskMarketItem d n) ->
         case Map.lookup d market of
           Just m1 ->
             do let (t,m2) = getMarket n m1
                player <- view (getField (playerState playerId))
                update (AddTech playerId (playerNextTechId player) t)
                update (ChangeBag playerId BagDiscard Gray 1)
                update (SetMarket d m2)
           Nothing -> pure ()

