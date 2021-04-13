{- # Language OverloadedStrings #-}
module Common where

import Control.Monad(when,unless)
import Data.Text(Text)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Common.Basics
import Common.Field
import Common.Interact

import Bag
import Resource
import Tile
import Turn
import Geometry
import PlayerState
import Action
import AppTypes


checkAchievement :: PlayerId -> Interact ()
checkAchievement pid =
  do player <- view (getField (playerState pid))
     let achieve x =
           unless (x `Set.member` getField playerAchievements player)
                  (update (GainAchievement pid x))

     when (getField playerWorkers player == 0) (achieve ManyTroops)
     pnum <- view (length . gameTurnOrder)
     let gemLimit = if pnum > 2 then 12 else 15
     when (getField playerGems player >= gemLimit) (achieve ManyStars)
     when (Map.size (Map.filter techIsAdvanced (getField playerTech player))
              >= 5) (achieve ManyTechs)




doPlaceWorkerOnCapital :: PlayerId -> Interact ()
doPlaceWorkerOnCapital pid =
  do loc <- view (getField (gameBoard .> boardCapital .> mapAt pid))
     doPlaceWorkerOn pid loc

doPlaceWorkerOn :: PlayerId -> Loc -> Interact ()
doPlaceWorkerOn pid loc =
  do update (ChangeWorkers pid (-1))
     update (ChangeUnit pid FreeUnit loc 1)
     checkAchievement pid

doGainGem :: PlayerId -> Interact ()
doGainGem playerId =
  do update (ChangeGems playerId 1)
     checkAchievement playerId



haveInSupply :: Resource -> Interact Bool
haveInSupply r = view ((> 0) . bagContains r . getField gameSupply)

doGainCube :: PlayerId -> Resource -> Interact ()
doGainCube pid r
  | r == Gray = update (ChangeBag pid BagSource r 1)
  | otherwise =
  do have <- haveInSupply r
     when have
       do update (ChangeSupply r (-1))
          update (ChangeBag pid BagSource r 1)

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



data GeneralCubeLoc = InBag BagName | OnTech CubeLoc

removeCubeOpts :: PlayerState -> Maybe ResourceReq -> [ (Input,Text) ]
removeCubeOpts player mbLim = zip (discard ++ ready ++ fromTech) (repeat help)
  where
  bags        = getField playerBag player
  resources b = filter ok $ map fst $ bagToList $ getField (mapAt b) bags
  ready       = map AskReady (resources BagReady)
  discard     = map AskDiscard (resources BagDiscard)
  fromTech    = map AskCubeLoc (techSpots player)
  (ok,techSpots,help) =
    case mbLim of
      Nothing -> (const True, returnSpots, "Return cube")
      Just t  -> (matches t, removeSpots t, "Remove cube")



doRemoveCube ::
  PlayerId -> Maybe ResourceReq ->
  (GeneralCubeLoc -> Resource -> Interact ()) -> Interact ()
doRemoveCube playerId mbLim k =
  do player <- view (getField (playerState playerId))
     mb <- chooseMaybe playerId (removeCubeOpts player mbLim)
     case mb of
       Nothing -> pure ()
       Just act ->
         case act of
           AskCubeLoc loc ->
             case getField (costSpot loc .> spotResource) player of
               Just r ->
                 do update (RemoveCube playerId loc)
                    k (OnTech loc) r
               Nothing -> pure () -- bug

           AskReady r ->
             do update (ChangeBag playerId BagReady   r (-1))
                k (InBag BagReady) r

           ~(AskDiscard r) ->
             do update (ChangeBag playerId BagDiscard r (-1))
                k (InBag BagDiscard) r


looseUpgradeOpts :: PlayerState -> [ (Input, Text) ]
looseUpgradeOpts player =
  [ (AskUpgrade r, "Loose upgrade point")
  | (r,n) <- Map.toList (getField playerDevel player), n > 0
  ]

doLooseUpgrade :: PlayerId -> Interact ()
doLooseUpgrade playerId =
  do player <- view (getField (playerState playerId))
     mb <- chooseMaybe playerId (looseUpgradeOpts player)
     case mb of
       Nothing -> pure ()
       Just ~(AskUpgrade r) -> update (Upgrade playerId r (-1))


looseWorkerOptions :: PlayerId -> Board -> [(Input,Text)]
looseWorkerOptions playerId geo =
  [ (q, "Loose soldier")
  | ent <- Map.toList (getField boardMap geo)
  , q   <- question ent
  ]
  where
  question (loc,tile) =
      [ AskUnit loc playerId | tileHasOutsideUnits      playerId tile] ++
      [ AskCity loc cityId   | cityId <- tileUnitsInCities playerId tile ] ++
      [ AskRuin loc ruinId   | ruinId <- tileUnitsInRuins  playerId tile ]

doLooseWorker :: PlayerId -> Interact ()
doLooseWorker playerId =
  do board <- view (getField gameBoard)
     mb <- chooseMaybe playerId (looseWorkerOptions playerId board)
     case mb of
       Just (AskUnit loc _) ->
         do let tile = getField (tileAt loc) board
                ty   = if tileCountBlocked playerId tile > 0 then BlockedUnit
                                                             else FreeUnit
            doRemoveUnit playerId ty loc
            update (ChangeWorkers playerId 1)

       Just (AskCity loc cityId) ->
         do let tile = getField (tileAt loc) board
                f    = cityAt cityId .> citySpot
            update (ChangeTile loc (setField f Empty tile))
            update (ChangeWorkers playerId 1)

       Just (AskRuin loc ruinId) ->
         do let tile = getField (tileAt loc) board
                f    = ruinAt ruinId .> ruinSpot
            update (ChangeTile loc (setField f Empty tile))
            update (ChangeWorkers playerId 1)

       _ -> pure ()


doRemoveUnit :: PlayerId -> UnitType -> Loc -> Interact ()
doRemoveUnit pid ty loc =
  do update (ChangeUnit pid ty loc (-1))
     mb <- view (shouldUnblock . getField (gameBoard .> tileAt loc))
     case mb of
       Nothing -> pure ()
       Just (pid',n) ->
         do update (ChangeUnit pid' BlockedUnit loc (-n))
            update (ChangeUnit pid' FreeUnit    loc   n)


