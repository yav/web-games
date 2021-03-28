module Common where

import Control.Monad(when)

import Common.Basics
import Common.Field
import Common.Interact

import Bag
import Resource
import Tile
import Turn
import Geometry
import PlayerState
import AppTypes

doPlaceWorkerOnCapital :: PlayerId -> Interact ()
doPlaceWorkerOnCapital pid =
  do loc <- view (getField (gameBoard .> boardCapital .> mapAt pid))
     doPlaceWorkerOn pid loc

-- XXX: check for achievement
doPlaceWorkerOn :: PlayerId -> Loc -> Interact ()
doPlaceWorkerOn pid loc =
  do update (ChangeWorkers pid (-1))
     update (ChangeUnit pid FreeUnit loc 1)

-- XXX: check for achievement
doGainGem :: PlayerId -> Interact ()
doGainGem playerId = update (ChangeGems playerId 1)

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

doRemoveCube ::
  PlayerId -> Maybe ResourceReq ->
  (GeneralCubeLoc -> Resource -> Interact ()) -> Interact ()
doRemoveCube playerId mbLim k =
  do player <- view (getField (playerState playerId))
     let bags        = getField playerBag player
         resources b = filter ok $ map fst $ bagToList $ getField (mapAt b) bags
         ready       = map AskReady (resources BagReady)
         discard     = map AskDiscard (resources BagDiscard)
         fromTech    = map AskCubeLoc (techSpots player)
         questions   = zip (discard ++ ready ++ fromTech) (repeat help)
     mb <- chooseMaybe playerId questions
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
  where
  (ok,techSpots,help) =
    case mbLim of
      Nothing -> (const True, returnSpots, "Return cube")
      Just t  -> (matches t, removeSpots t, "Remove cube")


