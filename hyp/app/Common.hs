module Common where

import Control.Monad(when)

import Common.Basics
import Common.Field
import Common.Interact

import Bag
import Resource
import Tile
import Geometry
import PlayerState
import Tech
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


doGainCube :: PlayerId -> Resource -> Interact ()
doGainCube pid r
  | r == Gray = update (ChangeBag pid BagSource r 1)
  | otherwise =
  do n <- view (bagContains r . getField gameSupply)
     when (n > 0)
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

