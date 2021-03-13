module Common where

import Common.Basics
import Common.Field
import Common.Interact

import Tile
import Geometry
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
