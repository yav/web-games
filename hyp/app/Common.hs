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
     update (ChangeWorkers pid (-1))
     update (ChangeUnit pid FreeUnit loc 1)

