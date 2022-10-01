module AppTypes where

import Common.Basics(PlayerId)
import Common.Field
import qualified Game as App

type State      = App.Game
type StateView  = App.GameView
type Finished   = State
type Update     = App.Game
type Input      = App.Input

doUpdate   :: Update -> State -> Either Finished State
doUpdate new _ = case getField App.gameStatus new of
                   App.InProgress -> Right new
                   _              -> Left new

playerView :: PlayerId -> State -> StateView
playerView = App.gameView

type UpdateView = StateView

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView = playerView


