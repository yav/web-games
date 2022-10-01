module AppTypes where

import Common.Basics(PlayerId)
import qualified Game as App

type State      = App.Game
type StateView  = App.GameView
type Finished   = State
type Update     = App.Game
type Input      = App.Input

doUpdate   :: Update -> State -> Either Finished State
doUpdate new _ = Right new -- XXX: end

playerView :: PlayerId -> State -> StateView
playerView = App.gameView

type UpdateView = StateView

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView = playerView


