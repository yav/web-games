module AppTypes where

import Common.Basics(PlayerId)
import Common.Field
import qualified Game as App

type State      = App.Game
type StateView  = App.GameView
type Update     = App.Game
type Input      = App.Input

doUpdate   :: Update -> State -> State
doUpdate new _ = new

finalState :: State -> Bool
finalState s = case getField App.gameStatus s of
                 App.InProgress {} -> False
                 _                 -> True

playerView :: PlayerId -> State -> StateView
playerView = App.gameView

type UpdateView = StateView

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView = playerView


