module AppTypes (module AppTypes, doUpdate) where

import Common.Basics(PlayerId)
import Game(Game,GameFinished,GameUpdate,doUpdate)
import Question(Choice)

type State      = Game
type StateView  = State
type Finished   = GameFinished
type Update     = GameUpdate
type UpdateView = Update
type Input      = Choice

playerView :: PlayerId -> State -> StateView
playerView _ = id

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView _ = id

