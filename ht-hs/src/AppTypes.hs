module AppTypes (module AppTypes, doUpdate) where

import Common.Basics(PlayerId)
import Game(Game,GameUpdate,doUpdate,gameIsFinished)
import Question(Choice)

type State      = Game
type StateView  = State
type Update     = GameUpdate
type UpdateView = Update
type Input      = Choice

finalState :: State -> Bool
finalState = gameIsFinished

playerView :: PlayerId -> State -> StateView
playerView _ = id

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView _ = id

