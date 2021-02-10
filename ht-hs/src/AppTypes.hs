module AppTypes (module AppTypes, doUpdate) where

import Common.Basics(PlayerId)
import Game(Game,GameFinished,GameUpdate,doUpdate)
import Question(Choice)

type State    = Game
type Finished = GameFinished
type Update   = GameUpdate
type Input    = Choice
type View     = Game

playerView :: PlayerId -> State -> View
playerView _ = id

