module AppTypes (module AppTypes, doUpdate) where

import Game(Game,GameFinished,GameUpdate,doUpdate)
import Question(Choice)

type State    = Game
type Finished = GameFinished
type Update   = GameUpdate
type Input    = Choice

