module AppTypes where

import GHC.Generics(Generic)
import Common.Basics(PlayerId)
import Data.Aeson(ToJSON,FromJSON)
import Game

type State = Game
type StateView = State
type UpdateView = Update

data Update = Update
  deriving (Generic,ToJSON)

data Input = Input
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

doUpdate   :: Update -> State -> State
doUpdate _ = id

playerView :: PlayerId -> State -> StateView
playerView _ = id

finalState :: State -> Bool
finalState = const False -- XXX

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView _ = id


