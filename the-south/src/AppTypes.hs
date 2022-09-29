module AppTypes where

import GHC.Generics(Generic)
import Common.Basics(PlayerId)
import Data.Aeson(ToJSON,FromJSON)
import qualified Game as App

type State      = App.Game
type StateView  = App.GameView
type Finished   = State

data Update = Update
  deriving (Generic,ToJSON)

data Input = Input
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

doUpdate   :: Update -> State -> Either Finished State
doUpdate  _ = Right

playerView :: PlayerId -> State -> StateView
playerView = App.gameView

type UpdateView = Update

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView _ = id


