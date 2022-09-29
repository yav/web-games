module AppTypes where

import GHC.Generics(Generic)
import Common.Basics(PlayerId)
import Data.Aeson(ToJSON,FromJSON)

data State = State
  deriving (Generic,ToJSON)

type Finished = State

data Update = Update
  deriving (Generic,ToJSON)

data Input = Input
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

doUpdate   :: Update -> State -> Either Finished State
doUpdate  _ = Right


type StateView = State

playerView :: PlayerId -> State -> StateView
playerView _ = id


type UpdateView = Update

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView _ = id


