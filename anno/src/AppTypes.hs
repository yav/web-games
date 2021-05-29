module AppTypes where

import GHC.Generics(Generic)
import Common.Basics(PlayerId)
import Data.Aeson(ToJSON,FromJSON)

data State = State
  {
  }
  deriving (Generic,ToJSON)

type Finished = State

data Update = Update
  deriving (Generic,ToJSON)

data Input = Input
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

doUpdate   :: Update -> State -> Either Finished State
doUpdate  _ = Right

playerView :: PlayerId -> State -> State
playerView _ = id

playerUpdateView :: PlayerId -> Update -> Update
playerUpdateView _ = id


