module AppTypes where

import GHC.Generics(Generic)
import Common.Basics(PlayerId)
import Data.Aeson(ToJSON,FromJSON)
import Data.Aeson qualified as JS

data State = State PlayerId Int
  deriving (Generic,ToJSON)

data Update = SetState State
  deriving (Generic,ToJSON)

data Input = Inc | Dec
  deriving (Eq,Ord,Show,Read,Generic)

instance ToJSON Input where
  toJSON = JS.genericToJSON jsInputOpts
  toEncoding = JS.genericToEncoding jsInputOpts

instance FromJSON Input where
  parseJSON = JS.genericParseJSON jsInputOpts

jsInputOpts :: JS.Options
jsInputOpts = JS.defaultOptions
  { JS.allNullaryToStringTag = False
  , JS.tagSingleConstructors = True
  }


doUpdate   :: Update -> State -> State
doUpdate  (SetState s) _ = s

finalState :: State -> Bool
finalState = const False


type StateView = State

playerView :: PlayerId -> State -> StateView
playerView _ = id


type UpdateView = Update

playerUpdateView :: PlayerId -> Update -> UpdateView
playerUpdateView _ = id


