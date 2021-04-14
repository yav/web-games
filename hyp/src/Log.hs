module Log where

import Data.Text(Text)
import qualified Data.Text as Text
import GHC.Generics
import Data.String

import Data.Aeson(ToJSON)

import Common.Basics

import Resource
import Geometry
import Tile
import PlayerState

type LogEvent = [LogWord]

data LogWord =
    LogText Text
  | LogPlayer PlayerId
  | LogNewTurn (Maybe PlayerId)
  | LogCube Resource
  | LogUpgrade Resource Int
  | LogHex Loc
  | LogCity Loc CityId
  | LogRuin Loc RuinId
  | LogAchievement Achievement
  deriving (Generic,ToJSON)

instance IsString LogWord where
  fromString = LogText . Text.pack
