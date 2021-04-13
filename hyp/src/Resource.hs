module Resource where

import Data.Maybe(isNothing)
import GHC.Generics(Generic)
import Data.Aeson(FromJSON,ToJSON,ToJSONKey(..),
                    genericToJSONKey,defaultJSONKeyOptions)

import Common.Field

data Resource = Blue | Yellow | Orange | Purple | Red | Green | Gray
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic,FromJSON,ToJSON)

instance ToJSONKey Resource where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

data ResourceReq = Exact Resource | AnyNormal
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

isNormal :: Resource -> Bool
isNormal r =
  case r of
    Gray -> False
    _    -> True

matches :: ResourceReq -> Resource -> Bool
matches req r =
  case req of
    Exact x   -> x == r
    AnyNormal -> isNormal r

resourceAmount :: Resource -> Int
resourceAmount r
  | isNormal r = 24
  | otherwise  = 36


data ResourceSpot = ResourceSpot
  { spotRequires  :: ResourceReq
  , _spotResource  :: Maybe Resource
  } deriving (Generic,ToJSON)

declareFields ''ResourceSpot

emptySpot :: ResourceReq -> ResourceSpot
emptySpot r = ResourceSpot { spotRequires = r, _spotResource = Nothing }


type ResourceCost = [ ResourceSpot ]

costFreeSpots :: ResourceCost -> [(Int,ResourceReq)]
costFreeSpots c = [ (n,spotRequires s) | (n,s) <- zip [ 0 .. ] c
                                       , isNothing (getField spotResource s) ]

costFullSpots :: ResourceCost -> [(Int,Resource)]
costFullSpots c = [ (n,r) | (n,s)  <- zip [ 0 .. ] c
                          , Just r <- [ getField spotResource s ] ]


