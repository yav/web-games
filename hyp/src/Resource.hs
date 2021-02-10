module Resource where

import Data.Maybe(isNothing)
import GHC.Generics(Generic)
import Data.Aeson(ToJSON,ToJSONKey(..),genericToJSONKey,defaultJSONKeyOptions)

data Resource = Blue | Yellow | Orange | Purple | Red | Green | Gray
  deriving (Eq,Ord,Enum,Bounded,Generic,ToJSON)

instance ToJSONKey Resource where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

data ResourceReq = Exact Resource | AnyNormal
  deriving (Generic,ToJSON)

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
  , spotResource  :: Maybe Resource
  } deriving (Generic,ToJSON)

emptySpot :: ResourceReq -> ResourceSpot
emptySpot r = ResourceSpot { spotRequires = r, spotResource = Nothing }


type ResourceCost = [ ResourceSpot ]

costFreeSpots :: ResourceCost -> [(Int,ResourceReq)]
costFreeSpots c = [ (n,spotRequires s) | (n,s) <- zip [ 0 .. ] c
                                       , isNothing (spotResource s) ]

costFullSpots :: ResourceCost -> [(Int,Resource)]
costFullSpots c = [ (n,r) | (n,s)  <- zip [ 0 .. ] c
                          , Just r <- [ spotResource s ] ]

costSetResource :: Int -> Maybe Resource -> ResourceCost -> ResourceCost
costSetResource n r c =
  case splitAt n c of
    (as,b:bs) -> as ++ b { spotResource = r } : bs
    _ -> c



