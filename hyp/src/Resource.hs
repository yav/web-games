module Resource where

import GHC.Generics(Generic)
import Common.Field(declareFields)

data Resource = Blue | Yellow | Orange | Purple | Red | Green | Gray
  deriving (Eq,Ord,Enum,Bounded,Generic)

data ResourceReq = Exact Resource | AnyNormal

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
  , _spotResource :: Maybe Resource
  }

emptySpot :: ResourceReq -> ResourceSpot
emptySpot r = ResourceSpot { spotRequires = r, _spotResource = Nothing }

declareFields ''ResourceSpot

