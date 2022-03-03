module Hex where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON,
                    ToJSONKey(..),genericToJSONKey,defaultJSONKeyOptions)

data Dir = N | NE | SE | S | SW | NW
  deriving (Generic,ToJSON,FromJSON,Eq,Ord,Bounded,Enum)

data Loc = Loc Int Int
  deriving (Generic,ToJSON,Eq,Ord)

instance ToJSONKey Loc where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

clocwise :: Int -> Dir -> Dir
clocwise n = toEnum . (`mod` 6) . (+ n) . fromEnum

move :: Int -> Dir -> Loc -> Loc
move n d (Loc x y) =
  case d of
    N   -> Loc x (y + n)
    S   -> Loc x (y - n)

    SE  -> Loc (x + n) y
    NW  -> Loc (x - n) y

    NE  -> Loc (x + n) (y + n)
    SW  -> Loc (x - n) (y - n)

distance :: Loc -> Loc -> Int
distance (Loc x y) (Loc a b) = div s 2
  where s = abs (x - a) + abs (y - b) + abs ((x+y) - (a+b))
