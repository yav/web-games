module Hex where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON,
                    ToJSONKey(..),genericToJSONKey,defaultJSONKeyOptions)
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

data Dir = N | NE | SE | S | SW | NW
  deriving (Generic,ToJSON,FromJSON,Eq,Ord,Bounded,Enum,Show)

data Loc = Loc Int Int
  deriving (Generic,ToJSON,Eq,Ord,Show)

instance ToJSONKey Loc where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

allDirs :: [Dir]
allDirs = [ minBound .. maxBound ]

clocwise :: Int -> Dir -> Dir
clocwise n = toEnum . (`mod` 6) . (+ n) . fromEnum

opposite :: Dir -> Dir
opposite = clocwise 3

move :: Int -> Dir -> Loc -> Loc
move n d (Loc x y) =
  case d of
    N   -> Loc x (y - n)
    S   -> Loc x (y + n)

    SE  -> Loc (x + n) y
    NW  -> Loc (x - n) y

    NE  -> Loc (x + n) (y - n)
    SW  -> Loc (x - n) (y + n)

distance :: Loc -> Loc -> Int
distance (Loc x y) (Loc a b) = div s 2
  where s = abs (x - a) + abs (y - b) + abs ((x+y) - (a+b))

data Edge = Edge Loc Dir
  deriving (Generic,ToJSON,Eq,Ord,Show)

edge :: Loc -> Dir -> Edge
edge l d = Edge l' d'
  where
  (l',d')
    | fromEnum d < 3 = (l,d)
    | otherwise      = (move 1 d l, opposite d)

edgeHexes :: Edge -> (Loc,Loc)
edgeHexes (Edge l d) = (l, move 1 d l)

splitRegion :: Set Loc -> [Edge] -> (Set Loc, Set Loc)
splitRegion ls es = (aRegion, ls `Set.difference` aRegion)
  where
  seprated  = Map.fromList
                [ (x,y)
                | e <- es
                , let (x,y) = edgeHexes e
                , x `Set.member` ls && y `Set.member` ls
                ]
  aRegion   = complete Set.empty (Map.keys seprated)

  neighbours l = [ l1
                 | d <- allDirs
                 , let l1 = move 1 d l
                 , l1 `Set.member` ls
                 , Map.lookup l seprated /= Just l1
                 ]

  complete r todo =
    case todo of
      [] -> r
      a : more
        | a `Set.member` r -> complete r more
        | otherwise -> complete (Set.insert a r) (neighbours a ++ todo)

regionBorder :: Set Loc -> Set Edge
regionBorder xs =
  Set.fromList
    [ edge l d
    | l <- Set.toList xs
    , d <- allDirs
    , let l1 = move 1 d l
    , not (l1 `Set.member` xs)
    ]

allRegionBorders :: [Set Loc] -> Set Edge
allRegionBorders rs =
  Set.unions
    [ Set.intersection bs cs
    | (r1,bs) <- Map.toList rbs
    , (r2,cs) <- Map.toList rbs
    , r1 /= r2
    ]
  where
  rbs = Map.fromList (zip [ 0 :: Int .. ] (map regionBorder rs))



