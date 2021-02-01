module Geometry where

data Dir = NE | E | SE | SW | W | NW
  deriving (Eq,Ord,Show,Enum,Bounded)

toCart :: Dir -> Int -> (Int,Int)
toCart dir n =
  case dir of
    NE -> (0,n)
    E  -> (n,0)
    SE -> (n,-n)
    SW -> (0,-n)
    W  -> (-n,0)
    NW -> (-n,n)


  
