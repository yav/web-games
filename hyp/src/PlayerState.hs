module PlayerState where

import Data.Map(Map)

import Resource


data PlayerState = PlayerState
  { playerBag :: Map Resource Int
  }
