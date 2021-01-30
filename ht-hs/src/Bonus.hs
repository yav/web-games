module Bonus where

import qualified Data.Aeson as JS
import GHC.Generics

import Common.Utils(enumAll,jsDeriveKey)

data BonusToken =
    BonusUpgrade
  | BonusSwap
  | BonusMove
  | BonusExtra
  | BonusAct3
  | BonusAct4
  deriving (Eq,Ord,Show,Read,Generic,Enum,Bounded)

-- | Not counting the starting ones
tokenNumber :: BonusToken -> Int
tokenNumber token =
  case token of
    BonusUpgrade -> 2
    BonusAct3    -> 2
    BonusAct4    -> 2
    BonusSwap    -> 2
    BonusMove    -> 1
    BonusExtra   -> 3


startTokens :: [BonusToken]
startTokens = [ BonusExtra, BonusMove, BonusSwap ]

tokenList :: [BonusToken]
tokenList = [ tok | ty <- enumAll, tok <- replicate (tokenNumber ty) ty ]

data FixedBonus =
    BonusPlace2
  | BonusMove2
  | BonusGainPrivilege
  | BonusBuildInGreen
  | BonusReuse2
  deriving (Eq,Ord,Show,Read,Generic)


bonusPoints :: Int -> Int
bonusPoints n
  | n < 1     = 0
  | n == 1    = 1
  | n <= 3    = 3
  | n <= 5    = 6
  | n <= 7    = 10
  | n <= 9    = 15
  | otherwise = 21

--------------------------------------------------------------------------------
instance JS.ToJSON    BonusToken
instance JS.ToJSONKey BonusToken where toJSONKey = jsDeriveKey
instance JS.FromJSON  BonusToken
instance JS.ToJSON    FixedBonus



