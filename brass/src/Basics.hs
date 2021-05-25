-- | Basic types
module Basics where

import Common.Bag

-- | The ages in the game
data Age      = Canal | Rail
  deriving Eq

-- | The game resources
data Resource = Coal | Iron | Beer
  deriving (Eq,Ord)

-- | The cost to do something
data Cost = Cost
  { costMoney    :: Int
  , costResource :: Bag Resource
  }

