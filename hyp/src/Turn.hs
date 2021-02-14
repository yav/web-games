module Turn where

import Data.Map(Map)
import GHC.Generics(Generic)
import Data.Aeson(FromJSON,ToJSON)

import Common.Basics
import Common.Field
import Bag

import Action

data Turn = Turn
  { turnPlayer   :: PlayerId
  , _turnReady   :: Bag BasicAction
  , _turnIfs     :: [(BasicAction, [BasicAction])]
  , _turnOrs     :: [(BasicAction,BasicAction)]
  } deriving (Generic,ToJSON)


data Input = XXXInput
  deriving (Eq,Ord,Show,Generic,FromJSON,ToJSON)

declareFields ''Turn

newTurn :: PlayerId -> Turn
newTurn p =
   turnAddAction (Action [Move]) -- XXX
   Turn { turnPlayer  = p
        , _turnReady  = bagEmpty
        , _turnIfs    = []
        , _turnOrs    = []
        }

turnAddAction :: Action -> Turn -> Turn
turnAddAction act =
  case act of
    If x ys   -> updField turnIfs ((x,ys):)
    Or x y    -> updField turnOrs ((x,y):)
    Action as -> \t -> foldr turnAddBasicAction t as

turnAddBasicAction :: BasicAction -> Turn -> Turn
turnAddBasicAction act =
  case act of
    a `Times` n -> \t -> foldr turnAddBasicAction t (replicate n a)
    _           -> updField turnReady (bagAdd act)






