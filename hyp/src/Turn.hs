module Turn where

import Data.Text(Text)
import Data.Map(Map)
import GHC.Generics(Generic)
import Data.Aeson(FromJSON,ToJSON)

import Common.Basics
import Common.Field
import Bag

import Resource
import Action
import PlayerState

data Turn = Turn
  { turnPlayer   :: PlayerId
  , _turnReady   :: Bag BasicAction
  , _turnIfs     :: [(BasicAction, [BasicAction])]
  , _turnOrs     :: [(BasicAction,BasicAction)]
  } deriving (Generic,ToJSON)


data Input = AskCubeLoc CubeLoc
           | AskReady Resource
           | AskButton Text
           | AskReadyAction  BasicAction
           | AskUpgrade Resource
  deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)


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
    _           -> updField turnReady (bagChange 1 act)






