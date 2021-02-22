module Turn where

import Data.Text(Text)
import Data.List(partition)
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
           | AskIfAction Int
           | AskOrActionLeft Int
           | AskOrActionRight Int
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
    If x ys   -> updField turnIfs (++[(x,ys)])
    Or x y    -> updField turnOrs (++[(x,y)])
    Action as -> \t -> foldr turnAddBasicAction t as

turnAddBasicAction :: BasicAction -> Turn -> Turn
turnAddBasicAction act =
  case act of
    a `Times` n -> \t -> foldr turnAddBasicAction t (replicate n a)
    _           -> updField turnReady (bagChange 1 act)



turnGetIf :: Int -> Turn -> (BasicAction,[BasicAction],Turn)
turnGetIf n t =
  case splitAt n (getField turnIfs t) of
    (as,(x,xs):bs) -> (x,xs, setField turnIfs (as ++ bs) t)
    _              -> error "turnGetIfs: invalid index"


turnGetOr :: Either Int Int -> Turn -> (BasicAction,Turn)
turnGetOr i t =
  case i of
    Left n  | ((a,_),t1) <- getFrom n -> (a,t1)
    Right n | ((_,b),t1) <- getFrom n -> (b,t1)

  where
  getFrom n =
    case splitAt n (getField turnOrs t) of
      (as,b:bs) -> (b, setField turnOrs (as++bs) t)
      _ -> error "turnGetOr: invalid index"


turnAutoExecute :: Turn -> ([BasicAction],Turn)
turnAutoExecute t = ( concatMap rebuild exec
                    , setField turnReady (bagFromNumList stay) t
                    )
  where
  (exec,stay) = partition (autoExecute . fst) (bagToList (getField turnReady t))
  rebuild (a,n) = replicate n a



