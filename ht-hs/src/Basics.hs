module Basics
  ( module Basics
  , module Common.Basics
  ) where

import GHC.Generics
import qualified Data.Aeson as JS
import Common.Utils
import Data.Map(Map)
import Data.Text(Text)

import Common.Basics

type NodeId       = Int      -- ^ Identifies a city
type EdgeId       = Int      -- ^ Identifies a route
type ProvinceId   = Int      -- ^ Identifies a route (Brittania expansion)

type Score        = Map Text (Map PlayerId Int)

data WorkerType   = Cube | Disc
  deriving (Eq,Ord,Show,Read,Generic,Bounded,Enum)

replacementCost :: WorkerType -> Int
replacementCost wt =
  case wt of
    Cube -> 1
    Disc -> 2

otherType :: WorkerType -> WorkerType
otherType ty =
  case ty of
    Cube -> Disc
    Disc -> Cube


data WorkerHome = Active | Passive
  deriving (Eq,Ord)

data RequireWorker = AnyWorker | Require WorkerType
  deriving (Eq,Ord,Show,Read,Generic)

accepts :: RequireWorker -> WorkerType -> Bool
accepts requirement workerType =
  case requirement of
    AnyWorker     -> True
    Require shape -> workerType == shape


data Worker = Worker
  { owner :: PlayerId
  , shape :: WorkerType
  } deriving (Eq,Ord,Show,Read,Generic)




--------------------------------------------------------------------------------

instance JSKey WorkerType where
  jsKey wt =
    case wt of
      Disc -> "Disc"
      Cube -> "Cube"

instance JS.ToJSONKey WorkerType where toJSONKey = jsDeriveKey
instance JS.ToJSON    WorkerType
instance JS.FromJSON  WorkerType

instance JS.ToJSON    Worker
instance JS.FromJSON  Worker




