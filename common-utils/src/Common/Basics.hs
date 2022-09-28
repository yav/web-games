module Common.Basics where

import Data.Text(Text)
import GHC.Generics
import Data.Aeson (ToJSONKey(..), ToJSON(..),FromJSON,(.=))
import qualified Data.Aeson as JS

import Common.Utils


newtype PlayerId  = PlayerId Text
  deriving (Show,Read,Generic,Eq,Ord,ToJSONKey)

data WithPlayer a = PlayerId :-> a
  deriving (Eq,Ord,Show,Read)

instance Functor WithPlayer where
  fmap f (p :-> q) = p :-> f q

playerAnnot :: WithPlayer a -> PlayerId
playerAnnot (p :-> _) = p

--------------------------------------------------------------------------------

instance ToJSON   PlayerId
instance FromJSON PlayerId

instance JSKey PlayerId where
  jsKey (PlayerId t) = t

instance ToJSON a => ToJSON (WithPlayer a) where
  toJSON (p :-> a) = JS.object [ "player" .= p, "thing" .= a ]

