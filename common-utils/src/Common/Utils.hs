module Common.Utils where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON,(.=))
import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as JS
import GHC.Generics

showText :: Show a => a -> Text
showText = Text.pack . show

enumAll :: (Bounded a,Enum a) => [a]
enumAll = [ minBound .. maxBound ]

jsMap :: (JSKey a, ToJSON b) => Map a b -> JS.Value
jsMap mp = JS.object [ JS.fromText (jsKey a) .= b | (a,b) <- Map.toList mp ]

class JSKey a where
  jsKey :: a -> Text

instance JSKey Text where
  jsKey = id

instance JSKey Int where
  jsKey = showText

jsDeriveKey :: (Generic a, JS.GToJSONKey (Rep a)) => JS.ToJSONKeyFunction a
jsDeriveKey = JS.genericToJSONKey JS.defaultJSONKeyOptions


