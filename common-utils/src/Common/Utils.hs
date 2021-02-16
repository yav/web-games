module Common.Utils where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON(toJSON),(.=))
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import GHC.Generics

showText :: Show a => a -> Text
showText = Text.pack . show

enumAll :: (Bounded a,Enum a) => [a]
enumAll = [ minBound .. maxBound ]

jsTag :: Text -> JS.Pair
jsTag t = "tag" JS..= t

jsTagged :: Text -> [JS.Pair] -> JS.Value
jsTagged t xs = JS.object (jsTag t : xs)

jsCall :: JS.ToJSON a => Text -> [a] -> JS.Value
jsCall f as = jsTagged f [ "args" JS..= as ]

jsCall' :: Text -> JS.Value
jsCall' f = jsTagged f [ "args" JS..= ([] :: [JS.Value]) ]

js :: JS.ToJSON a => a -> JS.Value
js = JS.toJSON

jsParseEnum :: (Bounded a, Enum a, JSKey a) => String -> JS.Value -> JS.Parser a
jsParseEnum lab =
  JS.withText lab \txt ->
  case lookup txt [ (jsKey s, s) | s <- enumAll ] of
    Just a  -> pure a
    Nothing -> fail ("Invalid " ++ lab)

jsEnum :: JSKey a => a -> JS.Value
jsEnum = toJSON . jsKey

jsMap :: (JSKey a, ToJSON b) => Map a b -> JS.Value
jsMap mp = JS.object [ jsKey a .= b | (a,b) <- Map.toList mp ]

class JSKey a where
  jsKey :: a -> Text

instance JSKey Text where
  jsKey = id

instance JSKey Int where
  jsKey = showText


jsDeriveKey :: (Generic a, JS.GToJSONKey (Rep a)) => JS.ToJSONKeyFunction a
-- jsDeriveKey :: (Generic a, JS.GetConName (Rep a)) => JS.ToJSONKeyFunction a
jsDeriveKey = JS.genericToJSONKey JS.defaultJSONKeyOptions


