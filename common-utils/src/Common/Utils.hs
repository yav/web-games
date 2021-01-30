module Common.Utils where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Control.Monad.ST
import System.Random.TF
import System.Random.TF.Instances
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Data.Aeson (ToJSON(toJSON),(.=))
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import GHC.Generics

showText :: Show a => a -> Text
showText = Text.pack . show

enumAll :: (Bounded a,Enum a) => [a]
enumAll = [ minBound .. maxBound ]

shuffle :: [a] -> TFGen -> ([a],TFGen)
shuffle xs g0
  | null xs = ([],g0)
  | otherwise =
    runST do v <- Vector.unsafeThaw (Vector.fromList xs)
             mutShuffle v g0 (MVector.length v - 1)
  where
  mutShuffle v g i
    | i > 0 =
      case randomR (0,i) g of
        (j,g1) -> do swap v i j
                     mutShuffle v g1 (i-1)

    | otherwise = do v1 <- Vector.unsafeFreeze v
                     pure (Vector.toList v1,g)

  swap v a b =
    do x <- MVector.read v a
       y <- MVector.read v b
       MVector.write v a y
       MVector.write v b x

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


