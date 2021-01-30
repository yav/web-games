module Common.SaveLoad where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Codec.Compression.GZip as GZip


save :: Show a => a -> String
save = LBS.unpack . Base64.encode . GZip.compress . LBS.pack . show

load :: Read a => String -> Maybe a
load bs64 =
  case Base64.decode (LBS.pack bs64) of
    Left _ -> Nothing
    Right bs ->
      case reads (LBS.unpack (GZip.decompress bs)) of
        [(a,"")] -> Just a
        _ -> Nothing

