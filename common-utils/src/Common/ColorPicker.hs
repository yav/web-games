module Common.ColorPicker
  ( Color
  , PColors(..)
  , makePlayers
  , jsColors
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Data.Char(toLower)
import Data.List(foldl')

import qualified Data.Aeson as JS

import Common.Basics

type Color = Text

data PColors = PColors
  { usedBy :: Map Color PlayerId
  , pcolor :: Map PlayerId Color
  , free   :: [Color]
  }

doSetColor :: PlayerId -> Color -> PColors -> PColors
doSetColor p c cs = cs { usedBy = Map.insert c p (usedBy cs)
                       , pcolor = Map.insert p c (pcolor cs)
                       }

pickColor :: PlayerId -> PColors -> PColors
pickColor p cs =
  case free cs of
    c : more -> doSetColor p c cs { free = more }
    []       -> cs

setColor :: PlayerId -> Color -> PColors -> PColors
setColor p c cs =
  case Map.lookup c (usedBy cs) of
    Nothing -> doSetColor p c cs
    Just p1 ->
      case Map.lookup p (pcolor cs) of
        Nothing -> doSetColor p c (pickColor p1 cs)
        Just c1 -> doSetColor p c (doSetColor p1 c1 cs)

makePlayers :: [Color] -> [(String,Maybe String)] -> PColors
makePlayers colors = foldl' mkPlayer noColors
  where
  noColors = PColors { usedBy = Map.empty, pcolor = Map.empty, free = colors }
  mkPlayer c (p,mb) =
    let pid = PlayerId (Text.pack p)
    in case mb of
         Just co | let co' = Text.pack (map toLower co)
                 , co' `elem` colors -> setColor pid co' c
         _ -> pickColor pid c

jsColors :: Map PlayerId Color -> BS8.ByteString
jsColors mp = BS8.unlines
  [ "const playerColors ="
  , LBS.toStrict (JS.encode mp)
  ]

