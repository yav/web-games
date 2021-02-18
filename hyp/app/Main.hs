module Main where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BS8 (toStrict)
import Data.List(foldl')
import Data.Map(Map)
import qualified Data.Map as Map
import System.Console.GetOpt
import qualified Data.Aeson as JS

import Common.Basics
import Common.RNG
import Common.Server
import Common.CallJS
import Common.Interact

import AppTypes
import Action
import Play

main :: IO ()
main =
  newServer options \opts ->
    do seed <- newRNG
       let ps = pcolor (makePlayers (players opts))
       pure
         ( jsColors ps
            <> BS8.pack $(jsHandlers [ ''OutMsg, ''Update, ''Input
                                 , ''BasicAction ])
         , startGame GameInfo
                 { gPlayers = Map.keysSet ps
                 , gState   = initialState seed (useFog opts) (Map.keys ps)
                 , gInit    = setup
                 , gSave    = \_m -> ""
                 }
                 []
         )

--------------------------------------------------------------------------------
type Color = Text

-- | default color order
colors :: [Color]
colors = [ "red", "yellow", "blue", "orange", "purple", "green" ]

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
    []       -> doSetColor p "gray" cs -- shouldn't happen

setColor :: PlayerId -> Color -> PColors -> PColors
setColor p c cs =
  case Map.lookup c (usedBy cs) of
    Nothing -> doSetColor p c cs
    Just p1 ->
      case Map.lookup p (pcolor cs) of
        Nothing -> doSetColor p c (pickColor p1 cs)
        Just c1 -> doSetColor p c (doSetColor p1 c1 cs)

makePlayers :: [(String,Maybe String)] -> PColors
makePlayers = foldl' mkPlayer noColors
  where
  noColors = PColors { usedBy = Map.empty, pcolor = Map.empty, free = colors }
  mkPlayer c (p,mb) =
    let pid = PlayerId (Text.pack p)
    in case mb of
         Just co | let co' = Text.pack co
                 , co' `elem` colors -> setColor pid co' c
         _ -> pickColor pid c

jsColors :: Map PlayerId Color -> BS8.ByteString
jsColors mp = BS8.unlines
  [ "const playerColors ="
  , BS8.toStrict (JS.encode mp)
  ]

--------------------------------------------------------------------------------
data Options = Options
  { players :: [(String,Maybe String)]
  , useFog  :: Bool
  }

instance Semigroup Options where
  a <> b = Options { players = players a ++ players b
                   , useFog  = useFog a && useFog b
                   }

instance Monoid Options where
  mempty = Options { players = [], useFog = True }


options :: [ OptDescr Options ]
options =
  [ Option [] ["player"]
    (ReqArg playerOpt "NAME:COLOR")
    "Add a player"
  , Option [] ["no-fog"]
    (NoArg mempty { useFog = False })
    "No fog of war"
  ]

playerOpt :: String -> Options
playerOpt b =
  case break (== ':') b of
    (a,_:c) -> mempty { players = [(a,Just c)] }
    (a,[])  -> mempty { players = [(a,Nothing)] }


