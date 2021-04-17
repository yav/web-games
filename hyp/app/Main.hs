module Main where

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import System.Console.GetOpt
import Control.Monad(when)

import Common.Basics
import Common.RNG
import Common.Server
import Common.CallJS
import Common.Interact
import Common.ColorPicker

import AppTypes
import Action
import Play

main :: IO ()
main =
  newServer options \opts ->
    begin =<< if not (null (load opts))
                 then read <$> readFile (load opts)
                 else do seed <- newRNGSeed
                         pure Save { seed = seed, moves = [], opts = opts }

begin :: Save -> IO (ByteString, InteractState)
begin Save { .. } =
  do when (gameLen opts == 0) $ fail "Need a game length."
     let ps  = pcolor (makePlayers colors (players opts))
         rng = seedRNG seed
     pure
       ( jsColors ps
          <> BS8.pack $(jsHandlers [ ''OutMsg, ''Update, ''Input
                               , ''BasicAction ])
       , startGame GameInfo
               { gPlayers = Map.keysSet ps
               , gState   = initialState rng (useFog opts) (gameLen opts)
                                                            (Map.keys ps)
               , gInit    = setup
               , gSave    = \m -> show Save { moves = m
                                            , opts = opts { load = "" }
                                            , .. }
               }
               moves
       )

--------------------------------------------------------------------------------
data Save = Save
  { seed  :: Seed
  , moves :: [WithPlayer Input]
  , opts  :: Options
  } deriving (Read,Show)

--------------------------------------------------------------------------------
-- | default color order
colors :: [Color]
colors = [ "red", "yellow", "blue", "orange", "purple", "green" ]

--------------------------------------------------------------------------------
data Options = Options
  { players :: [(String,Maybe String)]
  , useFog  :: Bool
  , gameLen :: Int
  , load    :: FilePath
  } deriving (Read,Show)

instance Semigroup Options where
  a <> b = Options { players = players a ++ players b
                   , useFog  = useFog a && useFog b
                   , gameLen = max (gameLen a) (gameLen b)
                   , load    = load a ++ load b
                   }

instance Monoid Options where
  mempty = Options { players = [], useFog = True, gameLen = 0, load = "" }


options :: [ OptDescr Options ]
options =
  [ Option [] ["player"]
    (ReqArg playerOpt "NAME:COLOR")
    "Add a player"
  , Option [] ["no-fog"]
    (NoArg mempty { useFog = False })
    "No fog of war"
  , Option [] ["short"]
    (NoArg mempty { gameLen = 1 })
    "Short game"
  , Option [] ["regular"]
    (NoArg mempty { gameLen = 2 })
    "Regular game"
  , Option [] ["long"]
    (NoArg mempty { gameLen = 3 })
    "Long game"
  , Option [] ["load"]
    (ReqArg (\x -> mempty { load =  x}) "FILE")
    "Load save game"
  ]

playerOpt :: String -> Options
playerOpt b =
  case break (== ':') b of
    (a,_:c) -> mempty { players = [(a,Just c)] }
    (a,[])  -> mempty { players = [(a,Nothing)] }


