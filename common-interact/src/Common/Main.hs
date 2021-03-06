module Common.Main where

import Data.Text(Text)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set

import Common.Basics
import Common.RNG
import Common.Server
import Common.Interact hiding (save)
import Common.Options

import AppTypes

data App = App
  { appOptions      :: [Option]
    -- ^ Command line flags

  , appColors       :: [Text]
    -- ^ Available player colors

  , appJS           :: String
    -- ^ Javascript to be exported via dynamic.js

  , appInitialState :: RNG -> Options -> [PlayerId] -> Either String State
    -- ^ Initial game state

  , appStart        :: Interact ()
    -- ^ Execute this to start the game
  }

data Save = Save
  { seed  :: Seed
  , moves :: [WithPlayer Input]
  , opts  :: Options
  } deriving (Read,Show)



startApp :: App -> IO ()
startApp app =
  newServer (optionsGetOpt (appOptions app)) \opts' ->
    do let fullOpts = opts' <> optionsDefaults (appOptions app)
       save <- case getOptLoad fullOpts of
                 Just file -> read <$> readFile file
                 Nothing   -> do theSeed <- newRNGSeed
                                 pure Save { seed = theSeed
                                           , moves = []
                                           , opts = fullOpts }
       case begin app save of
         Right ok -> pure ok
         Left err -> fail err


begin :: App -> Save -> Either String (ByteString, InteractState)
begin app save =
  do state <- appInitialState app rng (opts save) ps
     pure ( cols <> BS8.pack (appJS app)
          , startGame GameInfo
                  { gPlayers = Set.fromList ps
                  , gState   = state
                  , gInit    = appStart app
                  , gSave    = \m -> show save { moves = m }
                  }
                  (moves save)
          )

  where
  (ps,cols)  = getOptPlayers (appColors app) (opts save)
  rng        = seedRNG (seed save)




