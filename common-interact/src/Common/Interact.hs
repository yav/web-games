{-# Language TemplateHaskell, OverloadedStrings #-}
-- | A server to play a game.
module Common.Interact
  ( -- * Starting a server
    startApp
  , App(..)

    -- * Interaction
  , Interact

    -- ** User input
  , askInputs
  , choose
  , chooseMaybe

    -- ** Read the state
  , view
  , getState

    -- ** Modify the state
  , update
  , localUpdate
  , localUpdate_

    -- ** Save the game
  , Interact.save
  ) where

import Data.Text(Text)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set

import Common.Basics
import Common.CallJS(jsHandlers)
import Common.RNG
import Common.Server
import Common.InteractImpl hiding (save)
import qualified Common.InteractImpl as Interact
import Common.Options

import AppTypes

-- | Desciption of an application.
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


-- | Start a web server for the given application.
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
     let outMsg = $(jsHandlers [''OutMsg])
     pure ( BS8.unlines [ cols, outMsg, BS8.pack (appJS app) ]
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




