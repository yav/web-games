module Common.Server
  ( newServer
  , GameInfo(..)
  ) where

import Data.Maybe(fromMaybe)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.IORef(IORef,newIORef,readIORef,atomicModifyIORef')
import Control.Exception(catch,SomeException(..))

import qualified Data.Aeson as JS
import Network.WebSockets(Connection,sendTextData,receiveData)
import qualified Network.WebSockets as WS

import System.FastLogger(Logger,logMsg,newLogger)
import qualified Snap.Http.Server as Snap
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Network.WebSockets.Snap as WS

import Common.Basics
import Common.Interact

import System.Console.GetOpt
--------------------------------------------------------------------------------

data Server = Server
  { serverRef    :: IORef ServerState
  , serverLogger :: Logger
  }

data ServerState = ServerState
  { connected :: Map PlayerId Connection
  , gameState :: InteractState
  }

newServer ::
  Monoid a => [OptDescr a] -> (a -> IO (ByteString,InteractState )) -> IO ()
newServer options k =
  do let toCFG o = Just (Snap.setOther o Snap.emptyConfig)
         opts    = map (Snap.fmapOpt toCFG) options ++
                                          Snap.optDescrs Snap.defaultConfig
     cfg <- Snap.extendedCommandLineConfig opts (<>) Snap.defaultConfig
     (dyn,s) <- k (fromMaybe mempty (Snap.getOther cfg))

     ref <- newIORef ServerState { connected = Map.empty
                                 , gameState = s
                                 }
     logger <- newLogger "-"
     let srv = Server { serverRef = ref, serverLogger = logger }

     Snap.httpServe cfg $
       Snap.route
         [ ("/ws", WS.runWebSocketsSnap \pending ->
                   do conn <- WS.acceptRequest pending
                      WS.withPingThread conn 30 (pure ()) (newClient srv conn)
           )
         , ("/dynamic.js",
              do Snap.modifyResponse
                    (Snap.setHeader "Content-type" "application/javascript")
                 Snap.writeBS dyn)
         , ("/", Snap.serveDirectory "ui")
         ]

serverLog :: Server -> String -> IO ()
serverLog server = logMsg (serverLogger server) . BS8.pack

serverUpate :: Server -> (ServerState -> (ServerState,a)) -> IO a
serverUpate srv = atomicModifyIORef' (serverRef srv)

serverUpate_ :: Server -> (ServerState -> ServerState) -> IO ()
serverUpate_ srv f = serverUpate srv \s -> (f s, ())

serverState :: Server -> IO ServerState
serverState srv = readIORef (serverRef srv)

--------------------------------------------------------------------------------

newClient :: Server -> Connection -> IO ()
newClient srv conn =
  do serverLog srv "New connection requested, waiting for ID"
     who <- PlayerId <$> receiveData conn
     mbErr <- serverUpate srv \state ->
             if Map.member who (connected state)
                then ( state, Just "player already connected")
                -- XXX: check only approved players connect?
                -- (i.e., no spectators)
                else ( state { connected =
                                      Map.insert who conn (connected state) }
                           , Nothing
                           )
     case mbErr of
       Nothing ->
         do serverLog srv ("Player connected: " ++ show who)
            clientLoop srv who conn
       Just err -> disconnect srv conn Nothing err


clientLoop :: Server -> PlayerId -> Connection -> IO ()
clientLoop srv who conn =
  loop `catch` \e -> disconnect srv conn (Just who)
                                         (show (e :: SomeException))
  where
  loop =
    do bytes <- receiveData conn
       case JS.decode bytes of
         Nothing  -> fail "invalid request"
         Just msg -> inMsg srv (who :-> msg) >> loop

disconnect :: Server -> Connection -> Maybe PlayerId -> String -> IO ()
disconnect srv conn mbWho reason =
  do serverLog srv ("Requesting disconnect: " <> reason)
     case mbWho of
       Just who ->
         serverUpate_ srv \state ->
                           state { connected =
                                        Map.delete who (connected state) }
       Nothing -> pure ()
     WS.sendClose conn ("Disconnected" :: Text)
        `catch` \SomeException {} -> pure ()

inMsg :: Server -> WithPlayer PlayerRequest -> IO ()
inMsg srv msg =
  do (msgs,mbSave) <- serverUpate srv \srvState ->
             let (newGameState,msgs,mb) = handleMessage msg (gameState srvState)
             in (srvState { gameState = newGameState }, (msgs,mb))
     case mbSave of
       Just (n,s) -> writeFile ("save_" ++ show n) s
       Nothing -> pure ()
     mapM_ (outMsg srv) msgs

outMsg :: Server -> WithPlayer OutMsg -> IO ()
outMsg srv (tgt :-> msg) =
  do state <- serverState srv
     case Map.lookup tgt (connected state) of
       Just conn ->
        sendTextData conn (JS.encode msg)
          `catch` \e -> disconnect srv conn (Just tgt)
                                            (show (e :: SomeException))
       Nothing   -> pure ()



