module Main(main) where

import Data.List(isPrefixOf)
import Text.Read(readMaybe)
import Control.Monad(when,unless,forM_)
import System.IO(openFile,IOMode(..))
import Control.Exception(catch,SomeException(..))
import Control.Concurrent(forkIO)
import System.Exit(ExitCode)
import Control.Monad.IO.Class
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map(Map)
import qualified Data.Map as Map

import System.Random.TF(newTFGen)
import System.Random.TF.Instances(randomRs)
import qualified System.Directory as Dir
import System.FilePath((</>))
import System.Process

import Data.IORef(IORef,newIORef,atomicModifyIORef',modifyIORef',readIORef)

import qualified Snap.Http.Server as Snap
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap

sessionDir :: ByteString -> FilePath
sessionDir x = "sessions" </> BS8.unpack x

templates :: FilePath
templates = "."

type Component = Map ByteString [ByteString] -> Either ByteString CreateSession

components :: [ (ByteString, Component) ]
components =
  [ ("ht-hs", simple "ht-hs")
  , ("hyp", simple "hyp")
  ]
  where
  validP p = p `elem` [ "player", "board", "fog", "length" ]
  simple c = \params ->
               do unless (all validP (Map.keys params))
                    $ Left "Invliad parameter"
                  pure CreateSession
                          { component = c
                          , arguments = [ BS8.unpack ("--" <> x <> "=" <> y)
                                        | (x,ys) <- Map.toList params
                                        , y      <- ys
                                        ]
                          }


data State = State
  { ports    :: [Int]
  , sessions :: Map ByteString Session
  }


data Session =
    Running Int (IO ())   -- ^ pause
  | Paused


getPort :: IORef State -> Snap.Snap Int
getPort ref =
  do mbPort <- liftIO $ atomicModifyIORef' ref
                          \state -> case ports state of
                                      []     -> (state, Nothing)
                                      p:more -> (state { ports = more }, Just p)
     case mbPort of
       Nothing -> unhappy "All tables are full at the moment"
       Just p  -> pure p

getSession :: IORef State -> Snap.Snap (ByteString,Session)
getSession ref =
  do params <- Snap.rqParams <$> Snap.getRequest
     mp     <- liftIO (sessions <$> readIORef ref)
     case getParam "session" params of
       [x] | Just s <- Map.lookup x mp -> pure (x,s)
       _ -> unhappy "Unknown session"



newSession :: IORef State -> Component -> Snap.Snap ()
newSession ref checkParams =
  do params <- Snap.rqParams <$> Snap.getRequest
     mb <- case checkParams params of
             Right ok -> liftIO (createSession ref ok)
             Left bad -> unhappy bad
     case mb of
       Nothing -> unhappy "failed to create session"
       Just x  -> Snap.redirect ("/start/" <> x)


getStatus :: IORef State -> Snap.Snap ()
getStatus ref =
  do (name,status) <- getSession ref
     Snap.writeBS "<html><body>"
     let link a msg =
            Snap.writeBS ("<a href=\"/" <> a <> "/" <> name <> "\">"
                            <> msg <> "</a>")
     case status of
       Running port _ ->
         do Snap.writeBS ("Running on port " <> BS8.pack (show port) <> " ")
            link "stop" "Stop"
       Paused ->
         do Snap.writeBS "Paused "
            link "start" "Start"
     Snap.writeBS "</body></html>"


startSession :: IORef State -> Snap.Snap ()
startSession ref =
  do (name,status) <- getSession ref
     case status of
       Paused ->
         do port <- getPort ref
            liftIO (animateSession ref port name)
       Running {} -> pure ()
     Snap.redirect ("/status/" <> name)

stopSession :: IORef State -> Snap.Snap ()
stopSession ref =
  do (name,status) <- getSession ref
     case status of
       Paused -> pure ()
       Running _ stop -> liftIO stop
     Snap.redirect ("/status/" <> name)


main :: IO ()
main =
  do state <- newIORef State { ports = [ 9000 .. 9010 ]
                             , sessions = Map.empty
                             }
     -- XXX: find existing session on startup
     Snap.quickHttpServe $
       Snap.route $
           ("/status/:session", getStatus state)
         : ("/start/:session", startSession state)
         : ("/stop/:session", stopSession state)
         : ("/", Snap.serveDirectory "ui")
         : [ ( "/new/" <> path, newSession state checkParams)
           | (path,checkParams) <- components
           ]

getParam :: ByteString -> Map ByteString [ByteString] -> [ByteString]
getParam = Map.findWithDefault []


unhappy :: ByteString -> Snap.Snap a
unhappy msg =
  do Snap.modifyResponse $ Snap.setResponseStatus 400 "Bad request"
     Snap.writeBS msg
     Snap.finishWith =<< Snap.getResponse

sessionName :: IO ByteString
sessionName = BS8.pack . take 32 . randomRs ('a','z') <$> newTFGen

copy :: FilePath -> FilePath -> IO ()
copy srcDir tgtDir =
  do files <- Dir.listDirectory srcDir
     forM_ files \file ->
       do let srcPath = srcDir </> file
              tgtPath = tgtDir </> file
          isDir <- Dir.doesDirectoryExist srcPath
          if isDir
            then do Dir.createDirectoryIfMissing True tgtPath
                    copy srcPath tgtPath
            else Dir.copyFile srcPath tgtPath

data CreateSession = CreateSession
  { component   :: String
  , arguments   :: [String]
  } deriving (Read,Show)


createSession :: IORef State -> CreateSession -> IO (Maybe ByteString)
createSession ref ci@CreateSession { .. } =
  do session <- sessionName
     let dir = sessionDir session
     there <- Dir.doesDirectoryExist dir
     when there (fail "Session already exists")
     Dir.createDirectoryIfMissing True dir
     do let templateDir = templates </> component
        copy templateDir dir
        let sessionFile = dir </> "session"
        writeFile sessionFile (show ci)
        modifyIORef' ref \s -> s { sessions =
                                      Map.insert session Paused (sessions s) }
        pure (Just session)
       `catch` \e@SomeException {} ->
          do putStrLn $ unlines [ "Failed to create session"
                                , show e
                                ]
             Dir.removeDirectoryRecursive dir
             pure Nothing
  `catch` \SomeException {} -> pure Nothing

latestSave :: FilePath -> IO (Maybe FilePath)
latestSave dir = search Nothing =<< Dir.listDirectory dir
  where
  search mb files =
    case files of
      file : more ->
        do yes <- Dir.doesFileExist (dir </> file)
           if yes then search (isBiggerSave mb file) more
                  else search mb more
      [] -> pure do n <- mb
                    pure (pref ++ show (n :: Int))

  pref = "save_"
  isBiggerSave mb f
    | pref `isPrefixOf` f
    , Just n <- readMaybe (drop (length pref) f) =
      case mb of
        Nothing -> pure n
        Just x  -> if n > x then pure n else mb
    | otherwise = mb


data Status = Started (Maybe Pid) | Finished ExitCode
  deriving Show

animateSession :: IORef State -> Int -> ByteString -> IO ()
animateSession ref port session =
  do let dir   = sessionDir session
         info  = dir </> "session"
     txt <- readFile info `catch` \SomeException{} -> pure ""
     CreateSession { .. } <-
        case readMaybe txt of
          Just ci -> pure ci
          _ -> fail ("Failed to resume session: " ++ show dir)

     mbSave <- latestSave dir

     let stdoutFile = dir </> "stdout"
         stderrFile = dir </> "stderr"
     hstdout <- openFile stdoutFile AppendMode
     hstderr <- openFile stderrFile AppendMode

     let args1 = case mbSave of
                   Nothing -> arguments
                   Just s  -> ["--load=" ++ s]
         args = ("--port=" ++ show port) : args1

     (_,_,_,pid) <- createProcess CreateProcess
                      { cmdspec = RawCommand ("." </> component) args
                      , cwd = Just dir
                      , env = Nothing
                      , std_in = NoStream
                      , std_out = UseHandle hstdout
                      , std_err = UseHandle hstderr
                      , close_fds = True
                      , new_session = False
                      , delegate_ctlc = False

                      , detach_console = False
                      , create_new_console = False
                      , create_group = False
                      , child_group = Nothing
                      , child_user = Nothing
                      , use_process_jobs = False
                      }

     let statusFile = dir </> "status"
     stat <- getPid pid
     writeFile statusFile (show (Started stat))
     _ <- forkIO do ex <- waitForProcess pid
                    modifyIORef' ref \state ->
                                      state { ports = port : ports state }
                    writeFile statusFile (show (Finished ex))
                    modifyIORef' ref \state ->
                      state { sessions = Map.insert session Paused
                                                          (sessions state) }
     modifyIORef' ref \state ->
        state { sessions = Map.insert session
                                (Running port (terminateProcess pid))
                                (sessions state) }

