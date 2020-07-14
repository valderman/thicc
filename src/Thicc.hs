module Thicc
  ( module C
  , thicc
  , sendStdOut, awaitStdIn, sendPriv
  ) where
import Control.Concurrent (killThread)
import Control.Monad (void, when)
import Data.IORef
import Data.Maybe (isNothing)
import System.Directory
import System.Exit
import System.FilePath (takeDirectory)
import System.IO (hFlush, stdout)
import System.Posix.Files
import System.Posix.Signals
  ( Handler (Catch), signalProcess, installHandler
  , sigTERM, sigINT, sigHUP, sigQUIT
  )
import System.Posix.Types (ProcessID, CMode)
import System.Posix.User (getRealUserID, getUserEntryForName)
import System.Process (ProcessHandle, waitForProcess, terminateProcess)
import Thicc.Actions (dropPrivileges)
import Thicc.Config as C
import Thicc.Daemon
import Thicc.Daemonize as D
import Thicc.Messages
import Thicc.Monad
import Thicc.Pipes
import Thicc.Socket

fileCreationMask :: CMode
fileCreationMask = 0o77

watchProcess :: (Message a, Message b)
              => ProcessHandle
              -> (ReadPipe a, WritePipe a)
              -> (ReadPipe b, WritePipe b)
              -> ThiccM ()
watchProcess process_handle (r2w, w2d) (d2w, w2r) = do
  r2d <- forkIO $ serve pure r2w w2d
  d2r <- forkIO $ serve pure d2w w2r
  io $ do
    void $ waitForProcess process_handle
    killThread r2d
    killThread d2r

restartProcess :: (Message a, Message b)
               => ThiccM ()
               -> (ProcessHandle -> ThiccM ())
               -> WritePipe a
               -> ReadPipe b
               -> ThiccM ()
restartProcess terminate_if_requested set_rest_pid output input = do
  cfg <- getConfig
  mask $ \restore -> do
    (stdi, stdo, proc_handle) <- runWithPipes (restBinary cfg) [show (restPort cfg)]
    set_rest_pid proc_handle
    void $ try $ restore $ watchProcess proc_handle (stdo, output) (input, stdi)
  terminate_if_requested
  restartProcess terminate_if_requested set_rest_pid output input

createTerminationHandler :: ThiccM (ThiccM (), ProcessHandle -> ThiccM (), IO ())
createTerminationHandler = do
  kill_ref <- io $ newIORef False
  pid_ref <- io $ newIORef Nothing
  return
    ( io $ do
        should_die <- readIORef kill_ref
        when should_die exitSuccess
    , io . writeIORef pid_ref . Just
    , do
        writeIORef kill_ref True
        readIORef pid_ref >>= maybe (return ()) terminateProcess
    )

startWatchdog :: (Message a, Message b) => ReadPipe a -> WritePipe b -> ThiccM ProcessID
startWatchdog input output = fork $ do
  dropPrivileges
  (maybe_terminate, set_rest_pid, handle_termination) <- createTerminationHandler
  _ <- io $ installHandler sigHUP (Catch handle_termination) Nothing
  restartProcess maybe_terminate set_rest_pid output input

installExitHandlers :: ThiccM ()
installExitHandlers = do
    cfg <- getConfig
    io $ mapM_ (\s -> installHandler s (Catch $ exitThicc cfg) Nothing) sigs
  where
    sigs = [sigQUIT, sigINT, sigTERM]
    exitThicc cfg = do
      response <- sendPriv (C.privilegedSocket cfg) Quit
      case response of
        Fail msg -> die msg
        _        -> return ()

initConfig :: ThiccM ()
initConfig = do
  cfg <- getConfig
  io $ createDirectoryIfMissing True (availableFileDirectory cfg)
  io $ createDirectoryIfMissing True (composeFileDirectory cfg)
  _ <- io $ setFileCreationMask fileCreationMask
  _ <- try $ io $ removeFile (privilegedSocket cfg)
  _ <- try $ io $ removeDirectory (takeDirectory $ privilegedSocket cfg)
  io $ createDirectory (takeDirectory $ privilegedSocket cfg)
  io $ createDirectoryIfMissing True (keyFileDirectory cfg)
  muser <- try $ io $ getUserEntryForName (restUser cfg)
  when (isNothing muser) $
    error ("Configured REST API user does not exist: " ++ restUser cfg)

startThicc :: ThiccM ()
startThicc = do
  (from_daemon, to_watchdog) <- pipe
  (from_watchdog, to_daemon) <- pipe
  installExitHandlers
  watchdog_pid <- startWatchdog from_daemon to_daemon
  _ <- io $ setFileCreationMask fileCreationMask
  _ <- try $ thiccDaemon from_watchdog to_watchdog
  io $ signalProcess sigHUP watchdog_pid
  cfg <- getConfig
  void $ try $ io $ removeDirectory (takeDirectory $ privilegedSocket cfg)

thicc :: Config -> IO ()
thicc cfg = flip runThiccM cfg $ do
    uid <- io getRealUserID
    when (uid /= 0) $ io $ die "thicc daemon must be started as root"
    initConfig
    run startThicc
  where
    run | C.daemonize cfg = io . D.daemonize . flip runThiccM cfg
        | otherwise       = id

-- | Send the given message over standard output.
sendStdOut :: Message a => a -> IO ()
sendStdOut req = do
  putStrLn (from req)
  hFlush stdout

-- | Wait for a message on standard input.
awaitStdIn :: Message a => IO a
awaitStdIn = do
  ln <- getLine
  case to ln of
    Just x -> return x
    _      -> awaitStdIn