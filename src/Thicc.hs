module Thicc
  ( module C
  , thicc
  , sendStdOut, awaitStdIn, sendPriv
  ) where
import Control.Concurrent.MVar
import Control.Monad (when)
import Data.Maybe (isNothing)
import System.Directory
import System.Exit
import System.FilePath (takeDirectory)
import System.IO (hFlush, stdout)
import System.Posix.Files
import System.Posix.User
import System.Posix.Signals
import System.Process (waitForProcess, terminateProcess)
import Thicc.Actions (dropPrivileges)
import Thicc.Config as C
import Thicc.Daemon
import Thicc.Daemonize as D
import Thicc.Messages
import Thicc.Monad
import Thicc.Pipes
import Thicc.Socket

thicc :: Config -> IO ()
thicc cfg = flip runThiccM cfg $ do
    uid <- io getRealUserID
    when (uid /= 0) $ io $ die "thicc daemon must be started as root"
    initConfig
    run start
  where
    run | C.daemonize cfg = io . D.daemonize . flip runThiccM cfg
        | otherwise       = id

    start = do
      (fromDaemon, toRest) <- pipe
      (fromRest, toDaemon) <- pipe

      rest_pid <- fork $ do
        dropPrivileges
        child <- io $ newMVar Nothing
        let killRest = takeMVar child >>= maybe (return ()) terminateProcess
        _ <- io $ installHandler sigHUP (Catch killRest) Nothing
        forever $ do
          result <- io $ tryTakeMVar child
          when (isNothing result) $ io exitSuccess
          ph <- runWithPipes
            (restBinary cfg)
            [show (restPort cfg)]
            toDaemon
            fromDaemon
          io $ putMVar child (Just ph)
          io $ waitForProcess ph

      io $ mapM_ (\s -> installHandler s (Catch cleanup) Nothing) sigs
      _ <- io $ setFileCreationMask 0o77
      _ <- try $ thiccDaemon fromRest toRest

      _ <- try $ io $ removeDirectory (takeDirectory $ privilegedSocket cfg)
      io $ signalProcess sigHUP rest_pid

    sigs = [sigQUIT, sigINT, sigTERM]

    cleanup = do
      response <- sendPriv (C.privilegedSocket cfg) Quit
      case response of
        Fail msg -> die msg
        _        -> return ()

    initConfig = do
      io $ createDirectoryIfMissing True (availableFileDirectory cfg)
      io $ createDirectoryIfMissing True (composeFileDirectory cfg)
      _ <- io $ setFileCreationMask 0o77
      _ <- try $ io $ removeFile (privilegedSocket cfg)
      _ <- try $ io $ removeDirectory (takeDirectory $ privilegedSocket cfg)
      io $ createDirectory (takeDirectory $ privilegedSocket cfg)
      io $ createDirectoryIfMissing True (keyFileDirectory cfg)
      muser <- try $ io $ getUserEntryForName (restUser cfg)
      when (isNothing muser) $
        error ("Configured REST API user does not exist: " ++ restUser cfg)


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