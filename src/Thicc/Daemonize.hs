module Thicc.Daemonize (daemonize) where
import Control.Monad (void)
import System.Exit
import System.Posix

daemonize :: IO () -> IO ()
daemonize m = do
  void $ setFileCreationMask 0
  void $ forkProcess $ do
    void $ createSession
    void $ forkProcess $ do
      changeWorkingDirectory "/"
      devnull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
      mapM_ closeFd [stdInput, stdOutput, stdError]
      mapM_ (dupTo devnull) [stdInput, stdOutput, stdError]
      void $ installHandler sigHUP Ignore Nothing
      m
    exitImmediately ExitSuccess
  exitImmediately ExitSuccess
