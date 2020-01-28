module Thicc.Actions
  ( pull, down, up, prune
  , validate, install, uninstall
  , enable, disable
  , dropPrivileges
  ) where
import Control.Monad (when, unless, void)
import System.Directory
import System.Posix.Files
import System.Posix.User
import Thicc.Config
import Thicc.Monad

getComposeFile :: AppName -> ThiccM AppName
getComposeFile app = do
  cfg <- getConfig
  return (composeFile cfg app)

pull :: AppName -> ThiccM ()
pull f = do
  file <- getComposeFile f
  compose ["-f", file, "pull"]

down :: AppName -> ThiccM ()
down f = do
  file <- getComposeFile f
  compose ["-f", file, "down"]

up :: AppName -> ThiccM ()
up f = do
  file <- getComposeFile f
  compose ["-f", file, "up", "-d"]

prune :: ThiccM ()
prune = void $ try $ docker ["image", "prune", "-f"]

validate :: FilePath -> ThiccM Bool
validate file = do
  result <- try $ compose ["-f", file, "config"]
  case result of
    Just () -> return True
    _       -> return False

dropPrivileges :: ThiccM ()
dropPrivileges = do
  cfg <- getConfig
  io $ do
    entry <- getUserEntryForName (restUser cfg)
    setGroupID (userGroupID entry)
    setUserID (userID entry)

install :: AppName -> FilePath -> ThiccM ()
install app f = do
  target <- flip availableFile app <$> getConfig
  io $ copyFile f target

uninstall :: AppName -> ThiccM ()
uninstall app = do
  file <- flip availableFile app <$> getConfig
  exists <- io $ doesFileExist file
  when exists (io $ removeFile file)

disable :: AppName -> ThiccM ()
disable app = do
  file <- getComposeFile app
  exists <- io $ doesFileExist file
  when exists $ do
    down app
    io $ removeFile file

enable :: AppName -> ThiccM ()
enable app = do
  let source = relativeAvailableFile app
  target <- flip composeFile app <$> getConfig
  exists <- io $ doesFileExist target
  unless exists $ do
    io $ createSymbolicLink source target
    up app
