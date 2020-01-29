module Thicc.Daemon (thiccDaemon) where
import Control.Concurrent (killThread)
import Control.Concurrent.MVar
import Control.Monad (when, filterM)
import System.Directory
import System.FilePath (takeBaseName)
import Thicc.Actions
import Thicc.ApiKeys
import Thicc.Config
import Thicc.Messages
import Thicc.Monad
import Thicc.Pipes
import Thicc.Socket

thiccDaemon :: ReadPipe ThiccRequest -> WritePipe ThiccResponse -> ThiccM ()
thiccDaemon r w = do
    mapM_ up =<< getComposeApps
    update_lock <- io newEmptyMVar
    updater_pid <- forkIO $ serve (handle update_lock) r w
    io $ putMVar update_lock updater_pid
    handlePrivSocket $ \request -> do
      result <- try $ handlePriv update_lock request
      case result of
        Just res -> return res
        _        -> return (True, Fail "command failed")
  where
    handlePriv lock Quit             = handleQuit lock
    handlePriv _    (Add app file)   = handleAdd app file
    handlePriv _    (Remove app)     = handleRemove app
    handlePriv _    (Enable app)     = handleEnable app
    handlePriv _    (Disable app)    = handleDisable app
    handlePriv _    ListEnabled      = handleListEnabled
    handlePriv _    ListAvailable    = handleListAvailable
    handlePriv _    (PrivUpdate app) = handlePrivUpdate app
    handlePriv _    (URL app)        = handleUrl app

    handleQuit lock = do
      updater_pid <- io $ takeMVar lock
      io $ killThread updater_pid
      mapM_ down =<< getComposeApps
      return (False, OK Nothing)

    handleAdd app file = do
      cfg <- getConfig
      already_exists <- io $ doesFileExist (availableFile cfg app)
      if already_exists
        then return (True, Fail "app already exists")
        else do
          is_valid <- validate file
          if is_valid
            then install app file >> return (True, OK Nothing)
            else return (True, Fail "invalid docker-compose file")

    handleRemove app = do
      exists <- validate =<< flip availableFile app <$> getConfig
      if exists
        then do
          disable app
          uninstall app
          return (True, OK Nothing)
        else return (True, Fail "no such app installed")

    handleEnable app = do
      exists <- validate =<< flip availableFile app <$> getConfig
      if exists
        then do
          enabled <- validate =<< flip composeFile app <$> getConfig
          if enabled
            then return (True, Fail "app is already enabled")
            else enable app >> return (True, OK Nothing)
        else return (True, Fail "no such app installed")

    handleDisable app = do
      exists <- validate =<< flip availableFile app <$> getConfig
      if exists
        then do
          enabled <- validate =<< flip composeFile app <$> getConfig
          if enabled
            then disable app >> return (True, OK Nothing)
            else return (True, Fail "app is not enabled")
        else return (True, Fail "no such app installed")

    handleUrl app = do
      cfg <- getConfig
      exists <- validate $ availableFile cfg app
      if exists
        then do
          mk <- getKeyFor app
          case mk of
            Just k -> mkUrl app k >>= \url -> return (True, OK (Just url))
            _      -> return (True, Fail "BUG: unable to get key!")
        else return (True, Fail "no such app installed")

    handleListAvailable = do
      apps <- getAvailableApps
      return (True, OK (Just (unwords apps)))

    handleListEnabled = do
      apps <- getComposeApps
      return (True, OK (Just (unwords apps)))

    handlePrivUpdate app = do
      exists <- validateEnabledApp app
      if exists
        then do
          update app
          return (True, OK Nothing)
        else do
          available <- validateAvailableApp app
          if available
            then return (True, Fail "app is not enabled")
            else return (True, Fail "no such app installed")

    handle lock (Update app key) = do
      pid <- io $ takeMVar lock
      result <- try $ handleUpdate app key
      io $ putMVar lock pid
      case result of
        Just res -> return res
        _        -> error "BUG: handleUpdate exploded!"

    handleUpdate app key = do
      exists <- validateEnabledApp app
      if exists
        then do
          key' <- getKeyFor app
          if Just key == key'
            then maybe UpdateFailed (const UpdateOK) <$> try (update app)
            else return UpdateFailed
        else return UpdateFailed

update :: AppName -> ThiccM ()
update key = do
  pull key
  down key
  up key
  cfg <- getConfig
  when (pruneAfterUpdate cfg) prune

mkUrl :: AppName -> ApiKey -> ThiccM String
mkUrl app key = do
  port <- show . restPort <$> getConfig
  return $ "<host>:" ++ port ++ "/c/" ++ app ++ "/" ++ key

getComposeApps :: ThiccM [AppName]
getComposeApps = do
  dir <- composeFileDirectory <$> getConfig
  filterM validateEnabledApp =<< io (map takeBaseName <$> listDirectory dir)

validateEnabledApp :: AppName -> ThiccM Bool
validateEnabledApp app = do
  file <- flip composeFile app <$> getConfig
  validate file

getAvailableApps :: ThiccM [AppName]
getAvailableApps = do
  dir <- availableFileDirectory <$> getConfig
  filterM validateAvailableApp =<< io (map takeBaseName <$> listDirectory dir)

validateAvailableApp :: AppName -> ThiccM Bool
validateAvailableApp app = do
  file <- flip availableFile app <$> getConfig
  validate file
