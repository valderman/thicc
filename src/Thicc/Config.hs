{-# LANGUAGE DeriveGeneric, DataKinds, TypeFamilies #-}
module Thicc.Config
  ( Config, PortNumber, AppName, ApiKey
  , defaultConfig
  , composeFileDirectory, availableFileDirectory, keyFileDirectory
  , apiKeyFile, composeFile, availableFile, relativeAvailableFile
    -- * Configuration options
  , pruneAfterUpdate, daemonize, restPort, restUser, configDirectory
  , dockerBinary, composeBinary, restBinary
  , privilegedSocket
  ) where
import Data.Tini.Configurable
import Data.Word (Word16)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), (<.>), takeDirectory)
import System.IO.Unsafe

type PortNumber = Word16
type AppName = String
type ApiKey = String

data Config = Config
  { -- | Run @docker image prune@ after updates, to free up disk space?
    --
    --   Default: @True@
    pruneAfterUpdate :: Bool

    -- | Detach from console and run as a daemon?
    --
    --   Default: @False@
  , daemonize :: Bool

    -- | User to run the REST API under.
    --
    --   Default: @thicc-api@
  , restUser :: String

    -- | User to run the REST API under.
    --
    --   Default: @12321@
  , restPort :: PortNumber

    -- | Directory in which the config and compose yaml files can be found.
    --
    --   Default: @/etc/thicc@
  , configDirectory :: FilePath

    -- | Path to the @docker@ binary.
    --
    --   Default: @/usr/bin/docker@
  , dockerBinary :: FilePath

    -- | Path to the @docker-compose@ binary.
    --
    --   Default: @/usr/bin/docker-compose@
  , composeBinary :: FilePath

    -- | Path to the @thicc-rest@ binary.
    --
    --   The @thicc-rest@ program has a single purpose: to ask thicc to update
    --   and restart applications and report back either success or failure
    --   to its caller.
    --   When executed by thicc, the program receives 'restPort'
    --   as its only argument. It sends requests to thicc over stdout,
    --   and receives responses over stdin.
    --   See "Thicc.Messages" for more information.
    --
    --   The default program does sets up a single HTTP(S) endpoint
    --   for each application which can be used for Docker Hub hooks,
    --   but this behaviour can be overriden by providing your own program.
    --
    --   Default: @<directory of current binary>/thicc-rest@
  , restBinary :: FilePath

    -- | Name of socket through which to send privileged messages to the daemon.
    --   Note that the socket should *always* be located in a thicc-specific
    --   subdirectory.
    --
    --   Default: @/tmp/thicc-daemon/socket@
  , privilegedSocket :: FilePath
  } deriving Generic

instance Configurable Config where
  type ExcludedFields Config = '["configDirectory"]
  defaultConfig = Config
    { pruneAfterUpdate = True
    , daemonize        = False
    , restUser         = "thicc-rest"
    , restPort         = 12321
    , configDirectory  = "/etc/thicc"
    , dockerBinary     = "/usr/bin/docker"
    , composeBinary    = "/usr/bin/docker-compose"
    , restBinary       = thiccBinaryDirectory </> "thicc-rest"
    , privilegedSocket = "/tmp/thicc-daemon/socket"
    }

-- | Directory in which the currently running @thicc@ binary resides.
thiccBinaryDirectory :: FilePath
thiccBinaryDirectory = takeDirectory (unsafePerformIO getExecutablePath)

-- | Directory in which available docker compose yaml files are found.
availableFileDirectory :: Config -> FilePath
availableFileDirectory cfg = configDirectory cfg </> "apps-available"

-- | Directory in which enabled docker compose yaml files are found.
composeFileDirectory :: Config -> FilePath
composeFileDirectory cfg = configDirectory cfg </> "apps-enabled"

-- | Directory in which API keys live.
keyFileDirectory :: Config -> FilePath
keyFileDirectory cfg = configDirectory cfg </> "keys"

apiKeyFile :: Config -> String -> FilePath
apiKeyFile cfg f = keyFileDirectory cfg </> f <.> "key"

-- | Path to the compose file for the given app, if enabled.
composeFile :: Config -> String -> FilePath
composeFile cfg f = composeFileDirectory cfg </> f <.> "yml"

-- | Path to the compose file for the given, not necessarily enabled, app.
availableFile :: Config -> String -> FilePath
availableFile cfg f = availableFileDirectory cfg </> f <.> "yml"

-- | Path to the compose file for the given app in the available directory,
--   relative to the enabled directory.
relativeAvailableFile :: String -> FilePath
relativeAvailableFile app = ".." </> "apps-available" </> app <.> "yml"