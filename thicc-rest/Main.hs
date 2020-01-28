{-# LANGUAGE TypeOperators, DataKinds, OverloadedStrings #-}
module Main where
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Success, Error)
import Data.String (fromString)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant
import Servant.Server
import System.Environment (getArgs)
import Thicc
import Thicc.Messages

newtype CallbackUrl = CallbackUrl { unCallbackUrl :: String }

instance FromJSON CallbackUrl where
  parseJSON = withObject "CallbackUrl" $ \o ->
    CallbackUrl <$> o .: "callback_url"

data HookResult = Success | Failure | Error
  deriving Eq
instance Show HookResult where
  show Success = "success"
  show Failure = "failure"
  show Error   = "error"

data HookResponse = HookResponse
  { result :: HookResult
  , description :: String
  , context :: String
  , targetUrl :: Maybe String
  }

instance ToJSON HookResponse where
  toJSON r = object
    [ "state" .= show (result r)
    , "description" .= description r
    , "context" .= context r
    , "target_url" .= targetUrl r
    ]

type DockerCallback
  =  "c"
  :> Capture "app" String
  :> Capture "key" String
  :> ReqBody '[JSON] CallbackUrl
  :> Post '[JSON] ()

dockerCallback :: Server DockerCallback
dockerCallback app key (CallbackUrl url) = liftIO $ do
  sendStdOut (Update app key)
  forkIO $ awaitStdIn >>= respond url
  return ()

respond :: String -> ThiccResponse -> IO ()
respond url resp = do
  mgr <- getGlobalManager
  let req = (fromString url)
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode $ HookResponse
          { result = Success
          , description = "Update succeeded!"
          , context = "CI by thicc"
          , targetUrl = Nothing
          }
        }
  response <- httpLbs req mgr
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [port] -> run (read port) $
      serve (Proxy :: Proxy DockerCallback) dockerCallback
    _      -> error "usage: thicc-rest PORT"