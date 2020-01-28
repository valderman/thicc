module Thicc.Socket where
import Control.Monad (when)
import Data.ByteString.Char8 as BS
import Network.Socket
import Network.Socket.ByteString
import System.Directory
import Thicc.Config
import Thicc.Messages
import Thicc.Monad

handlePrivSocket :: (PrivRequest -> ThiccM (Bool, PrivResponse)) -> ThiccM ()
handlePrivSocket handler = do
  sockname <- privilegedSocket <$> getConfig
  handleSocket sockname Fail handler

handleSocket :: (Message a, Message b)
             => String -> (String -> b) -> (a -> ThiccM (Bool, b)) -> ThiccM ()
handleSocket sockname failure handler = do
    _ <- try $ io $ removeFile sockname
    listen_sock <- io $ socket AF_UNIX Stream 0
    io $ bind listen_sock (SockAddrUnix sockname)
    io $ listen listen_sock 5
    go $ do
      (sock, _) <- io $ accept listen_sock
      request <- io $ to . BS.unpack <$> recv sock 4096
      (cont, response) <- case request of
        Just req -> handler req
        _        -> pure (True, failure "invalid message")
      io $ sendAll sock (BS.pack $ from response)
      io $ close sock
      return cont
    io $ close listen_sock
    io $ removeFile sockname
  where
    go m = do
      continue <- m
      when continue (go m)

sendPriv :: FilePath -> PrivRequest -> IO PrivResponse
sendPriv sockname msg = do
  sock <- socket AF_UNIX Stream 0
  connect sock (SockAddrUnix sockname)
  sendAll sock (BS.pack $ from msg)
  response <- to . BS.unpack <$> recv sock 4096
  close sock
  return $ case response of
    Just resp -> resp
    _         -> Fail "invalid response"