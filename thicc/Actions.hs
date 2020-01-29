module Actions where
import System.Directory (makeAbsolute)
import System.Exit
import System.IO
import Thicc
import Thicc.Messages

send :: Config -> PrivRequest -> IO ()
send cfg msg = do
  response <- sendPriv (privilegedSocket cfg) msg
  case response of
    OK Nothing    -> return ()
    OK (Just msg) -> putStrLn msg
    Fail msg      -> hPutStrLn stderr msg >> exitFailure

stop :: Config -> IO ()
stop = flip send Quit

add :: Config -> String -> String -> IO ()
add cfg app file = send cfg . Add app =<< makeAbsolute file

remove :: Config -> String -> IO ()
remove cfg = send cfg . Remove

enable :: Config -> String -> IO ()
enable cfg = send cfg . Enable

disable :: Config -> String -> IO ()
disable cfg = send cfg . Disable

listEnabled :: Config -> IO ()
listEnabled = flip send ListEnabled

listAvailable :: Config -> IO ()
listAvailable = flip send ListAvailable

update :: Config -> String -> IO ()
update cfg = send cfg . PrivUpdate

url :: Config -> String -> String -> IO ()
url cfg host app = do
  response <- sendPriv (privilegedSocket cfg) (URL app)
  case response of
    OK Nothing  -> hPutStrLn stderr "BUG: got empty response" >> exitFailure
    OK (Just u) -> putStrLn ("http://" ++ host ++ u)
    Fail msg    -> hPutStrLn stderr msg >> exitFailure
