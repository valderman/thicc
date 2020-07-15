module Main where
import Control.Monad (unless, (<=<))
import Data.Tini.Configurable
import System.Console.GetOpt
import System.Directory (makeAbsolute)
import System.Environment
import System.Exit
import System.IO
import Thicc
import Actions

opts :: [OptDescr (Config -> IO Config)]
opts =
  [ Option "c" ["config"]
    (ReqArg (\s c -> makeAbsolute s >>= \d -> pure c { configDirectory = d }) "DIR")
    "Look for thicc config files in DIR."
  , Option "f" ["foreground"]
    (NoArg $ \c -> pure c { daemonize = False } )
    "Run the thicc server in the foreground (the default)."
  , Option "d" ["daemon"]
    (NoArg $ \c -> pure c { daemonize = True } )
    "Run the thicc server as a daemon."
  ]

type Action = (String, ([String], [String] -> Config -> IO (), String))

actions :: [Action]
actions =
  [ ("start",          ([], const thicc,
     "Starts the thicc daemon."))
  , ("stop",           ([], const stop,
     "Stops the thicc daemon."))
  , ("add",            (["appname", "file"], \[app, f] cfg -> add cfg app f,
     "Adds docker-compose file <file> to the daemon as <appname>."))
  , ("remove",         (["appname"], \[app] cfg -> remove cfg app,
     "Removes the application by the name <appname> from thicc."))
  , ("enable",         (["appname"], \[app] cfg -> enable cfg app,
     "Starts the application <appname> and monitors it for updates."))
  , ("disable",        (["appname"], \[app] cfg -> disable cfg app,
     "Stops the application <appname> and stops monitoring it for updates."))
  , ("url",            (["appname", "host"], \[app, h] cfg -> url cfg h app,
     "Prints the callback URL for <appname> when served from <host>."))
  , ("list-enabled",   ([], const listEnabled,
     "Lists all enabled applications."))
  , ("list-available", ([], const listAvailable,
     "Lists all available applications."))
  , ("update",         (["appname"], \[app] cfg -> update cfg app,
     "Forcibly update <appname>."))
  , ("help",           ([], \_ _ -> printHelp,
     "Print this message."))
  ]

printHelp :: IO ()
printHelp = do
  putStrLn "Controls and/or executes the thicc daemon."
  putStrLn "Available commands:"
  flip mapM_ actions $ \(name, (args, _, doc)) -> do
    let prefix = "  " ++ name ++ " " ++ unwords args
        numTabs = (32 - length prefix - 1) `quot` 8
        tabs = replicate numTabs '\t'
    putStrLn $ prefix ++ tabs ++ doc
  putStrLn $ usageInfo "\nAvailable options:" opts
  

choose :: [Action] -> [String] -> Either String (Config -> IO ())
choose acts (x:xs) =
  case lookup x acts of
    Just (args, action, _)
      | length xs == length args -> Right (action xs)
      | otherwise                -> Left $ wrongNumberOfArgs x args
    Nothing                      -> Left $ unwords ["no such action:", x]
choose _ [] =
  Left "no action specified; try 'help'"

wrongNumberOfArgs :: String -> [String] -> String
wrongNumberOfArgs act as
  | len == 0 = action ["does not accept any arguments"]
  | len == 1 = action ["requires exactly one argument:", head as]
  | len == 2 = action ["requires exactly two arguments:", as!!0, "and", as!!1]
  | otherwise = action $ ["requires exactly", show len, "arguments:"] ++ as
  where
    len = length as
    action ws = unwords (("action '" ++ act ++ "'") : ws)

main :: IO ()
main = do
  args <- getArgs
  let (cfgs, nonopts, errors) = getOpt Permute opts args
      mkCfg = foldl (<=<) pure cfgs
  cfg <- mkCfg defaultConfig
  unless (null errors) $ do
    mapM_ (hPutStrLn stderr) errors
    exitFailure
  cfg' <- readConfigFileWith cfg (configFile cfg)
  case choose actions nonopts of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right go -> mkCfg cfg' >>= go
