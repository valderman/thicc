{-# LANGUAGE DefaultSignatures #-}
module Thicc.Pipes
  ( ReadPipe, WritePipe, Message (..)
  , pipe, await, send, serve, runWithPipes
  ) where
import System.IO
import System.Process
import Thicc.Monad

class Message a where
  to :: String -> Maybe a
  default to :: Read a => String -> Maybe a
  to s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

  from :: a -> String
  default from :: Show a => a -> String
  from = show

newtype ReadPipe a = RP Handle
newtype WritePipe a = WP Handle

pipe :: ThiccM (ReadPipe a, WritePipe a)
pipe = io $ do
  (r, w) <- createPipe
  return (RP r, WP w)

await :: Message a => ReadPipe a -> ThiccM a
await (RP h) = do
  ln <- io $ hGetLine h
  case to ln of
    Just x -> return x
    _      -> await (RP h)

send :: Message a => WritePipe a -> a -> ThiccM ()
send (WP h) m = io $ hPutStrLn h (from m) >> hFlush h

serve :: (Message a, Message b)
      => (a -> ThiccM b) -> ReadPipe a -> WritePipe b -> ThiccM ()
serve handler r w = do
  response <- handler =<< await r
  send w response
  serve handler r w

runWithPipes :: FilePath -> [String] -> ThiccM (WritePipe a, ReadPipe b, ProcessHandle)
runWithPipes bin args = do
    (stdi, stdo, _, ph) <- io $ createProcess p
    return
      ( maybe (error "unreachable") WP stdi
      , maybe (error "unreachable") RP stdo
      , ph
      )
  where
    p = (proc bin args)
      { std_in = CreatePipe
      , std_out = CreatePipe
      }