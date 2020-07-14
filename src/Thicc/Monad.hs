{-# LANGUAGE RankNTypes #-}
module Thicc.Monad
  ( ThiccM, runThiccM
  , docker, compose, getConfig, try, io, fork, forkIO
  , forever, mask
  ) where
import qualified Control.Concurrent as CC
import qualified Control.Exception as C
import System.Exit (ExitCode (..))
import System.Posix.Process
import System.Posix.Types
import System.Process
import Thicc.Config (Config, dockerBinary, composeBinary)

newtype ThiccM a = ThiccM { runThiccM :: Config -> IO a }

instance Functor ThiccM where
  fmap f m = m >>= return . f

instance Applicative ThiccM where
  pure = return
  f <*> x = do
    f' <- f
    f' <$> x

instance Monad ThiccM where
  return x = ThiccM $ \_ -> return x
  ThiccM m >>= f = ThiccM $ \cfg -> do
    x <- m cfg
    runThiccM (f x) cfg

-- | Run docker with the given arguments.
docker :: [String] -> ThiccM ()
docker args = do
  docker_binary <- dockerBinary <$> getConfig
  io $ callProcess docker_binary args

-- | Run docker-compose with the given arguments.
compose :: [String] -> ThiccM ()
compose args = do
  compose_binary <- composeBinary <$> getConfig
  io $ callProcess compose_binary args

-- | Return the thicc configuration.
getConfig :: ThiccM Config
getConfig = ThiccM pure

-- | Lift the given IO computation into ThiccM.
io :: IO a -> ThiccM a
io m = ThiccM $ \_ -> m

-- | Run the given computation in a new process.
fork :: ThiccM () -> ThiccM ProcessID
fork (ThiccM m) = ThiccM (forkProcess . m)

-- | Run the given computation in a new (Haskell) thread.
forkIO :: ThiccM () -> ThiccM CC.ThreadId
forkIO (ThiccM m) = ThiccM (CC.forkIO . m)

-- | Run the given computation, returning @Nothing@ if an exception occurred.
try :: ThiccM a -> ThiccM (Maybe a)
try (ThiccM m) = ThiccM $ \cfg -> do
  (Just <$> m cfg) `C.catches`
    [ C.Handler (\e -> C.throw (e :: ExitCode))
    , C.Handler (\(C.SomeException e) -> print e >> return Nothing)
    ]

mask :: ((forall a. ThiccM a -> ThiccM a) -> ThiccM b) -> ThiccM b
mask f = ThiccM $ \cfg -> do
  C.mask $ \outer_restore -> do
    runThiccM (f $ \(ThiccM m) -> io $ outer_restore (m cfg)) cfg

forever :: Monad m => m a -> m b
forever m = m >> forever m