
module Log (module Log, i) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad
import Data.IORef
import Data.String.Interpolate (i)

import System.IO.Unsafe

import System.IO (hFlush, hPutStrLn, stderr, stdout)

data Level = DEBUG | ERROR deriving (Eq, Ord)

{-# NOINLINE logLevel #-}
logLevel :: IORef Level
logLevel = unsafePerformIO do
  newIORef ERROR

{-# NOINLINE logLock #-}
logLock :: MVar ()
logLock = unsafePerformIO do
  newMVar ()

debug :: MonadIO m => String -> String -> m ()
debug sys msg = liftIO do
  lvl <- readIORef logLevel
  when (lvl == DEBUG) do
    synchronized do
      hPutStrLn stderr $ "DEBUG (" <> sys <> "): " <> msg
      hFlush stderr

err :: MonadIO m => String -> String -> m ()
err sys msg = liftIO do
  synchronized do
    hPutStrLn stderr $ "ERROR (" <> sys <> "): " <> msg

enableDebug :: MonadIO m => Level -> m ()
enableDebug level = liftIO do
  writeIORef logLevel level
  return ()

synchronized :: (MonadMask m, MonadIO m) => m a -> m a
synchronized action = bracket_
  do liftIO $ takeMVar logLock
  do liftIO $ putMVar  logLock ()
  action

-- -- | Helper that outputs message to stdout and stderr immediately.
-- output :: String -> String -> IO ()
-- output msg err = do
--   hPutStrLn stdout $ "OUTOUT(stdout): " ++ msg
--   hFlush stdout
--   hPutStrLn stderr $ "OUTOUT(stderr): " ++ err
--   hFlush stderr
