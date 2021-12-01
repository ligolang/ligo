module Log
  ( Level (..)
  , i
  , debug
  , err
  , setLogLevel
  , flagBasedLogLevel
  ) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad
import Data.IORef
import Data.String.Interpolate.IsString (i)
import Language.Haskell.TH (Exp (ConE), Q)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

data Level = DEBUG | ERROR | CRASH deriving stock (Eq, Ord, Read, Show)

{-# NOINLINE logLevel #-}
logLevel :: IORef Level
logLevel = unsafePerformIO do
  newIORef CRASH

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

setLogLevel :: MonadIO m => Level -> m ()
setLogLevel level = liftIO do
  writeIORef logLevel level
  return ()

synchronized :: (MonadMask m, MonadIO m) => m a -> m a
synchronized = bracket_
  do liftIO $ takeMVar logLock
  do liftIO $ putMVar  logLock ()

flagBasedLogLevel :: Q Exp
flagBasedLogLevel = do
  let flagName = "LIGO_LOG_LEVEL"
  ConE <$> liftIO do
    lookupEnv flagName >>= maybe
      (pure 'ERROR)
      (\case
        "DEBUG" -> pure 'DEBUG
        "ERROR" -> pure 'ERROR
        "CRASH" -> pure 'CRASH
        other -> fail $ "Unrecognized " <> flagName <> " flag: " <> other)
