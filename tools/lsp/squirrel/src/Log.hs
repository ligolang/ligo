-- | Provides a façade to Katip's logger.
module Log
  ( Severity (..)
  , NoLoggingT (..)
  , LogT
  , Log
  , i
  , addContext
  , addNamespace
  , sl
  , debug
  , warning
  , err
  , critical
  , withLogger
  , withoutLogger
  , flagBasedEnv
  , flagBasedSeverity
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.String.Interpolate.IsString (i)
import Data.Text (pack)
import Katip
import Katip.Monadic (NoLoggingT (..))
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax.Compat (SpliceQ, examineSplice, liftSplice)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (stderr)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import UnliftIO.Exception (bracket)

type LogT = KatipContextT
type Log = KatipContext

addContext :: (LogItem i, Log m) => i -> m a -> m a
addContext = katipAddContext

addNamespace :: Log m => Namespace -> m a -> m a
addNamespace = katipAddNamespace

debug :: ExpQ
debug = [| $logTM DebugS |]
{-# INLINE debug #-}

warning :: ExpQ
warning = [| $logTM WarningS |]
{-# INLINE warning #-}

err :: ExpQ
err = [| $logTM ErrorS |]
{-# INLINE err #-}

critical :: ExpQ
critical = [| $logTM CriticalS |]
{-# INLINE critical #-}

withoutLogger
  :: ((forall a. NoLoggingT m a -> m a) -> m b)
  -> m b
withoutLogger action = action runNoLoggingT

withLogger
  :: MonadUnliftIO m
  => Severity
  -> Namespace
  -> Environment
  -> ((forall a. LogT m a -> m a) -> m b)
  -> m b
withLogger level initNamespace env action = do
  initEnv <- liftIO $ initLogEnv "ligo" env
  let
    mkLogEnv = liftIO do
      stderrScribe <- mkHandleScribe ColorIfTerminal stderr (permitItem level) V2
      le <- registerScribe "stderr" stderrScribe defaultScribeSettings initEnv

      dir <- getCanonicalTemporaryDirectory
      handleScribe <- mkFileScribe (dir </> "ligo-language-server.log") (permitItem DebugS) V3
      registerScribe "Log file" handleScribe defaultScribeSettings le
    delLogEnv = liftIO . closeScribes
  bracket mkLogEnv delLogEnv \le ->
    action (runKatipContextT le () initNamespace)

flagBasedEnv :: SpliceQ Environment
flagBasedEnv = liftSplice do
  let flagName = "LIGO_ENV"
  liftIO (lookupEnv flagName) >>= maybe
    (examineSplice [|| "production" ||])
    (\flag -> [|| Environment (pack flag) ||])

flagBasedSeverity :: SpliceQ Severity
flagBasedSeverity = liftSplice do
  let flagName = "LIGO_SEVERITY"
  liftIO (lookupEnv flagName) >>= maybe
    (examineSplice [|| ErrorS ||])
    (\flag -> case textToSeverity $ pack flag of
      Nothing -> fail $ "Unrecognized " <> flagName <> " flag: " <> flag
      Just severity -> examineSplice [|| severity ||])
