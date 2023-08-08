{-# LANGUAGE TypeFamilyDependencies #-}

module Language.LIGO.Debugger.Error
  ( DebuggerException(..)
  , DebuggerExceptionType(..)

  , ImpossibleHappened(..)

  , handleJSONException
  ) where

import Data.Aeson (decode)
import Fmt.Buildable (Buildable, FromDoc, build, pretty)
import GHC.TypeLits (KnownSymbol, Symbol)
import UnliftIO (MonadUnliftIO, handle, throwIO)

import Language.LIGO.Debugger.CLI.Exception
import Language.LIGO.Extension

-- | Exceptions allowed in debugger logic.
class (Exception e, KnownSymbol (ExceptionTag e)) => DebuggerException e where
  -- | Unique name of the exception type.
  type ExceptionTag e = (r :: Symbol) | r -> e

  -- | Category of exception.
  debuggerExceptionType :: e -> DebuggerExceptionType

  -- | Flag that indicates whether debugging session
  -- should be interrupted or not.
  --
  -- Note that a debugging session would be interrupted
  -- only if an exception raises after @Launch@ request
  -- (in case it's @True@ of course).
  shouldInterruptDebuggingSession :: Bool

  -- | Additional data that will be provided to the plugin.
  debuggerExceptionData :: e -> Map Text Text
  debuggerExceptionData _ = mempty

-- | Classification of exceptions by who is guilty in that it occured.
data DebuggerExceptionType
  = UserException
    -- ^ A normal, fully expected exception that is caused by the user's input.
    --
    -- It should be reported to the user mostly as-is to let them know
    -- that they should do something differently.
  | MidPluginLayerException
    -- ^ Exception happens due to miscommunication between
    -- the plugin and our adapter.
    --
    -- For the adapter, these errors are normal errors of bad input provided
    -- by the plugin, but for the eventual user these are internal errors.
  | MidLigoLayerException
    -- ^ Exception is caused by @ligo@ executable behaving unexpectedly,
    -- or us not handling some of its normal cases.
    --
    -- If some other version of @ligo@ may cause your error, choose this
    -- exception type.
  | LigoLayerException
    -- ^ Call to @ligo@ executable produced this error.
    --
    -- Here there is no easy way to say whether the error is in user input
    -- (e.g. invalid contract) or due to our way of calling @ligo@
    -- (e.g. recently ligo changed its CLI interface).
    -- So this is put to a separate category.
  | AdapterInternalException
    -- ^ Exception is caused solely by a bug in the adapter.
    --
    -- If not sure whether to choose this exception type or another one,
    -- prefer the other one, since otherwise any exception can be put
    -- into this category, as always remains a chance of a bug taking place
    -- in our code.
  deriving stock (Eq)

instance Buildable DebuggerExceptionType where
  build = \case
    UserException -> "user"
    MidPluginLayerException -> "adapter-plugin"
    MidLigoLayerException -> "adapter-ligo"
    LigoLayerException -> "ligo"
    AdapterInternalException -> "adapter"

-- | Something unexpected happened disregard the input from other places
-- (plugin or LIGO).
newtype ImpossibleHappened = ImpossibleHappened Text
  deriving stock (Eq, Show)
  deriving newtype (FromDoc, Buildable)

instance Exception ImpossibleHappened where
  displayException = pretty

instance DebuggerException ImpossibleHappened where
  type ExceptionTag ImpossibleHappened = "ImpossibleHappened"
  debuggerExceptionType _ = AdapterInternalException
  shouldInterruptDebuggingSession = True

----------------------------------------------------------------------------
-- Instances for exceptions
----------------------------------------------------------------------------

instance DebuggerException LigoIOException where
  type ExceptionTag LigoIOException = "LIGO-IO"
  debuggerExceptionType _ =
    -- This is usually some "ligo executable not found in path", i.e.
    -- something to be fixed by the user
    UserException
  shouldInterruptDebuggingSession = False

instance DebuggerException UnsupportedExtension where
  type ExceptionTag UnsupportedExtension = "UnsupportedExtension"
  debuggerExceptionType _ = MidLigoLayerException
  shouldInterruptDebuggingSession = False

instance DebuggerException LigoDecodeException where
  type ExceptionTag LigoDecodeException = "LigoDecode"
  debuggerExceptionType _ = MidLigoLayerException
  shouldInterruptDebuggingSession = False

instance DebuggerException LigoResolveConfigException where
  type ExceptionTag LigoResolveConfigException = "LigoResolveConfig"
  debuggerExceptionType _ = UserException
  shouldInterruptDebuggingSession = False

instance DebuggerException ConfigurationException where
  type ExceptionTag ConfigurationException = "Configuration"
  debuggerExceptionType _ = UserException
  shouldInterruptDebuggingSession = False

instance DebuggerException PluginCommunicationException where
  type ExceptionTag PluginCommunicationException = "PluginComminication"
  debuggerExceptionType _ = MidPluginLayerException
  shouldInterruptDebuggingSession = True

instance DebuggerException LigoCallException where
  type ExceptionTag LigoCallException = "LigoCall"
  debuggerExceptionType _ = LigoLayerException
  shouldInterruptDebuggingSession = False

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Handle @LigoClientFailureException@, try to decode it from JSON
-- and wrap it's contents with provided constructor.
handleJSONException :: (DebuggerException exc, MonadUnliftIO m) => (Text -> exc) -> m a -> m a
handleJSONException ctor = handle \e@LigoClientFailureException{..} -> do
  let excBody = encodeUtf8 cfeStderr
  case decode excBody of
    Just [LigoJSONException{..}] -> do
      let rewrap = ctor (ljecMessage ljeContent)
      throwIO rewrap
    _ -> throwIO e
