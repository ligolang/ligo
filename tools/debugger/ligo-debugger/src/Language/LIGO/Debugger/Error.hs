{-# LANGUAGE TypeFamilyDependencies #-}

module Language.LIGO.Debugger.Error
  ( DebuggerException(..)
  , DebuggerExceptionType(..)

  , ImpossibleHappened(..)
  ) where

import Fmt (Buildable (..), pretty)
import Fmt.Internal.Core (FromBuilder)
import GHC.TypeLits (KnownSymbol, Symbol)

import Cli.Impl qualified as LSP

-- | Exceptions allowed in debugger logic.
class (Exception e, KnownSymbol (ExceptionTag e)) => DebuggerException e where
  -- | Unique name of the exception type.
  type ExceptionTag e = (r :: Symbol) | r -> e

  -- | Category of exception.
  debuggerExceptionType :: e -> DebuggerExceptionType

  -- | Additional data that will be provided to the plugin.
  debuggerExceptionData :: e -> Map String String
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
  deriving newtype (FromBuilder, Buildable)

instance Exception ImpossibleHappened where
  displayException = pretty

instance DebuggerException ImpossibleHappened where
  type ExceptionTag ImpossibleHappened = "ImpossibleHappened"
  debuggerExceptionType _ = AdapterInternalException

----------------------------------------------------------------------------
-- Instances for LSP types
----------------------------------------------------------------------------

instance DebuggerException LSP.LigoIOException where
  type ExceptionTag LSP.LigoIOException = "LIGO-IO"
  debuggerExceptionType _ =
    -- This is usually some "ligo executable not found in path", i.e.
    -- something to be fixed by the user
    UserException