{-# LANGUAGE TypeFamilyDependencies #-}

module Language.LIGO.Debugger.Error
  ( DebuggerException(..)
  , DebuggerExceptionType(..)
  ) where

import GHC.TypeLits (Symbol)

-- | Exceptions allowed in debugger logic.
class (Exception e) => DebuggerException e where
  -- | Unique name of the exception type.
  type ExceptionTag e = (r :: Symbol) | r -> e

  -- | Category of exception.
  debuggerExceptionType :: e -> DebuggerExceptionType

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
