{-# LANGUAGE TypeFamilyDependencies #-}

module Language.LIGO.Debugger.Error
  ( DebuggerException(..)
  ) where

import GHC.TypeLits (Symbol)

-- | Exceptions allowed in debugger logic.
class (Exception e) => DebuggerException e where
  type ExceptionTag e = (r :: Symbol) | r -> e
