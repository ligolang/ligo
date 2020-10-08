
{-# language Strict #-}

{- | /The/ scope resolution system.
-}

module AST.Scope
  ( module M
  )
  where

import AST.Scope.Common as M
import AST.Scope.Fallback as M
import AST.Scope.FromCompiler as M
import AST.Scope.Standard as M
