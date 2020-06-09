
module AST.Find where

import Control.Monad

import AST.Types
import AST.Scope

import Tree
import Range
import Lattice

import Debug.Trace

findScopedDecl
  :: ( HasEnv info
     , HasRange info
     )
  => Range
  -> Pascal info
  -> Maybe ScopedDecl
findScopedDecl pos tree = do
  point <- lookupTree (\info -> pos <? getRange info) tree
  let env = getEnv (infoOf point)
  lookupEnv (void point) env

definitionOf
  :: ( HasEnv info
     , HasRange info
     )
  => Range
  -> Pascal info
  -> Maybe Range
definitionOf pos tree =
  _sdOrigin <$> findScopedDecl pos tree

typeOf
  :: ( HasEnv info
     , HasRange info
     )
  => Range
  -> Pascal info
  -> Maybe (Either (Pascal ()) Kind)
typeOf pos tree =
  _sdType =<< findScopedDecl pos tree

implementationOf
  :: ( HasEnv info
     , HasRange info
     )
  => Range
  -> Pascal info
  -> Maybe Range
implementationOf pos tree =
  _sdBody =<< findScopedDecl pos tree
