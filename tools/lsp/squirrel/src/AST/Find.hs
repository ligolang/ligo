
module AST.Find where

import Control.Monad

import AST.Types
import AST.Scope
import AST.Parser

import Parser
import Tree
import Range
import Lattice
import Pretty

import Debug.Trace

findScopedDecl
  :: ( HasLocalScope info
     , HasRange info
     )
  => Range
  -> Pascal info
  -> Maybe ScopedDecl
findScopedDecl pos tree = do
  point <- lookupTree pos tree
  lookupEnv (ppToText $ void point) (getLocalScope (infoOf point))

definitionOf
  :: ( HasLocalScope info
     , HasRange info
     )
  => Range
  -> Pascal info
  -> Maybe Range
definitionOf pos tree =
  _sdOrigin <$> findScopedDecl pos tree

typeOf
  :: ( HasLocalScope info
     , HasRange info
     )
  => Range
  -> Pascal info
  -> Maybe (Either (Pascal ()) Kind)
typeOf pos tree =
  _sdType =<< findScopedDecl pos tree

implementationOf
  :: ( HasLocalScope info
     , HasRange info
     )
  => Range
  -> Pascal info
  -> Maybe Range
implementationOf pos tree =
  _sdBody =<< findScopedDecl pos tree

referencesOf
  :: ( HasLocalScope info
     , HasRange info
     )
  => Range
  -> Pascal info
  -> Maybe [Range]
referencesOf pos tree =
  _sdRefs <$> findScopedDecl pos tree
