
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
import Product

import Debug.Trace

findScopedDecl
  :: ( Contains [ScopedDecl] xs
     , Contains Range xs
     , Contains (Maybe Category) xs
     )
  => Range
  -> Pascal (Product xs)
  -> Maybe ScopedDecl
findScopedDecl pos tree = do
  point <- lookupTree pos tree
  let info = infoOf point
  let fullEnv = getElem info
  do
    cat <- getElem info
    let filtered = filter (ofCategory cat) fullEnv
    lookupEnv (ppToText $ void point) filtered

definitionOf
  :: ( Contains [ScopedDecl] xs
     , Contains Range xs
     , Contains (Maybe Category) xs
     )
  => Range
  -> Pascal (Product xs)
  -> Maybe Range
definitionOf pos tree =
  _sdOrigin <$> findScopedDecl pos tree

typeOf
  :: ( Contains [ScopedDecl] xs
     , Contains Range xs
     , Contains (Maybe Category) xs
     )
  => Range
  -> Pascal (Product xs)
  -> Maybe (Either (Pascal ()) Kind)
typeOf pos tree =
  _sdType =<< findScopedDecl pos tree

implementationOf
  :: ( Contains [ScopedDecl] xs
     , Contains Range xs
     , Contains (Maybe Category) xs
     )
  => Range
  -> Pascal (Product xs)
  -> Maybe Range
implementationOf pos tree =
  _sdBody =<< findScopedDecl pos tree

referencesOf
  :: ( Contains [ScopedDecl] xs
     , Contains Range xs
     , Contains (Maybe Category) xs
     )
  => Range
  -> Pascal (Product xs)
  -> Maybe [Range]
referencesOf pos tree =
  _sdRefs <$> findScopedDecl pos tree
