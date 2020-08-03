
module AST.Find where

import Control.Monad

import Data.Maybe (listToMaybe)

import Duplo.Tree
import Duplo.Pretty
import Duplo.Lattice

import Data.Text (Text)

import AST.Types
import AST.Scope

import Product
import Range

-- import Debug.Trace

type CanSearch xs =
  ( Contains [ScopedDecl] xs
  , Contains Range xs
  , Contains (Maybe Category) xs
  , Contains [Text] xs
  , Pretty (Product xs)
  , Eq (Product xs)
  )

findScopedDecl
  :: CanSearch xs
  => Range
  -> LIGO (Product xs)
  -> Maybe ScopedDecl
findScopedDecl pos tree = do
  pt <- listToMaybe $ spineTo (\i -> pos `leq` getElem i) tree
  let info = extract pt
  let fullEnv = getElem info
  do
    categ <- getElem info
    let filtered = filter (ofCategory categ) fullEnv
    lookupEnv (ppToText $ void pt) filtered

definitionOf
  :: CanSearch xs
  => Range
  -> LIGO (Product xs)
  -> Maybe Range
definitionOf pos tree =
  _sdOrigin <$> findScopedDecl pos tree

typeOf
  :: CanSearch xs
  => Range
  -> LIGO (Product xs)
  -> Maybe (Either (LIGO ()) Kind)
typeOf pos tree =
  _sdType =<< findScopedDecl pos tree

implementationOf
  :: CanSearch xs
  => Range
  -> LIGO (Product xs)
  -> Maybe Range
implementationOf pos tree =
  _sdBody =<< findScopedDecl pos tree

referencesOf
  :: CanSearch xs
  => Range
  -> LIGO (Product xs)
  -> Maybe [Range]
referencesOf pos tree =
  _sdRefs <$> findScopedDecl pos tree
