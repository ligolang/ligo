module AST.Capabilities.Find where

import Control.Monad
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree

import AST.Scope (Level, lookupEnv, ofLevel)
import AST.Scope.ScopedDecl
  (DeclarationSpecifics (..), ScopedDecl (..), TypeDeclSpecifics (..), ValueDeclSpecifics (..))
import AST.Skeleton

import Product
import Range

type CanSearch xs =
  ( Contains [ScopedDecl] xs
  , Contains Range xs
  , Contains (Maybe Level) xs
  , Contains [Text] xs
  , Pretty (Product xs)
  , Modifies (Product xs)
  , Eq (Product xs)
  )

findScopedDecl
  :: CanSearch xs
  => Range
  -> LIGO xs
  -> Maybe ScopedDecl
findScopedDecl pos tree = do
  pt <- listToMaybe $ spineTo (\i -> pos `leq` getElem i) tree
  let info = extract pt
  let fullEnv = getElem info
  do
    level <- getElem info
    let filtered = filter (ofLevel level) fullEnv
    lookupEnv (ppToText $ void pt) filtered

definitionOf
  :: CanSearch xs
  => Range
  -> LIGO xs
  -> Maybe Range
definitionOf pos tree =
  _sdOrigin <$> findScopedDecl pos tree

implementationOf
  :: CanSearch xs
  => Range
  -> LIGO xs
  -> Maybe Range
implementationOf pos tree = do
  decl <- findScopedDecl pos tree
  case _sdSpec decl of
    ValueSpec vspec -> _vdsInitRange vspec
    TypeSpec tspec -> pure (_tdsInitRange tspec)

referencesOf
  :: CanSearch xs
  => Range
  -> LIGO xs
  -> Maybe [Range]
referencesOf pos tree =
  _sdRefs <$> findScopedDecl pos tree
