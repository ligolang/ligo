module AST.Capabilities.Find
  ( CanSearch
  , TypeDefinitionRes (..)
  , findScopedDecl
  , findNodeAtPoint
  , definitionOf
  , typeDefinitionOf
  , typeDefinitionAt
  , dereferenceTspec
  , referencesOf
  , findModuleDecl
  ) where

import Prelude hiding (Product (..), Type)

import Control.Lens (_Just)
import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree
import Unsafe qualified

import AST.Scope (Level (..), lookupEnv, ofLevel)
import AST.Scope.ScopedDecl
  (Scope, ScopedDecl (..), Type (..), TypeDeclSpecifics (..), _TypeSpec, _ValueSpec, extractRefName,
  sdSpec, vdsTspec)
import AST.Skeleton (Binding (BModuleAlias, BModuleDecl), LIGO, ModuleName, SomeLIGO, nestedLIGO)

import Product
import Range

type CanSearch xs =
  ( Contains Scope xs
  , Contains Range xs
  , Contains (Maybe Level) xs
  )

findScopedDecl
  :: CanSearch xs
  => Range
  -> SomeLIGO xs
  -> Maybe ScopedDecl
findScopedDecl pos tree = do
  node <- findNodeAtPoint pos tree
  let info = extract node
  let fullEnv = getElem info
  level <- getElem info
  let filtered = filter (ofLevel level) fullEnv
  lookupEnv (ppToText $ void node) (getRange node) filtered

findInfoAtPoint
  :: Contains Range xs
  => Range -> SomeLIGO xs -> Maybe (Product xs)
findInfoAtPoint pos tree = extract <$> findNodeAtPoint pos tree

findNodeAtPoint
  :: Contains Range xs
  => Range -> SomeLIGO xs -> Maybe (LIGO xs)
findNodeAtPoint pos tree = listToMaybe $ spineAtPoint pos tree

spineAtPoint
  :: Contains Range xs
  => Range -> SomeLIGO xs -> [LIGO xs]
spineAtPoint pos tree = spineTo (\i -> pos `leq` getRange i) (tree ^. nestedLIGO)

definitionOf
  :: CanSearch xs
  => Range
  -> SomeLIGO xs
  -> Maybe Range
definitionOf pos tree =
  _sdOrigin <$> findScopedDecl pos tree

-- | A type is declared if it has a name and a declaration corresponding to that
-- name in the scope. A type is inlined if it has a definition inlined into
-- something else (e.g. a variable definition). A type is not found if for some
-- reason it's not there (e.g. a variable lacks a type annotation).
data TypeDefinitionRes
  = TypeNotFound
  | TypeDeclared ScopedDecl
  | TypeInlined (TypeDeclSpecifics Type)

typeDefinitionOf :: CanSearch xs => Range -> SomeLIGO xs -> TypeDefinitionRes
typeDefinitionOf pos tree = fromMaybe TypeNotFound $ do
  scope <- getElem <$> findInfoAtPoint pos tree
  varDecl <- findScopedDecl pos tree
  tspec <- varDecl ^? sdSpec . _ValueSpec . vdsTspec . _Just
  Just $ case findTypeRefDeclaration scope (_tdsInit tspec) (_tdsInitRange tspec) of
    Nothing -> TypeInlined tspec
    Just decl -> TypeDeclared decl

findTypeRefDeclaration :: Scope -> Type -> Range -> Maybe ScopedDecl
findTypeRefDeclaration scope typ pos = do
  refName <- extractRefName typ
  lookupEnv refName pos (filter (ofLevel TypeLevel) scope)

typeDefinitionAt :: CanSearch xs => Range -> SomeLIGO xs -> Maybe Range
typeDefinitionAt pos tree = case typeDefinitionOf pos tree of
  TypeNotFound -> Nothing
  TypeDeclared decl -> Just (_sdOrigin decl)
  TypeInlined tspec -> Just (_tdsInitRange tspec)

-- | If the type is an alias to a declared type, substitute the type with the
-- aliased type. Otherwise, leave it be.
--
-- If the aliased name is undeclared, leave the type be.
dereferenceTspec :: Scope -> TypeDeclSpecifics Type -> TypeDeclSpecifics Type
dereferenceTspec scope tspec = fromMaybe tspec $ do
  refDecl <- findTypeRefDeclaration scope (_tdsInit tspec) (_tdsInitRange tspec)
  refDecl ^? sdSpec . _TypeSpec . _2

referencesOf
  :: CanSearch xs
  => Range
  -> SomeLIGO xs
  -> Maybe [Range]
referencesOf pos tree =
  _sdRefs <$> findScopedDecl pos tree

-- | Given a module name position, attempts to find where its origin
-- 'ScopedDecl' is. This function will also try to resolve every module alias.
findModuleDecl :: CanSearch xs => Range -> SomeLIGO xs -> Maybe ScopedDecl
findModuleDecl pos tree = do
  decl@ScopedDecl{..} <- findScopedDecl pos tree
  -- Found a module, but is it an alias or declaration?
  let spine = spineAtPoint _sdOrigin tree
  moduleNode <- layer =<< find (maybe False isModule . layer) spine
  case moduleNode of
    -- OK, found what we were looking for:
    BModuleDecl _ _ -> pure decl
    -- Alias, must resolve it:
    BModuleAlias _ path -> do
      (aliasR, _aliasNode) <- match @ModuleName $ Unsafe.last path
      findModuleDecl (getRange aliasR) tree
    -- Impossible:
    _ -> Nothing
  where
    isModule = \case
      BModuleDecl _ _ -> True
      BModuleAlias _ _ -> True
      _ -> False
