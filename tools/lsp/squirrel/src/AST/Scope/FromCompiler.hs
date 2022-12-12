module AST.Scope.FromCompiler
  ( FromCompiler
  ) where

import Control.Category ((>>>))
import Control.Comonad.Cofree (Cofree (..), _extract)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Duplo.Lattice
import UnliftIO.Directory (canonicalizePath)


import AST.Scope.Common
import AST.Scope.ScopedDecl
  (DeclarationSpecifics (..), Module (..), ModuleDeclSpecifics (..), ScopedDecl (..),
  Type (ArrowType), TypeDeclSpecifics (..), ValueDeclSpecifics (..), _tdsInit)
import AST.Scope.ScopedDecl.Parser (parseTypeDeclSpecifics)
import AST.Skeleton (Lang, SomeLIGO (..))
import Cli
import Diagnostic (Message (..), MessageDetail (..), Severity (..))
import ListZipper (atLocus, find, withListZipper)
import Log (Log)
import Log qualified
import Range
import Util (foldMapM)

data FromCompiler

instance (HasLigoClient m, Log m) => HasScopeForest FromCompiler m where
  scopeContract tempSettings (FindContract src (SomeLIGO dialect _) msgs) = do
    defs <- getLigoDefinitions tempSettings src
    (forest, msgs') <- fromCompiler dialect defs
    pure $ FindContract src forest (msgs <> msgs')

-- | Extract `ScopeForest` from LIGO scope dump.
fromCompiler :: forall m. Log m => Lang -> LigoDefinitions -> m (ScopeForest, [Message])
fromCompiler dialect (LigoDefinitions errors warnings decls scopes) = do
  let msgs = map fromLigoErrorToMsg (errors <> warnings)
  allRefs <- fromScopes (Namespace empty) decls
  foldlM
    (\(sf, errs) (LigoScope r es _ts ms) -> do
      let env = reverse $ es <> ms
      r' <- normalizeRange $ fromLigoRangeOrDef r
      (errs', decodedDecls) <- partitionEithers <$> traverse (decodeOrReport allRefs r') env
      let ds = Map.fromList $ map (declToRef &&& id) decodedDecls
      let rs = Map.keysSet ds
      pure (injectScope ((rs, r') :< []) ds sf, errs <> errs'))
    (emptyScopeForest, msgs)
    scopes
  where
    -- For a new scope to be injected, grab its range and decl and start
    -- injection process.
    fromScopes
      :: Namespace
      -> LigoDefinitionsInner
      -> m (HashMap Text ScopedDecl)
    fromScopes namespace (LigoDefinitionsInner variables _ modules moduleAliases) =
      foldMapM id
        [ foldMapM (uncurry (fromVariableDecl namespace)) $ toPairs variables
        -- TODO (LIGO-593): Types are ignored!
        , foldMapM (uncurry (fromModuleDecl namespace)) $ toPairs modules
        , foldMapM (uncurry (fromModuleAliasDecl namespace)) $ toPairs moduleAliases
        ]

    normalizeRange :: Range -> m Range
    normalizeRange = rFile canonicalizePath

    -- LIGO compiler provides no comments, so they left [].
    fromVariableDecl
      :: Namespace
      -> Text
      -> LigoVariableDefinitionScope
      -> m (HashMap Text ScopedDecl)
    fromVariableDecl namespace mangled (LigoVariableDefinitionScope n orig bodyR ty refs) = do
      r <- normalizeRange . fromLigoRangeOrDef $ orig
      rs <- mapM (normalizeRange . fromLigoRangeOrDef) refs
      let _vdsInitRange = mbFromLigoRange bodyR
          _vdsTspec = Just $ runReader (parseTypeDeclSpecifics $ fromLigoTypeFull ty) dialect
          -- `get scope` doesn't provide information about arguments.
          -- `_vdsParams` equals to `Nothing` maeans that it isn't a function.
          -- FIXME (LIGO-679)
          _vdsParams = case _vdsTspec of
                        Just TypeDeclSpecifics{_tdsInit = ArrowType _ _} -> Just []
                        _ -> Nothing
          vspec = ValueDeclSpecifics{ .. }
      pure $ HashMap.singleton mangled (ScopedDecl n r (r : rs) [] dialect (ValueSpec vspec) namespace)

    fromModuleDecl
      :: Namespace
      -> Text
      -> LigoModuleDefinitionScope
      -> m (HashMap Text ScopedDecl)
    fromModuleDecl namespace mangled (LigoModuleDefinitionScope definition members) = do
      let LigoVariableDefinitionScope moduleName orig bodyR _ refs = definition
      r <- normalizeRange $ fromLigoRangeOrDef orig
      rs <- mapM (normalizeRange . fromLigoRangeOrDef) refs
      let
        moduleSpec = ModuleSpec ModuleDeclSpecifics
          { _mdsInitRange = fromLigoRangeOrDef bodyR
          , _mdsInit = ModuleDecl
          , _mdsName = moduleName
          }
        moduleDecl = ScopedDecl moduleName r (r : rs) [] dialect moduleSpec namespace
      decls' <- fromScopes (namespace <> Namespace (one moduleName)) members
      pure $ HashMap.insert mangled moduleDecl decls'

    fromModuleAliasDecl
      :: Namespace
      -> Text
      -> LigoModuleAliasDefinitionScope
      -> m (HashMap Text ScopedDecl)
    fromModuleAliasDecl namespace mangled (LigoModuleAliasDefinitionScope definition alias) = do
      let LigoVariableDefinitionScope moduleName orig bodyR _ refs = definition
      r <- normalizeRange $ fromLigoRangeOrDef orig
      rs <- mapM (normalizeRange . fromLigoRangeOrDef) refs
      let
        moduleSpec = ModuleSpec ModuleDeclSpecifics
          { _mdsInitRange = fromLigoRangeOrDef bodyR
          , _mdsInit = ModuleAlias $ Namespace $ fromList alias
          , _mdsName = moduleName
          }
        moduleDecl = ScopedDecl moduleName r (r : rs) [] dialect moduleSpec namespace
      pure $ HashMap.singleton mangled moduleDecl

    -- Find a place for a scope inside a ScopeForest.
    injectScope :: ScopeTree -> Map DeclRef ScopedDecl -> ScopeForest -> ScopeForest
    injectScope subject ds' (ScopeForest forest ds) =
        ScopeForest (loop forest) (ds <> ds')
      where
        loop
          = withListZipper
          $ ListZipper.find (subject `isCoveredBy`) >>> atLocus maybeLoop

        isCoveredBy = leq `on` snd . view _extract

        -- If there are no trees above subject here, just put it in.
        -- Otherwise, put it in a tree that covers it.
        maybeLoop :: Maybe ScopeTree -> Maybe ScopeTree
        maybeLoop = Just . maybe subject restart

        -- Take a forest out of tree, loop, put it back.
        restart (r :< trees) = r :< loop trees

decodeOrReport
  :: Log m
  => HashMap Text ScopedDecl
  -> Range
  -> Text
  -> m (Either Message ScopedDecl)
decodeOrReport decls range decl =
  case HashMap.lookup decl decls of
    Nothing -> do
      let
        err :: IsString s => s
        err = [Log.i|Failed to decode #{decl}. This is a bug, please report it.|]
        msg = Message
          { mMessage = FromLanguageServer err
          , mSeverity = SeverityError
          , mRange = range
          }
      Left msg <$ $Log.err err
    Just found ->
      pure $ Right found

declToRef :: ScopedDecl -> DeclRef
declToRef ScopedDecl{..} = DeclRef
  { drName = _sdName
  , drRange = _sdOrigin
  }
