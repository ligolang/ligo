module AST.Scope.Fallback
  ( Fallback
  , loop
  , loopM
  , loopM_
  ) where

import Prelude hiding (Alt (..), Product (..), Sum (..), exp)

import Control.Category ((>>>))
import Control.Lens (makeLenses, (|>))
import Control.Monad.RWS.Strict (RWS, evalRWS, tell)
import Control.Monad.Writer (Writer, execWriter)
import Data.Foldable qualified as Foldable (for_)
import Data.HashMap.Lazy qualified as HashMap
import Data.Kind qualified (Type)
import Data.List.NonEmpty qualified as NE (unzip)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Duplo.Pretty (Doc, PP (..), Pretty (..), ppToText, (<+>), (<.>))
import Duplo.Tree hiding (loop)
import Witherable (wither)

import AST.Pretty (PPableLIGO, lppDialect)
import AST.Scope.Common
import AST.Scope.ScopedDecl
  (DeclarationSpecifics (..), Module (..), ModuleDeclSpecifics (..), ScopedDecl (..),
  Type (VariableType), TypeDeclSpecifics (..), TypeVariable (..), ValueDeclSpecifics (..),
  _ModuleSpec, mdsInit, sdName, sdNamespace, sdOrigin, sdRefs, sdSpec)
import AST.Scope.ScopedDecl.Parser
  (parseModule, parseParameters, parseQuotedTypeParams, parseTypeDeclSpecifics)
import AST.Skeleton hiding (QuotedTypeParams (..), Type)
import AST.Skeleton qualified as Skeleton (QuotedTypeParams (..), Type (..))
import Cli.Types
import Diagnostic (Message (..), MessageDetail (FromLanguageServer), Severity (..))
import Log (i)
import Parser (ParsedInfo)
import Product
import Range
import Util (foldMapM, (<<&>>))

data Fallback

data TreeDoesNotContainName =
  TreeDoesNotContainName
    Doc  -- ^ pprinted tree (used for simplifying purposes for not stacking
         -- type parameters for `ScopeM` which brings plethora of confusion)
    Range -- ^ location where the error has occurred
    Text -- ^ variable name
  deriving stock (Show)

-- | Contains a 'DeclRef', as well as some extra information needed to deal with
-- modules by this 'Fallback' scopes.
data ExtendedDeclRef = ExtendedDeclRef
  { edrDeclRef :: DeclRef
    -- ^ The 'DeclRef' of this reference.
  , edrNamespace :: Namespace
    -- ^ The namespace where this reference was declared.
  , edrRefKind :: RefKind
    -- ^ Whether this is an ordinary declaration, or a module declaration.
  } deriving Show via PP ExtendedDeclRef
    deriving stock (Eq, Ord)

data RefKind
  = OrdinaryRef Level
  | ModuleAliasRef (Maybe ExtendedDeclRef)
  deriving Show via PP RefKind
  deriving stock (Eq, Ord)

instance Pretty ExtendedDeclRef where
  pp (ExtendedDeclRef ref ns rk) = pp ns <.> "." <.> pp ref <+> pp rk

instance Pretty RefKind where
  pp (OrdinaryRef l) = pp l <+> "ref"
  pp (ModuleAliasRef ns) = "alias" <+> maybe "unresolved" pp ns

type ExtendedScopeInfo = (Set ExtendedDeclRef, Range)
type ExtendedScopeTree = Cofree [] ExtendedScopeInfo

data InScopeRef
  = InScopeOrdinaryRef Range
  -- ^ An ordinary value, function, or type reference, with its 'Range'.
  | InScopeModuleAliasRef Range (Maybe ExtendedDeclRef)
  -- ^ A reference to a module alias. Suppose that @module A = B.C@, then this
  -- constructor contains the 'Range' of @A@ as well as the chain of declaration
  -- references up to the original module (if resolved).
  deriving stock (Show)

-- | A 'Namespace' may be explicitly qualified (e.g. @Foo.bar@) or implicitly
-- qualified (e.g. @module Foo = struct let bar = 0 end@). This datatype records
-- in which situation we're accessing a name.
data Qualified = Qualified
  { unqualified :: Namespace
  , qualified   :: Namespace
  } deriving stock (Show)

mapInUnqualified, mapInQualified :: (Namespace -> Namespace) -> Qualified -> Qualified
mapInUnqualified f (Qualified uns qns) = Qualified (f uns) qns
mapInQualified   f (Qualified uns qns) = Qualified uns (f qns)

data RefKey = RefKey
  { rkNamespace :: Namespace
  , rkName :: Text
  , rkLevel :: Level
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- | The scoping environment, that may be locally updated at each subnode.
data ScopeEnv = ScopeEnv
  { _seNamespace :: Qualified
  , _seInScope :: HashMap RefKey InScopeRef
  , _seDialect :: Lang
  }

makeLenses ''ScopeEnv

initScopeEnv :: Lang -> ScopeEnv
initScopeEnv _seDialect = ScopeEnv
  { _seNamespace = Qualified (Namespace empty) (Namespace empty)
  , _seInScope = HashMap.empty
  , _seDialect
  }

toMsg :: TreeDoesNotContainName -> Message
toMsg (TreeDoesNotContainName tree range name) =
  Message (FromLanguageServer [i|Expected to find a #{name}, but got `#{tree}`|]) SeverityError range

instance HasLigoClient m => HasScopeForest Fallback m where
  scopeContract _ (FindContract src (SomeLIGO dialect ligo) msgs) = do
    let
      (sf, (`appEndo` []) -> errs) = evalRWS (getEnv ligo) (initScopeEnv dialect) Map.empty
      msgs' = map toMsg errs
    pure $ FindContract src sf (msgs <> msgs')

type ScopeM = RWS ScopeEnv (Endo [TreeDoesNotContainName]) (Map ExtendedDeclRef ScopedDecl)

askNamespace :: ScopeM Qualified
askNamespace = asks (view seNamespace)
{-# INLINE askNamespace #-}

askInScope :: ScopeM (HashMap RefKey InScopeRef)
askInScope = asks (view seInScope)
{-# INLINE askInScope #-}

askDialect :: ScopeM Lang
askDialect = asks (view seDialect)
{-# INLINE askDialect #-}

-- | Apply some transformation to the current 'Namespace'.
mapNamespace
  :: (Namespace -> Namespace)
  -> ScopeM a
  -> ScopeM a
mapNamespace f = mapQualified (mapInQualified f)

-- | Apply some transformation to the current 'Qualified'.
mapQualified
  :: (Qualified -> Qualified)
  -> ScopeM a
  -> ScopeM a
mapQualified f = local (seNamespace %~ f)

-- | Apply some transformation to the current scope.
mapInScope
  :: (HashMap RefKey InScopeRef -> HashMap RefKey InScopeRef)
  -> ScopeM a
  -> ScopeM a
mapInScope f = local (seInScope %~ f)

insertScope :: RefKind -> ScopedDecl -> ScopeM ExtendedDeclRef
insertScope edrRefKind scopedDecl = do
  let
    declRef = ExtendedDeclRef
      { edrDeclRef = DeclRef
        { drName = scopedDecl ^. sdName
        , drRange = scopedDecl ^. sdOrigin
        }
      , edrNamespace = scopedDecl ^. sdNamespace
      , edrRefKind
      }
  modify (Map.insert declRef scopedDecl)
  pure declRef

-- | Tries to expand a module alias into its full definition. For instance, if
--
-- > module A = struct
-- >   module Y = struct
-- >     module Z = struct
-- >     end
-- >   end
-- >
-- >   module X = Y.Z end
-- > end
-- > module W = A
--
-- Then using `W.X` will expand to 'A.Y.Z'.
resolveModuleAlias :: Text -> ScopeM (Maybe Namespace)
resolveModuleAlias moduleName = do
  namespace <- askNamespace
  lookupInOuterModules ModuleLevel namespace moduleName <&> \case
    Nothing -> Nothing
    Just (InScopeOrdinaryRef _, ns) -> Just $ ns <> Namespace (one moduleName)
    Just (InScopeModuleAliasRef _ ref, _) -> expandAliasRef ref
  where
    expandAliasRef :: Maybe ExtendedDeclRef -> Maybe Namespace
    expandAliasRef Nothing =
      Nothing
    expandAliasRef (Just ExtendedDeclRef{edrDeclRef = DeclRef{drName}, edrNamespace, edrRefKind = OrdinaryRef ModuleLevel}) =
      Just $ edrNamespace <> Namespace (one drName)
    expandAliasRef (Just ExtendedDeclRef{edrRefKind = OrdinaryRef _}) =
      Nothing
    expandAliasRef (Just ExtendedDeclRef{edrRefKind = ModuleAliasRef refM}) =
      expandAliasRef refM

-- It's possible that the resolved name has parts that are unresolved, so we
-- deal with them here.
--   This is done like so: the first name part of the namespace should always be
-- in scope of the current unqualified namespace (assuming it's bound). For the
-- next name parts, we insert the previous (resolved) namespaces as qualified
-- namespaces, so it will look _inside_ these namespaces.
--   We always expand the first name part until it's ordinary (or unbound), and
-- use it as the qualified namespace for the next parts, recursively.
--   If we get 'Nothing' at any point, it means it's unbound and we should stop
-- the expansion.
--   As an example, W.X will be expanded like below. The parentheses indicate
-- which name we're currenly expanding.
--   (W).X -> (A).X -> A.(X) -> A.(Y).Z -> A.Y.(Z) -> A.Y.Z
expandModuleAlias :: Namespace -> ScopeM (Maybe Namespace)
expandModuleAlias (Namespace Seq.Empty) = Just . qualified <$> askNamespace
expandModuleAlias (Namespace (base Seq.:<| nested)) =
  resolveModuleAlias base >>= \case
    -- Unbound name, stop.
    Nothing -> pure Nothing
    -- The base is already expanded, we can continue with the nested modules.
    Just resolved -> mapNamespace (const resolved) $ expandModuleAlias $ Namespace nested

withScope :: ExtendedDeclRef -> ScopeM a -> ScopeM a
withScope ExtendedDeclRef{edrDeclRef = DeclRef{..}, ..} =
  mapInScope (HashMap.insert (RefKey edrNamespace drName level) ref)
  where
    (ref, level) = case edrRefKind of
      OrdinaryRef l -> (InScopeOrdinaryRef drRange, l)
      ModuleAliasRef rhs -> (InScopeModuleAliasRef drRange rhs, ModuleLevel)

withScopes :: [ExtendedDeclRef] -> ScopeM a -> ScopeM a
withScopes declRefs m = foldl' (flip withScope) m declRefs

inScopeRefToRef :: Level -> InScopeRef -> (Range, RefKind)
inScopeRefToRef level = \case
  InScopeOrdinaryRef range -> (range, OrdinaryRef level)
  InScopeModuleAliasRef range ref -> (range, ModuleAliasRef ref)

insertRef :: Level -> Text -> PreprocessedRange -> ScopeM ()
insertRef level name (PreprocessedRange refRange) = do
  namespace <- askNamespace
  lookupInOuterModules level namespace name >>= \case
    Nothing -> pass
    Just (inScopeRef, declNamespace) -> do
      let
        (declRange, refKind) = inScopeRefToRef level inScopeRef
        ref = ExtendedDeclRef (DeclRef name declRange) declNamespace refKind
      modify $ Map.adjust (sdRefs %~ (refRange :)) ref

-- | Given a namespace @A.B@, attempts to find a name @n@ as @A.B.n@. If the
-- namespace in non-qualified, then it continues searching as @A.n@, and then as
-- @n@, in this order, stopping the lookup on the first occurrence that was
-- found, if any. Returns the namespace in which it was found.
lookupInOuterModules :: Level -> Qualified -> Text -> ScopeM (Maybe (InScopeRef, Namespace))
lookupInOuterModules level namespace name = do
  inScope <- askInScope
  let namespaceName = unqualified namespace <> qualified namespace
  case HashMap.lookup (RefKey namespaceName name level) inScope of
    Just inScopeRef -> pure $ Just (inScopeRef, namespaceName)
    Nothing         -> case namespace of
      Qualified (Namespace uns) qns -> case uns of
        Seq.Empty         -> pure Nothing
        initUns Seq.:|> _ -> lookupInOuterModules level (Qualified (Namespace initUns) qns) name

getEnv :: LIGO ParsedInfo -> ScopeM ScopeForest
getEnv info = do
  trees <- fmap getTree <$> walk info
  decls <- get
  let
    sf = ScopeForest
      (fmap (first (Set.map edrDeclRef)) <$> maybeToList trees)
      (Map.mapKeys edrDeclRef decls)
  pure sf

type ScopeRet =
  ( ExtendedScopeTree
  , [ExtendedDeclRef]
  )

getTree :: ScopeRet -> ExtendedScopeTree
getTree = view _1
{-# INLINE getTree #-}

-- | A helper function for @walk'@ that takes an ordinary @LIGO@ cofree node
-- rather than the individual field and layer components.
walk :: LIGO ParsedInfo -> ScopeM (Maybe ScopeRet)
walk (r :< s) = walk' r s

class HasGo (f :: Data.Kind.Type -> Data.Kind.Type) where
  -- | Recursively traverse an AST node, building its scope tree and collecting
  -- all declarations that were seen on the way.
  walk' :: Product ParsedInfo -> f (LIGO ParsedInfo) -> ScopeM (Maybe ScopeRet)

instance HasGo Name where
  walk' r (Name name) = walkName TermLevel r name

instance HasGo QualifiedName where
  walk' _ QualifiedName {..} = do
    walk qnSource
    mapM_ walk qnPath
    pure Nothing

instance HasGo Pattern where
  walk' _ pattern' = do
    case pattern' of
      IsConstr name mpat -> do
        void (walk name)
        whenJust mpat $ void . walk
      IsConstant _ -> pass
      IsVar name -> void (walk name)
      IsCons head' tail' -> do
        void (walk head')
        void (walk tail')
      IsAnnot pat typ -> do
        void (walk pat)
        void (walk typ)
      IsWildcard -> pass
      IsSpread name -> void (walk name)
      IsList pats -> mapM_ walk pats
      IsTuple pats -> mapM_ walk pats
      IsRecord rfps -> mapM_ walk rfps
      IsParen pat -> void (walk pat)
    pure Nothing

instance HasGo RecordFieldPattern where
  walk' _ = \case
    IsRecordField _ var -> walk var
    IsRecordCapture var -> walk var

instance HasGo Constant where
  walk' _ = \case
    CInt    {} -> pure Nothing
    CNat    {} -> pure Nothing
    CString {} -> pure Nothing
    CFloat  {} -> pure Nothing
    CBytes  {} -> pure Nothing
    CTez    {} -> pure Nothing

instance HasGo FieldAssignment where
  walk' _ = \case
    FieldAssignment _ expr -> walk expr
    Spread name -> walk name
    Capture name -> walk name

instance HasGo MapBinding where
  walk' _ (MapBinding expr1 expr2) = walk expr1 >> walk expr2 >> pure Nothing

instance HasGo Alt where
  walk' _ (Alt pat expr) = do
    void (walk pat)
    walk expr

instance HasGo Expr where
  walk' r = \case
    Let statements body -> do
      walk statements >>= \case
        Nothing -> do
          bodyTree <- fmap getTree <$> walk body
          pure ((, []) <$> bodyTree)
        Just (statementsTree, decls) -> do
          scopes <- withScopes decls (walk body)
          let bodyTree = getTree <$> scopes
          let scopeTrees = [statementsTree, (Set.fromList decls, getRange $ extract body) :< maybeToList bodyTree]
          pure $ Just ((Set.empty, getRange r) :< scopeTrees, [])
    Apply func params -> do
      funcTree <- maybeToList . fmap getTree <$> walk func
      paramTrees <- map getTree <$> wither walk params
      pure $ Just ((Set.empty, getRange r) :< funcTree ++ paramTrees, [])
    Constant _ -> pure Nothing
    Ident _ -> pure Nothing
    BinOp left _ right -> do
      walk left
      walk right
      pure Nothing
    UnOp _ child -> Nothing <$ walk child
    Op _ -> pure Nothing
    Record assignments  -> do
      for_ assignments $ \case
        (layer -> Just (Assign _ expr')) -> void (walk expr')
        _ -> pass
      pure Nothing
    If clause true false -> do
      walk clause
      walk true
      whenJust false $ void . walk
      pure Nothing
    Ternary clause true false -> do
      walk clause
      walk true
      walk false
    Assign name expr' -> do
      void (walk name)
      walk expr'
    AssignOp name _ expr -> do
      walk name
      walk expr
      pure Nothing
    List xs -> mapM_ walk xs >> pure Nothing
    ListAccess name indices -> do
      walk name
      mapM_ walk indices
      pure Nothing
    Set exprs -> mapM_ walk exprs >> pure Nothing
    Tuple xs -> mapM_ walk xs >> pure Nothing
    Annot expr' typ -> do
      walk expr'
      walk typ
      pure Nothing
    Attrs {} -> pure Nothing
    BigMap bindings -> mapM_ walk bindings >> pure Nothing
    Map bindings -> mapM_ walk bindings >> pure Nothing
    Remove expr1 _ expr2 -> walk expr1 >> walk expr2 >> pure Nothing
    Case expr' alts -> do
      void (walk expr')
      scopeTrees <- map getTree <$> wither walk alts
      pure $ Just ((Set.empty, getRange r) :< scopeTrees, [])
    Skip -> pure Nothing
    Break -> pure Nothing
    Return line -> maybe (pure Nothing) walk line
    SwitchStm expr' cases -> do
      void (walk expr')
      scopeTrees <- map getTree <$> wither walk cases
      pure $ Just ((Set.empty, getRange r) :< scopeTrees, [])
    ForLoop name begin end step body -> do
      void (walk name)
      void (walk begin)
      void (walk end)
      whenJust step $ void . walk
      st <- fmap getTree <$> walk body
      pure $ fmap (, []) st
    WhileLoop clause body -> do
      walk clause
      st <- fmap getTree <$> walk body
      pure $ fmap (, []) st
    ForOfLoop expr1 expr2 body -> do
      walk expr1
      walk expr2
      st <- fmap getTree <$> walk body
      pure $ fmap (, []) st
    Seq decls -> do
      (sts, refs) <- processSequence decls
      pure $ Just ((Set.empty, getRange r) :< sts, Set.toList refs)
    Block {} -> pure Nothing
    Lambda params tys typ body -> do
      tyRefs <- referencesForTypeParams tys
      withScopes tyRefs do
        mapM_ walk tys
        paramRefs <- referencesForParams params typ
        subforest <- fmap (fmap getTree) do
          void (maybe (pure Nothing) walk typ)
          for_ (zip (tyRefs <> paramRefs) (tys <> params)) \(pr, p) ->
            withScope pr (walk p)
          withScopes paramRefs (walk body)
        pure $ Just
          ( (Set.fromList paramRefs <> Set.fromList tyRefs, getRange r) :< maybeToList subforest
          , []
          )
    ForBox name mname2 coll expr1 expr2 -> do
      declRefs <- referencesForParams (catMaybes [Just name, mname2]) Nothing
      withScopes declRefs $ do
        void (walk name)
        whenJust mname2 $ void . walk
      walk coll
      walk expr1
      subforest <- fmap (fmap getTree) $ withScopes declRefs $ walk expr2
      pure $ Just ((Set.fromList declRefs, getRange r) :< maybeToList subforest, [])
    Patch expr1 expr2 -> do
      void (walk expr1)
      walk expr2
    RecordUpd name assignments -> do
      walk name
      mapM_ walk assignments
      pure Nothing
    CodeInj {} -> pure Nothing
    Paren expr' -> walk expr'

instance HasGo Collection where
  walk' _ = \case
    CList -> pure Nothing
    CMap  -> pure Nothing
    CSet  -> pure Nothing

walkTwoFields
  :: LIGO ParsedInfo
  -> Maybe (LIGO ParsedInfo)
  -> ScopeM (Maybe ScopeRet)
walkTwoFields a b = Nothing <$ (walk a *> whenJust b (void . walk))

instance HasGo TField where
  walk' _ (TField name mtype) = walkTwoFields name mtype

instance HasGo Variant where
  walk' _ (Variant name mtype) = walkTwoFields name mtype

instance HasGo Skeleton.Type where
  walk' _ = \case
    TArrow typ1 typ2 -> walk typ1 >> walk typ2 >> pure Nothing
    TRecord fields' -> mapM_ walk fields' >> pure Nothing
    TSum variants -> mapM_ walk variants >> pure Nothing
    TProduct types -> mapM_ walk types >> pure Nothing
    TApply name types -> walk name >> mapM_ walk types >> pure Nothing
    TString {} -> pure Nothing
    TWildcard -> pure Nothing
    TVariable var -> Nothing <$ walk var
    TParen typ -> walk typ

referencesForParams :: [LIGO ParsedInfo] -> Maybe (LIGO ParsedInfo) -> ScopeM [ExtendedDeclRef]
referencesForParams params returnType = foldMapM go params
  where
    go :: LIGO ParsedInfo -> ScopeM [ExtendedDeclRef]
    go = \case
      -- @const a : b@ -> @a : b@
      -- @const a@ -> @a@
      (match -> Just (r, BParameter pat mType)) ->
        go $ maybe id (\t p -> fastMake r $ IsAnnot p t) mType pat
      node@(match -> Just (r, pattern')) -> case pattern' of
        -- @(pat) : typ@ -> @pat : typ@
        IsAnnot (layer -> Just (IsParen pat)) typ ->
          go $ fastMake r $ IsAnnot pat typ
        -- @pat : (typ)@ -> @pat : typ@
        IsAnnot pat (layer -> Just (TParen typ)) ->
          go $ fastMake r $ IsAnnot pat typ
        -- @pat1, pat2 : typ1 * typ2@ -> @(pat1 : typ1), (pat2 : typ2)@
        IsAnnot (layer -> Just (IsTuple pats)) (layer -> Just (TProduct typs)) ->
          foldMapM go $ zipWith (\p t -> fastMake r $ IsAnnot p t) pats typs
        IsAnnot _pat typ ->
          mkDecl (valueScopedDecl [] node (Just typ) Nothing) >>=
            maybe (pure []) (fmap (: []) . insertScope (OrdinaryRef TermLevel))
        IsTuple xs -> foldMapM go xs
        IsParen x -> go x
        IsVar x -> go x
        IsRecord rfps ->
          flip foldMapM rfps $ layer >>> maybe (pure []) \case
            IsRecordField _ var -> go var
            IsRecordCapture var -> go var
        IsConstr _ _ -> pure []
        IsConstant _ -> pure []
        IsCons hd tl -> foldMapM go [hd, tl]
        IsWildcard -> pure []
        IsSpread _ -> pure []
        IsList xs -> foldMapM go xs
      node@(layer -> Just (NameDecl _)) ->
        mkDecl (valueScopedDecl [] node returnType Nothing) >>=
          maybe (pure []) (fmap (:[]) . insertScope (OrdinaryRef TermLevel))
      _ -> pure []

referencesForTypeParams :: [LIGO ParsedInfo] -> ScopeM [ExtendedDeclRef]
referencesForTypeParams = foldMapM go
  where
    go :: LIGO ParsedInfo -> ScopeM [ExtendedDeclRef]
    go = \case
      (layer -> Just (TVariable tvar)) -> go tvar
      (match -> Just (r, TypeVariableName name)) -> do
        scopedDecl <- mkTypeVariableScope name (getRange r)
        (: []) <$> insertScope (OrdinaryRef TypeLevel) scopedDecl
      _ -> pure []

-- | Removes all immediate layers of parentheses from the given node. That is,
-- all 'Paren', 'TParen', and 'IsParen' nodes.
peel :: LIGO xs -> LIGO xs
peel = \case
  (layer -> Just (Paren   exp)) -> peel exp
  (layer -> Just (TParen  typ)) -> peel typ
  (layer -> Just (IsParen pat)) -> peel pat
  node -> node

walkLet
  :: Product ParsedInfo  -- ^ The information about this node.
  -> Bool  -- ^ Whether this is recursive ('True') or not ('False').
  -> LIGO ParsedInfo  -- ^ The name node of this function. It may be any pattern.
  -> [LIGO ParsedInfo]  -- ^ Parametric types declared by this node.
  -> Maybe (LIGO ParsedInfo)  -- ^ The optional return type of this node.
  -> Maybe (LIGO ParsedInfo)  -- ^ The optional body of this declaration.
  -> ScopeM (Maybe ScopeRet)
-- @let rec (type tys) pat : typ = fun (type tys') params : typ' -> body@
walkLet r isRec name tys typ (Just (peel -> layer -> Just (Lambda params tys' typ' body))) = do
  let returnType = typ' <|> typ
  tyRefs <- referencesForTypeParams tys
  withScopes tyRefs $ mkDecl (functionScopedDecl [] name params returnType (Just body))
    >>= maybe (pure Nothing) \functionDecl -> do
      traverse_ walk tys
      functionRef <- insertScope (OrdinaryRef TermLevel) functionDecl
      tyRefs' <- referencesForTypeParams tys'
      withScopes tyRefs' do
        traverse_ walk tys'
        paramRefs <- referencesForParams params returnType
        subforest <- getTree <<$>> do
          void $ withScope functionRef (walk name)
          for_ (zip paramRefs params) \(pr, p) ->
            withScope pr (walk p)
          isJsLIGO <- (== Js) <$> askDialect  -- Functions in JsLIGO are always recursive.
          withScopes (bool id (functionRef :) (isRec || isJsLIGO) paramRefs) (walk body)
        whenJust returnType $ void . walk
        pure $ Just
          ( (Set.singleton functionRef, getRange r) :<
            [ (Set.fromList (tyRefs <> tyRefs'), getRange r) :< []
            , (Set.fromList paramRefs, getRange r) :< maybeToList subforest
            ]
          , [functionRef]
          )
walkLet r _isRec pat tys typ mexpr = do
  tyRefs <- referencesForTypeParams tys
  withScopes tyRefs do
    traverse_ walk tys
    refs <- referencesForParams [pat] typ
    void $ withScopes refs (walk pat)
    whenJust typ $ void . walk
    subforest <- maybe (pure Nothing) (fmap (fmap getTree) . walk) mexpr
    pure $ Just
      ( (Set.fromList refs, getRange r) :<
        ( ((Set.fromList tyRefs, getRange r) :< [])
        : maybeToList subforest
        )
      , refs
      )

instance HasGo Binding where
  -- A few cases that need to be handled. Note that arbitrary parentheses may be
  -- inserted _anywhere_.
  --
  -- [X] let f (a : t, b : u) : v = ...
  -- [X] let f (a, t : b * u) : v = ...
  -- [X] let f (a : t) (b : u) : v = ...
  -- [X] let f = fun (a : t, b : u) : v -> ...
  -- [X] let f = fun (a, b : t * u) : v -> ...
  -- [X] let f = fun (a : t) (b : u) : v -> ...
  -- [ ] let f : (t * u) -> v = fun (a, b) -> ...
  -- [ ] let f : t -> u -> v = fun a b -> ...
  walk' r = \case
    -- We convert a function to an equivalent written in terms of lambdas in
    -- order to reuse 'walkLet'. For example:
    --
    -- > let rec f (type t u) (x : t) : u = y
    --
    -- Becomes:
    --
    -- > let rec f = fun (type t u) (x : t) : u -> y
    --
    -- Note that LIGO doesn't really accept the above format of recursive
    -- functions (the return type must be in the header), but that is not a
    -- problem for us because this is only for scoping and won't be called in
    -- LIGO.
    --
    -- Note that this doesn't cover every case, for example, what if the 'body'
    -- is a lambda as well? But I hope this is a good enough approximation for
    -- most cases.
    BFunction isRec name tys params typ body -> do
      let body' = fastMake r $ Lambda params tys typ body
      walkLet r isRec name [] Nothing (Just body')
    BParameter name typ -> do
      void (walk name)
      maybe (pure Nothing) walk typ
    BVar pat tys ty mexpr ->
      walkLet r False pat tys ty mexpr
    BConst isRec pat tys ty mexpr ->
      walkLet r isRec pat tys ty mexpr
    BTypeDecl name mparams expr -> do
      let scopeVariant, scopeTField :: LIGO ParsedInfo -> ScopeM (Maybe ScopeRet)
          scopeVariant = \case
            (layer -> Just (Variant vname vtype)) ->
              mkDecl (functionScopedDecl [] vname (maybeToList vtype) (Just name) Nothing)
                >>= maybe (pure Nothing) \decl -> do
                  ref <- insertScope (OrdinaryRef TermLevel) decl
                  withScope ref (walk vname)
                  pure $ Just ((Set.singleton ref, getRange r) :< [], [ref])
            _ -> pure Nothing
          scopeTField = \case
            (layer -> Just (TField fname ftype)) ->
              mkDecl (valueScopedDecl [] fname ftype Nothing)
                >>= maybe (pure Nothing) \decl -> do
                  ref <- insertScope (OrdinaryRef TermLevel) decl
                  withScope ref (walk fname)
                  whenJust ftype $ void . walk
                  pure $ Just ((Set.singleton ref, getRange r) :< [], [ref])
            _ -> pure Nothing

      mkDecl (typeScopedDecl (getElem r) name mparams expr) >>=
        maybe (pure Nothing) \scopedDecl -> do
          declRef <- insertScope (OrdinaryRef TypeLevel) scopedDecl
          let params = case mparams of
                Just (layer -> Just (Skeleton.QuotedTypeParams ps)) -> ps
                Just (layer -> Just (Skeleton.QuotedTypeParam p)) -> [p]
                _ -> []

          paramRefs <- referencesForTypeParams params
          void (withScope declRef (walk name))
          for_ (zip paramRefs params) $ \(pr, p) ->
            withScope pr (walk p)
          (subforest, fromMaybe [] -> subRefs) <-
            fmap NE.unzip $ withScopes (declRef:paramRefs) $ case expr of
              (layer -> Just (TSum variants)) -> do
                (sts, concat -> refs) <- unzip <$> wither scopeVariant (toList variants)
                pure $ Just ((Set.empty, getRange r) :< sts, refs)
              (layer -> Just (TRecord fields)) -> do
                (sts, concat -> refs) <- unzip <$> wither scopeTField fields
                pure $ Just ((Set.empty, getRange r) :< sts, refs)
              (layer -> Just (ModuleAccess path field)) ->
                Nothing <$ walkModuleAccess path (Just field)
              _ -> pure Nothing
          pure $ Just
            ( (Set.fromList (declRef : paramRefs), getRange r) :< maybeToList subforest
            , declRef : paramRefs ++ subRefs
            )
    BAttribute    {} -> pure Nothing
    BInclude      {} -> pure Nothing
    BImport       {} -> pure Nothing
    BModuleDecl name decls ->
      mkDecl (moduleScopedDecl [] name decls)
        >>= maybe (pure Nothing) \moduleDecl -> do
          let moduleName = moduleDecl ^. sdName
          (declTree, declRefs) <-
            mapQualified (mapInUnqualified (\(Namespace ns) -> Namespace (ns |> moduleName)))
            $ processSequence decls
          moduleRef <- insertScope (OrdinaryRef ModuleLevel) moduleDecl
          void $ withScope moduleRef $ walk name
          pure $ Just
            ( (Set.singleton moduleRef, getRange r) :< declTree
            , moduleRef : Set.toList declRefs
            )
    BModuleAlias name path ->
      mkDecl (moduleScopedDecl [] name path)
        >>= maybe (pure Nothing) \moduleDecl -> do
          walkModuleAccess path Nothing

          declRefM <- runMaybeT do
            -- Extract the original module alias path and expand it to resolve
            -- all aliases.
            ModuleAlias alias <- hoistMaybe (moduleDecl ^? sdSpec . _ModuleSpec . mdsInit)
            Namespace expandedAlias <- MaybeT $ expandModuleAlias alias
            (aliasNamespace, drName) <- hoistMaybe $ case expandedAlias of
              Seq.Empty   -> Nothing
              h Seq.:|> t -> Just (h, t)

            -- Lookup the module name that we resolved to and return its cached
            -- reference.
            (ref, edrNamespace) <- MaybeT do
              currentNamespace <- askNamespace
              lookupInOuterModules
                ModuleLevel
                (mapInQualified (<> Namespace aliasNamespace) currentNamespace)
                drName
            let (drRange, edrRefKind) = inScopeRefToRef ModuleLevel ref
            pure ExtendedDeclRef{edrDeclRef = DeclRef{..}, ..}

          moduleRef <- insertScope (ModuleAliasRef declRefM) moduleDecl
          void $ withScope moduleRef $ walk name
          pure $ Just
            ( (Set.singleton moduleRef, getRange r) :< []
            , [moduleRef]
            )

instance HasGo RawContract where
  walk' r (RawContract statements) = do
    xs <- view _1 <$> processSequence statements
    pure $ Just ((Set.empty, getRange r) :< xs, [])

walkName :: Level -> Product ParsedInfo -> Text -> ScopeM (Maybe ScopeRet)
walkName level r name = Nothing <$ insertRef level name (getElem @PreprocessedRange r)

instance HasGo TypeName where
  walk' r (TypeName name) = walkName TypeLevel r name

instance HasGo TypeVariableName where
  walk' r (TypeVariableName name) = walkName TypeLevel r name

instance HasGo FieldName where
  walk' _ _ = pure Nothing

instance HasGo Verbatim where
  walk' _ _ = pure Nothing

instance HasGo Attr where
  walk' _ _ = pure Nothing

instance HasGo Error where
  walk' _ _ = pure Nothing

instance HasGo Ctor where
  walk' r (Ctor name) = walkName TermLevel r name

instance HasGo NameDecl where
  walk' r (NameDecl decl) = walkName TermLevel r decl

instance HasGo Preprocessor where
  walk' _ _ = pure Nothing

instance HasGo PreprocessorCommand where
  walk' _ _ = pure Nothing

instance HasGo PatchableExpr where
  walk' _ (PatchableExpr _ expr) = Nothing <$ walk expr

instance HasGo ModuleName where
  walk' r (ModuleName name) = walkName ModuleLevel r name

-- | Given a module access such as `A.B.C` ('Nothing' case) or `A.B.C.x` ('Just'
-- case), add references to each module in the chain and maybe walk over the
-- qualified accessor.
walkModuleAccess :: [LIGO ParsedInfo] -> Maybe (LIGO ParsedInfo) -> ScopeM ()
walkModuleAccess path accessorM = go path
  where
    go [] = whenJust accessorM $ void . walk
    go (modName : modNames) = do
      void $ walk modName
      getName modName >>= \case
        Left _ -> pass
        Right (_, namespacePart) -> whenJustM (resolveModuleAlias namespacePart) $
          \resolved -> mapNamespace (const resolved) (go modNames)

instance HasGo ModuleAccess where
  walk' _ (ModuleAccess path accessor) = Nothing <$ walkModuleAccess path (Just accessor)

instance HasGo Skeleton.QuotedTypeParams where
  walk' r = \case
    Skeleton.QuotedTypeParam  ty  -> walkQuotedTypeParams [ty]
    Skeleton.QuotedTypeParams tys -> walkQuotedTypeParams tys
    where
      walkQuotedTypeParams tys = do
        tyRefs <- referencesForTypeParams tys
        withScopes tyRefs do
          traverse_ walk tys
          pure $ Just ((Set.fromList tyRefs, getRange r) :< [], [])

instance HasGo CaseOrDefaultStm where
  walk' r = \case
    CaseStm expr statements -> do
      void (walk expr)
      scopeTrees <- view _1 <$> processSequence statements
      pure $ Just ((Set.empty, getRange r) :< scopeTrees, [])
    DefaultStm statements -> do
      scopeTrees <- view _1 <$> processSequence statements
      pure $ Just ((Set.empty, getRange r) :< scopeTrees, [])

instance HasGo (Sum RawLigoList) where
  walk' r = apply @HasGo (walk' r)

processSequence :: [LIGO ParsedInfo] -> ScopeM ([ExtendedScopeTree], Set.Set ExtendedDeclRef)
processSequence = go Set.empty
  where
    go prevRefs [] = pure ([], prevRefs)
    go prevRefs (x : xs) = do
      let
        addToTopLevel :: ExtendedScopeTree -> Set.Set ExtendedDeclRef -> ExtendedScopeTree
        addToTopLevel ((refs, range) :< rest) refs' = (Set.union refs refs', range) :< rest

      walk x >>= \case
        Nothing -> do
          (nextTrees, nextDecls) <- go prevRefs xs
          pure (((prevRefs, getRange (extract x)) :< []) : nextTrees, nextDecls)
        Just (scopeTree, refs) -> do
          let newRefs = Set.union prevRefs (Set.fromList refs)
          (nextTrees, nextDecls) <- withScopes refs $ go newRefs xs
          pure (addToTopLevel scopeTree prevRefs : nextTrees, nextDecls)

loop :: Functor f => (Cofree f a -> Cofree f a) -> Cofree f a -> Cofree f a
loop go = aux
  where
    aux (r :< fs) = go $ r :< fmap aux fs

loopM_ :: (Applicative t, Foldable f) => (Cofree f a -> t ()) -> (Cofree f a -> t ())
loopM_ go = aux
  where
    aux (r :< fs) = Foldable.for_ fs aux *> go (r :< fs)

loopM
  :: (Monad m, Traversable f)
  => (Cofree f a -> m (Cofree f a)) -> (Cofree f a -> m (Cofree f a))
loopM go = aux
  where
    aux (r :< fs) = go . (r :<) =<< traverse aux fs

functionScopedDecl
  :: ( PPableLIGO info
     , Contains PreprocessedRange info
     )
  => [Text] -- ^ documentation comments
  -> LIGO info -- ^ name node
  -> [LIGO info] -- ^ parameter nodes
  -> Maybe (LIGO info) -- ^ type node
  -> Maybe (LIGO info) -- ^ function body node, optional for type constructors
  -> ScopeM (Either TreeDoesNotContainName ScopedDecl)
functionScopedDecl docs nameNode paramNodes typ body = do
  dialect <- askDialect
  namespace <- askNamespace
  getName nameNode <<&>> \(PreprocessedRange origin, name) ->
    let
      _vdsInitRange = getRange <$> body
      _vdsParams = pure $ runReader (parseParameters paramNodes) dialect
      _vdsTspec = runReader (traverse parseTypeDeclSpecifics typ) dialect
    in
    ScopedDecl
      { _sdName = name
      , _sdOrigin = origin
      , _sdRefs = []
      , _sdDoc = docs
      , _sdDialect = dialect
      , _sdSpec = ValueSpec ValueDeclSpecifics{ .. }
      , _sdNamespace = unqualified namespace
      }

valueScopedDecl
  :: ( PPableLIGO info
     , Contains PreprocessedRange info
     )
  => [Text] -- ^ documentation comments
  -> LIGO info -- ^ name node
  -> Maybe (LIGO info) -- ^ type node
  -> Maybe (LIGO info) -- ^ initializer node
  -> ScopeM (Either TreeDoesNotContainName ScopedDecl)
valueScopedDecl docs nameNode typ body = do
  dialect <- askDialect
  namespace <- askNamespace

  let
    _vdsInitRange = getRange <$> body
    _vdsParams = Nothing
    _vdsTspec = runReader (traverse parseTypeDeclSpecifics typ) dialect

  getName nameNode <<&>> \(PreprocessedRange origin, name) ->
    ScopedDecl
      { _sdName = name
      , _sdOrigin = origin
      , _sdRefs = []
      , _sdDoc = docs
      , _sdDialect = dialect
      , _sdSpec = ValueSpec ValueDeclSpecifics{ .. }
      , _sdNamespace = unqualified namespace
      }

typeScopedDecl
  :: ( PPableLIGO info
     , Contains PreprocessedRange info
     )
  => [Text]  -- ^ documentation comments
  -> LIGO info  -- ^ name node
  -> Maybe (LIGO info)  -- ^ type parameters
  -> LIGO info  -- ^ type body node
  -> ScopeM (Either TreeDoesNotContainName ScopedDecl)
typeScopedDecl docs nameNode mparams body = do
  dialect <- askDialect
  namespace <- askNamespace
  let paramsSpec = mparams >>= usingReader dialect . parseQuotedTypeParams
  let typeSpec = runReader (parseTypeDeclSpecifics body) dialect
  getTypeName nameNode <<&>> \(PreprocessedRange origin, name) ->
    ScopedDecl
      { _sdName = name
      , _sdOrigin = origin
      , _sdRefs = []
      , _sdDoc = docs
      , _sdDialect = dialect
      , _sdSpec = TypeSpec paramsSpec typeSpec
      , _sdNamespace = unqualified namespace
      }

moduleScopedDecl
  :: ( PPableLIGO info
     , Contains PreprocessedRange info
     )
  => [Text] -- ^ documentation comments
  -> LIGO info -- ^ node name
  -> [LIGO info] -- ^ body node
  -> ScopeM (Either TreeDoesNotContainName ScopedDecl)
moduleScopedDecl docs nameNode body = do
  dialect <- askDialect
  namespace <- askNamespace
  getName nameNode <<&>> \(PreprocessedRange origin, name) ->
    ScopedDecl
      { _sdName = name
      , _sdOrigin = origin
      , _sdRefs = []
      , _sdDoc = docs
      , _sdDialect = dialect
      , _sdSpec = ModuleSpec ModuleDeclSpecifics
        { _mdsInitRange = getRange nameNode
        , _mdsInit = runReader (parseModule body) dialect
        , _mdsName = name
        }
      , _sdNamespace = unqualified namespace
      }

mkTypeVariableScope :: Text -> Range -> ScopeM ScopedDecl
mkTypeVariableScope name range = do
  dialect <- askDialect
  namespace <- askNamespace
  let tspec = TypeDeclSpecifics range $ VariableType (TypeVariable name)
  pure ScopedDecl
    { _sdName = name
    , _sdOrigin = range
    , _sdRefs = []
    , _sdDoc = []
    , _sdDialect = dialect
    , _sdSpec = TypeSpec Nothing tspec
    , _sdNamespace = unqualified namespace
    }

mkDecl :: Alternative f => ScopeM (Either TreeDoesNotContainName a) -> ScopeM (f a)
mkDecl = (either ((empty <$) . tell . Endo . (<>) . pure) (pure . pure) =<<)

select
  :: ( PPableLIGO info
     , MonadReader ScopeEnv m
     , Contains PreprocessedRange info
     )
  => Text
  -> [Visit RawLigoList (Product info) (Writer [LIGO info])]
  -> LIGO info
  -> m (Either TreeDoesNotContainName (PreprocessedRange, Text))
select what handlers t = do
  dialect <- view seDialect
  pure
    $ maybe
        (Left $ TreeDoesNotContainName (lppDialect dialect t) (extractRange $ getElem $ extract t) what)
        (Right . (getElem . extract &&& ppToText))
    $ listToMaybe
    $ execWriter
    $ visit' handlers
      t
  where
    extractRange (PreprocessedRange r) = r

getName
  :: ( PPableLIGO info
     , MonadReader ScopeEnv m
     , Contains PreprocessedRange info
     )
  => LIGO info
  -> m (Either TreeDoesNotContainName (PreprocessedRange, Text))
getName = select "name"
  [ Visit \r (NameDecl t) ->
      tell [fastMake r (Name t)]
  , Visit \r (Ctor t) ->
      tell [fastMake r (Name t)]
  , Visit \r (FieldName t) ->
      tell [fastMake r (Name t)]
  , Visit \r (ModuleName t) ->
      tell [fastMake r (Name t)]
  , Visit \r -> \case
      IsWildcard -> tell [fastMake r (Name "_")]
      _ -> tell []
  ]

getTypeName
  :: ( PPableLIGO info
     , MonadReader ScopeEnv m
     , Contains PreprocessedRange info
     )
  => LIGO info
  -> m (Either TreeDoesNotContainName (PreprocessedRange, Text))
getTypeName = select "type name"
  [ Visit \r (TypeName t) ->
      tell [fastMake r (TypeName t)]
  ]
