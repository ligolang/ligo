module AST.Scope.Fallback
  ( Fallback
  , loop
  , loopM
  , loopM_
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Control.Lens ((%~), (^.))
import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Monad.Writer (Endo (..), Writer, WriterT, execWriter, mapWriterT, runWriterT, tell)

import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.Kind qualified (Type)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashMap.Lazy (HashMap)
import Data.List (foldl')
import Data.List.NonEmpty (unzip)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Duplo.Pretty (Doc, pp, ppToText)
import Duplo.Tree hiding (loop)
import Prelude hiding (unzip)
import Witherable (wither)

import AST.Pretty (PPableLIGO)
import AST.Scope.Common
import AST.Scope.ScopedDecl
  ( DeclarationSpecifics (..), ScopedDecl (..), Type (VariableType)
  , TypeDeclSpecifics (..), TypeVariable (..), ValueDeclSpecifics (..), sdName
  , sdOrigin, sdRefs
  )
import AST.Scope.ScopedDecl.Parser (parseParameters, parseTypeDeclSpecifics)
import AST.Skeleton hiding (Type, TypeParams (..))
import AST.Skeleton qualified as Skeleton (Type (..), TypeParams (..))
import Cli.Types
import Diagnostic (Message (..), MessageDetail (FromLanguageServer), Severity (..))
import Log (i)
import Parser
import Product
import Range
import Util (foldMapM)

data Fallback

data TreeDoesNotContainName =
  TreeDoesNotContainName
    Doc  -- ^ pprinted tree (used for simplifying purposes for not stacking
         -- type parameters for `ScopeM` which brings plethora of confusion)
    Range -- ^ location where the error has occurred
    Text -- ^ variable name
  deriving stock (Show)

toMsg :: TreeDoesNotContainName -> Message
toMsg (TreeDoesNotContainName tree range name) =
  Message (FromLanguageServer [i|Expected to find a #{name}, but got `#{tree}`|]) SeverityError range

instance HasLigoClient m => HasScopeForest Fallback m where
  scopeContract _ (FindContract src (SomeLIGO dialect ligo) msgs) = do
    (sf, (`appEndo` []) -> errs) <-
        liftIO
      $ flip runReaderT (HashMap.empty, dialect)
      $ flip evalStateT Map.empty
      $ runWriterT
      $ getEnv ligo
    let msgs' = map toMsg errs
    pure $ FindContract src sf (msgs <> msgs')

type ScopeM =
  StateT (Map DeclRef ScopedDecl)
  (ReaderT (HashMap Text Range, Lang) IO)

type ScopeM' a = WriterT (Endo [TreeDoesNotContainName]) ScopeM a

askInScope :: ScopeM (HashMap Text Range)
askInScope = fst <$> lift ask

askDialect :: ScopeM Lang
askDialect = snd <$> lift ask

mapInScope :: (HashMap Text Range -> HashMap Text Range) -> ScopeM' a -> ScopeM' a
mapInScope = mapWriterT . mapStateT . local . first

insertScope :: ScopedDecl -> ScopeM' DeclRef
insertScope scopedDecl = do
  let declRef = DeclRef (scopedDecl ^. sdName) (scopedDecl ^. sdOrigin)
  modify (Map.insert declRef scopedDecl)
  pure declRef

withScope :: DeclRef -> ScopeM' a -> ScopeM' a
withScope declRef = mapInScope (HashMap.insert (drName declRef) (drRange declRef))

withScopes :: [DeclRef] -> ScopeM' a -> ScopeM' a
withScopes declRefs m = foldl' (flip withScope) m declRefs

insertRef :: Text -> PreprocessedRange -> ScopeM' ()
insertRef name (PreprocessedRange refRange) = do
  inScope <- lift askInScope
  case HashMap.lookup name inScope of
    Nothing -> pure ()
    Just declRange -> modify $
      Map.adjust (sdRefs %~ (refRange:)) (DeclRef name declRange)

getEnv :: LIGO ParsedInfo -> ScopeM' ScopeForest
getEnv info = do
  trees <- fmap fst <$> walk info
  decls <- get
  let sf = ScopeForest (maybeToList trees) decls
  pure sf

walk :: LIGO ParsedInfo -> ScopeM' (Maybe (ScopeTree, [DeclRef]))
walk (r :< s) = walk' r s

class HasGo (f :: Data.Kind.Type -> Data.Kind.Type) where
  walk' :: Product ParsedInfo -> f (LIGO ParsedInfo) -> ScopeM' (Maybe (ScopeTree, [DeclRef]))

instance HasGo Name where
  walk' r (Name name) =
    Nothing <$ insertRef name (getElem @PreprocessedRange r)

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
        maybe (pure ()) (void . walk) mpat
      IsConstant _ -> pure ()
      IsVar name -> void (walk name)
      IsCons head' tail' -> do
        void (walk head')
        void (walk tail')
      IsAnnot pat typ -> do
        void (walk pat)
        void (walk typ)
      IsWildcard -> pure ()
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
          bodyTree <- fmap (fmap fst) (walk body)
          pure ((,[]) <$> bodyTree)
        Just (statementsTree, decls) -> do
          bodyTree <- fmap (fmap fst) (withScopes decls (walk body))
          let scopeTrees = [statementsTree, (Set.fromList decls, getRange $ extract body) :< maybeToList bodyTree]
          pure $ Just $ (,[]) $
            (Set.empty, getRange r) :< scopeTrees
    Apply func params -> do
      funcTree <- maybeToList . fmap fst <$> walk func
      xs <- map unzip <$> mapM walk params
      let paramTrees = mapMaybe fst xs
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
      forM_ assignments $ \case
        (layer -> Just (Assign _ expr')) -> void (walk expr')
        _ -> pure ()
      pure Nothing
    If clause true false -> do
      walk clause
      walk true
      maybe (pure ()) (void . walk) false
      pure Nothing
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
      scopeTrees <- map fst <$> wither walk alts
      pure $ Just ((Set.empty, getRange r) :< scopeTrees, [])
    Skip -> pure Nothing
    Break -> pure Nothing
    Return line -> maybe (pure Nothing) walk line
    SwitchStm expr' cases -> do
      void (walk expr')
      scopeTrees <- map fst <$> wither walk cases
      pure $ Just ((Set.empty, getRange r) :< scopeTrees, [])
    ForLoop name begin end step body -> do
      void (walk name)
      void (walk begin)
      void (walk end)
      maybe (pure ()) (void . walk) step
      st <- fmap fst <$> walk body
      pure $ fmap (,[]) st
    WhileLoop clause body -> do
      walk clause
      st <- fmap fst <$> walk body
      pure $ fmap (,[]) st
    ForOfLoop expr1 expr2 body -> do
      walk expr1
      walk expr2
      st <- fmap fst <$> walk body
      pure $ fmap (,[]) st
    Seq decls -> do
      (sts, refs) <- processSequence Set.empty decls
      pure $ Just ((Set.empty, getRange r) :< sts, Set.toList refs)
    Block {} -> pure Nothing
    Lambda params typ body -> do
      paramRefs <- scopeParams params typ
      subforest <- fmap (fmap fst) $ do
        void (maybe (pure Nothing) walk typ)
        forM_ (zip paramRefs params) \(pr, p) ->
          withScope pr (walk p)
        withScopes paramRefs (walk body)
      pure $ Just ((Set.fromList paramRefs, getRange r) :< maybeToList subforest, [])
    ForBox name mname2 coll expr1 expr2 -> do
      declRefs <- scopeParams (catMaybes [Just name, mname2]) Nothing
      withScopes declRefs $ do
        void (walk name)
        maybe (pure ()) (void . walk) mname2
      walk coll
      walk expr1
      subforest <- fmap (fmap fst) $ withScopes declRefs $ walk expr2
      pure $ Just ((Set.fromList declRefs, getRange r) :< maybeToList subforest, [])
    Patch expr1 expr2 -> do
      void (walk expr1)
      walk expr2
    RecordUpd  name assignments -> do
      walk name
      mapM_ walk assignments
      pure Nothing
    Michelson {} -> pure Nothing
    Paren expr' -> walk expr'

instance HasGo Collection where
  walk' _ = \case
    CList -> pure Nothing
    CMap  -> pure Nothing
    CSet  -> pure Nothing

instance HasGo TField where
  walk' _ (TField name mtype) = do
    void (walk name)
    maybe (pure ()) (void . walk) mtype
    pure Nothing

instance HasGo Variant where
  walk' _ (Variant name mtype) = do
    void (walk name)
    maybe (pure ()) (void . walk) mtype
    pure Nothing

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

scopeParams :: [LIGO ParsedInfo] -> Maybe (LIGO ParsedInfo)-> ScopeM' [DeclRef]
scopeParams args ty = foldMapM go args
  where
    go :: LIGO ParsedInfo -> ScopeM' [DeclRef]
    go node = case node of
      (match -> Just (_, BParameter (layer -> Just (IsParen xs)) _)) ->
        scopeParams [xs] ty
      (match -> Just (_, BParameter (layer -> Just (IsTuple xs)) _)) ->
        foldMapM go xs
      (match -> Just (_, BParameter name mType)) -> do
        mkDecl (valueScopedDecl [] name mType Nothing)
          >>= maybe (pure []) (fmap (:[]) . insertScope)
      (match -> Just (_, IsAnnot pat _)) -> go pat
      (match -> Just (_, IsTuple xs)) -> foldMapM go xs
      (match -> Just (_, IsParen x)) -> go x
      (match -> Just (_, IsVar x)) -> go x
      (match -> Just (_, NameDecl _)) -> do
        mkDecl (valueScopedDecl [] node ty Nothing) >>=
          maybe (pure []) (fmap (:[]) . insertScope)
      (match -> Just (_, IsRecord rfps)) -> do
        flip foldMapM rfps $ \case
          (layer -> Just x) -> case x of
            IsRecordField _ var -> go var
            IsRecordCapture var -> go var
          _ -> pure []
      (match -> Just (r, TVariable (layer -> Just (TypeVariableName name)))) -> do
        scopedDecl <- lift (mkTypeVariableScope name (getRange r))
        (:[]) <$> insertScope scopedDecl
      _ -> pure []

instance HasGo Binding where
  walk' r = \case
    BFunction isRec name params typ body ->
      mkDecl (functionScopedDecl [] name params typ (Just body))
        >>= maybe (pure Nothing) \functionDecl -> do
          functionRef <- insertScope functionDecl
          paramRefs <- scopeParams params typ
          subforest <- fmap (fmap fst) $ do
            void $ withScope functionRef (walk name)
            mapM_ (withScopes paramRefs . walk) params
            maybe (pure ()) (void . walk) typ
            withScopes (bool id (functionRef :) isRec paramRefs) (walk body)
          pure $ Just ((Set.singleton functionRef, getRange r) :<
            [(Set.fromList paramRefs, getRange r) :< maybeToList subforest], [functionRef])

    BParameter name typ -> do
      void (walk name)
      maybe (pure Nothing) walk typ

    BVar pat typ mexpr ->
      mkDecl (valueScopedDecl [] pat typ mexpr)
        >>= maybe (pure Nothing) \scopedDecl -> do
          declRef <- insertScope scopedDecl
          subforest <- fmap (fmap fst) $ withScope declRef $ do
            void (walk pat)
            maybe (pure Nothing) walk mexpr
          maybe (pure ()) (void . walk) typ
          pure $ Just
            ((Set.singleton declRef, getRange r) :< maybeToList subforest, [declRef])

    BConst name typ (Just (layer -> Just (Lambda params _ body))) ->
      mkDecl (functionScopedDecl [] name params typ (Just body))
        >>= maybe (pure Nothing) \functionDecl -> do
          functionRef <- insertScope functionDecl
          paramRefs <- scopeParams params typ
          subforest <- fmap (fmap fst) $ do
            void $ withScope functionRef (walk name)
            forM_ (zip paramRefs params) \(pr, p) -> do
              withScope pr (walk p)
            withScopes (functionRef : paramRefs) (walk body)
          pure $ Just ((Set.singleton functionRef, getRange r) :<
            [(Set.fromList paramRefs, getRange r) :< maybeToList subforest], [functionRef])

    BConst (layer -> Just (IsParen (layer -> Just (IsTuple names)))) typ (Just (layer -> Just (Tuple vals))) -> do
      declRefs <- flip wither (zip names vals) \(name, val) ->
        mkDecl (valueScopedDecl (getElem r) name typ (Just val)) >>=
          maybe (pure Nothing) \scopedDecl -> do
            declRef <- insertScope scopedDecl
            withScope declRef (walk name)
            pure (Just declRef)
      maybe (pure ()) (void . walk) typ
      mapM_ walk vals
      pure $ Just ((Set.fromList declRefs, getRange r) :< [], declRefs)

    BConst (layer -> Just (IsTuple names)) typ (Just (layer -> Just (Tuple vals))) -> do
      declRefs <- flip wither (zip names vals) \(name, val) -> do
        mkDecl (valueScopedDecl (getElem r) name typ (Just val)) >>=
          maybe (pure Nothing) \scopedDecl -> do
            declRef <- insertScope scopedDecl
            withScope declRef (walk name)
            pure (Just declRef)
      maybe (pure ()) (void . walk) typ
      mapM_ walk vals
      pure $ Just ((Set.fromList declRefs, getRange r) :< [], declRefs)

    BConst pat ty mexpr -> do
      refs <- scopeParams [pat] ty
      void $ withScopes refs (walk pat)
      maybe (pure ()) (void . walk) ty
      subforest <- maybe (pure Nothing) (fmap (fmap fst) . walk) mexpr
      pure $ Just
          ((Set.fromList refs, getRange r) :< maybeToList subforest, refs)

    BTypeDecl name mparams expr -> do
      let scopeVariant :: LIGO ParsedInfo -> ScopeM' (Maybe (ScopeTree, [DeclRef]))
          scopeVariant = \case
            (layer -> Just (Variant vname vtype)) ->
              mkDecl (functionScopedDecl [] vname (maybeToList vtype) (Just name) Nothing)
                >>= maybe (pure Nothing) \decl -> do
                  ref <- insertScope decl
                  withScope ref (walk vname)
                  pure $ Just ((Set.singleton ref, getRange r) :< [], [ref])
            _ -> pure Nothing

      mkDecl (typeScopedDecl (getElem r) name expr) >>=
        maybe (pure Nothing) \scopedDecl -> do
          declRef <- insertScope scopedDecl
          let params = case mparams of
                Just (layer -> Just (Skeleton.TypeParams ps)) -> ps
                Just (layer -> Just (Skeleton.TypeParam p)) -> [p]
                _ -> []

          paramRefs <- scopeParams params Nothing
          void (withScope declRef (walk name))
          forM_ (zip paramRefs params) $ \(pr, p) ->
            withScope pr (walk p)
          (subforest, fromMaybe [] -> subRefs) <-
            fmap unzip $ withScopes (declRef:paramRefs) $ case expr of
              (layer -> Just (TSum variants)) -> do
                (sts, concat -> refs) <- unzip <$> wither scopeVariant variants
                pure $ Just ((Set.empty, getRange r) :< sts, refs)
              _ -> pure Nothing
          pure $ Just
            ((Set.fromList (declRef:paramRefs), getRange r) :< maybeToList subforest, declRef : paramRefs ++ subRefs)

    BAttribute    {} -> pure Nothing
    BInclude      {} -> pure Nothing
    BImport       {} -> pure Nothing
    BModuleDecl   {} -> pure Nothing
    BModuleAlias  {} -> pure Nothing

instance HasGo RawContract where
  walk' r (RawContract statements) = do
    xs <- fst <$> processSequence Set.empty statements
    pure $ Just ((Set.empty, getRange r) :< xs, [])

instance HasGo TypeName where
  walk' r (TypeName name) =
    Nothing <$ insertRef name (getElem @PreprocessedRange r)

instance HasGo TypeVariableName where
  walk' r (TypeVariableName decl) =
    Nothing <$ insertRef decl (getElem @PreprocessedRange r)

instance HasGo FieldName where
  walk' _ _ = pure Nothing

instance HasGo Verbatim where
  walk' _ _ = pure Nothing

instance HasGo Error where
  walk' _ _ = pure Nothing

instance HasGo Ctor where
  walk' r (Ctor name) =
    Nothing <$ insertRef name (getElem @PreprocessedRange r)

instance HasGo NameDecl where
  walk' r (NameDecl decl) =
    Nothing <$ insertRef decl (getElem @PreprocessedRange r)

instance HasGo Preprocessor where
  walk' _ _ = pure Nothing

instance HasGo PreprocessorCommand where
  walk' _ _ = pure Nothing

instance HasGo PatchableExpr where
  walk' _ (PatchableExpr _ expr) = Nothing <$ walk expr

instance HasGo ModuleName where
  walk' _ _ = pure Nothing

instance HasGo ModuleAccess where
  walk' _ _ = pure Nothing

instance HasGo Skeleton.TypeParams where
  walk' _ = \case
    Skeleton.TypeParam  {} -> pure Nothing
    Skeleton.TypeParams {} -> pure Nothing

instance HasGo CaseOrDefaultStm where
  walk' r = \case
    CaseStm expr statements -> do
      void (walk expr)
      scopeTrees <- fst <$> processSequence Set.empty statements
      pure $ Just ((Set.empty, getRange r) :< scopeTrees, [])
    DefaultStm statements -> do
      scopeTrees <- fst <$> processSequence Set.empty statements
      pure $ Just ((Set.empty, getRange r) :< scopeTrees, [])

instance HasGo (Sum RawLigoList) where
  walk' r = apply @HasGo (walk' r)

processSequence
  :: Set.Set DeclRef
  -> [LIGO ParsedInfo]
  -> ScopeM' ([ScopeTree], Set.Set DeclRef)
processSequence prevRefs [] = pure ([], prevRefs)
processSequence prevRefs (x:xs) = do
  let addToTopLevel :: ScopeTree -> Set.Set DeclRef -> ScopeTree
      addToTopLevel ((refs, range) :< rest) refs' =
        (Set.union refs refs', range) :< rest

  walk x >>= \case
    Nothing -> do
      (nextTrees, nextDecls) <- processSequence prevRefs xs
      pure (((prevRefs, getRange (extract x)) :< []) : nextTrees, nextDecls)
    Just (scopeTree, refs) -> do
      let newRefs = Set.union prevRefs (Set.fromList refs)
      (nextTrees, nextDecls) <- withScopes refs (processSequence newRefs xs)
      pure (addToTopLevel scopeTree prevRefs : nextTrees, nextDecls)

loop :: Functor f => (Cofree f a -> Cofree f a) -> Cofree f a -> Cofree f a
loop go = aux
  where
    aux (r :< fs) = go $ r :< fmap aux fs

loopM_ :: (Applicative t, Foldable f) => (Cofree f a -> t ()) -> (Cofree f a -> t ())
loopM_ go = aux
  where
    aux (r :< fs) = for_ fs aux *> go (r :< fs)

loopM
  :: (Monad m, Traversable f)
  => (Cofree f a -> m (Cofree f a)) -> (Cofree f a -> m (Cofree f a))
loopM go = aux
  where
    aux (r :< fs) = go . (r :<) =<< traverse aux fs

tellEndoList :: Monad m => a -> WriterT (Endo [a]) m ()
tellEndoList = tell . Endo . (<>) . pure

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
a <<&>> f = fmap (fmap f) a

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
  getName nameNode <<&>> \(PreprocessedRange origin, name) ->
    let
      _vdsInitRange = getRange <$> body
      _vdsParams = pure $ parseParameters paramNodes
      _vdsTspec = parseTypeDeclSpecifics <$> typ
    in
    ScopedDecl
      { _sdName = name
      , _sdOrigin = origin
      , _sdRefs = []
      , _sdDoc = docs
      , _sdDialect = dialect
      , _sdSpec = ValueSpec ValueDeclSpecifics{ .. }
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
  getName nameNode <<&>> \(PreprocessedRange origin, name) ->
    ScopedDecl
      { _sdName = name
      , _sdOrigin = origin
      , _sdRefs = []
      , _sdDoc = docs
      , _sdDialect = dialect
      , _sdSpec = ValueSpec ValueDeclSpecifics{ .. }
      }
  where
    _vdsInitRange = getRange <$> body
    _vdsParams = Nothing
    _vdsTspec = parseTypeDeclSpecifics <$> typ

typeScopedDecl
  :: ( PPableLIGO info
     , Contains PreprocessedRange info
     )
  => [Text]  -- ^ documentation comments
  -> LIGO info  -- ^ name node
  -> LIGO info  -- ^ type body node
  -> ScopeM (Either TreeDoesNotContainName ScopedDecl)
typeScopedDecl docs nameNode body = do
  dialect <- askDialect
  getTypeName nameNode <<&>> \(PreprocessedRange origin, name) ->
    ScopedDecl
      { _sdName = name
      , _sdOrigin = origin
      , _sdRefs = []
      , _sdDoc = docs
      , _sdDialect = dialect
      , _sdSpec = TypeSpec Nothing (parseTypeDeclSpecifics body)  -- The type variables are filled later
      }

mkTypeVariableScope :: Text -> Range -> ScopeM ScopedDecl
mkTypeVariableScope name range = do
  dialect <- askDialect
  let tspec = TypeDeclSpecifics range $ VariableType (TypeVariable name)
  pure $ ScopedDecl
    { _sdName = name
    , _sdOrigin = range
    , _sdRefs = []
    , _sdDoc = []
    , _sdDialect = dialect
    , _sdSpec = TypeSpec Nothing tspec
    }

mkDecl
  :: (Alternative f, Monad m)
  => m (Either e a)
  -> WriterT (Endo [e]) m (f a)
mkDecl = either ((empty <$) . tellEndoList) (pure . pure) <=< lift

select
  :: ( PPableLIGO info
     , Monad m
     , Contains PreprocessedRange info
     )
  => Text
  -> [Visit RawLigoList (Product info) (Writer [LIGO info])]
  -> LIGO info
  -> m (Either TreeDoesNotContainName (PreprocessedRange, Text))
select what handlers t
  = pure
  $ maybe
      (Left $ TreeDoesNotContainName (pp t) (extractRange $ getElem $ extract t) what)
      (Right . (getElem . extract &&& ppToText))
  $ listToMaybe
  $ execWriter
  $ visit' handlers
    t
  where
    extractRange (PreprocessedRange r) = r

getName
  :: ( PPableLIGO info
     , Monad m
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
  , Visit \r -> \case
      IsWildcard -> tell [fastMake r (Name "_")]
      _ -> tell []
  ]

getTypeName
  :: ( PPableLIGO info
     , Monad m
     , Contains PreprocessedRange info
     )
  => LIGO info
  -> m (Either TreeDoesNotContainName (PreprocessedRange, Text))
getTypeName = select "type name"
  [ Visit \r (TypeName t) ->
      tell [fastMake r (TypeName t)]
  ]
