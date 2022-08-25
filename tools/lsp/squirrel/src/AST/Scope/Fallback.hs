module AST.Scope.Fallback
  ( Fallback
  , loop
  , loopM
  , loopM_
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Control.Lens ((%~), (&), _Just, _head)
import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Monad.Writer (Endo (..), Writer, WriterT, execWriter, runWriter, runWriterT, tell)

import Data.Foldable (foldrM, for_, toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Duplo.Pretty (Doc, pp, ppToText)
import Duplo.Tree hiding (loop)
import Witherable (forMaybe)

import AST.Pretty (PPableLIGO)
import AST.Scope.Common
import AST.Scope.ScopedDecl
  ( DeclarationSpecifics (..), Scope, ScopedDecl (..), Type (VariableType)
  , TypeDeclSpecifics (..), TypeParams (..), TypeVariable (..), ValueDeclSpecifics (..)
  , fillTypeIntoCon, fillTypeParams
  )
import AST.Scope.ScopedDecl.Parser (parseParameters, parseTypeDeclSpecifics, parseTypeParams)
import AST.Skeleton hiding (Type, TypeParams (..))
import Cli.Types
import Diagnostic (Message (..), MessageDetail (FromLanguageServer), Severity (..))
import Log (i)
import Parser
import Product
import Range
import Util (foldMapM, unconsFromEnd)

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
    (sf, (`appEndo` []) -> errs) <- liftIO $ flip runReaderT dialect $ runWriterT $ getEnv ligo
    let msgs' = map toMsg errs
    pure $ FindContract src sf (msgs <> msgs')

addReferences :: LIGO ParsedInfo -> ScopeForest -> ScopeForest
addReferences ligo = execState $ loopM_ addRef ligo
  where
    addRef :: LIGO ParsedInfo -> State ScopeForest ()
    addRef = \case
      (match -> Just (r, Name             n)) -> addThisRef TermLevel (getElem r) n
      (match -> Just (r, NameDecl         n)) -> addThisRef TermLevel (getElem r) n
      (match -> Just (r, Ctor             n)) -> addThisRef TermLevel (getElem r) n
      (match -> Just (r, TypeName         n)) -> addThisRef TypeLevel (getElem r) n
      (match -> Just (r, TypeVariableName n)) -> addThisRef TypeLevel (getElem r) n
      _                                       -> pure ()

    addThisRef :: Level -> PreprocessedRange -> Text -> State ScopeForest ()
    addThisRef cat' (PreprocessedRange r) n = do
      modify
        $ withScopeForest \(sf, ds) ->
          flip runState ds do
            let frameSet = Set.toList =<< toList . spine r =<< sf
            walkScope cat' r n frameSet
            return sf

    walkScope :: Level -> Range -> Text -> [DeclRef] -> State (Map DeclRef ScopedDecl) ()
    walkScope _     _ _ [] = return ()
    walkScope level r n (declref : rest) = do
      decl <- gets (Map.! declref)
      if ofLevel level decl && (n == _sdName decl || r == _sdOrigin decl)
      then do
        modify $ Map.adjust (addRefToDecl r) declref
      else do
        walkScope level r n rest

    addRefToDecl :: Range -> ScopedDecl -> ScopedDecl
    addRefToDecl r sd = sd { _sdRefs = r : _sdRefs sd }

type ScopeM = ReaderT Lang IO

getEnv :: LIGO ParsedInfo -> WriterT (Endo [TreeDoesNotContainName]) ScopeM ScopeForest
getEnv tree
  = addReferences tree
  . extractScopeForest
  . compressScopeTree
  . extractScopeTree
  <$> prepareTree tree

prepareTree
  :: LIGO ParsedInfo
  -> WriterT (Endo [TreeDoesNotContainName]) ScopeM (LIGO (Scope ': Bool ': Range ': ParsedInfo))
prepareTree
  = assignDecls
  <=< pure . wildcardToName
  <=< lift . unSeq
  <=< lift . unLetRec

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

-- | Replace a wildcard with a dummy "_" name so that the LSP doesn't trip up
-- with 'TreeDoesNotContainName'. This is also necessary so that we are able to
-- use capabilities inside wildcards declarations, such as the definition of `x`
-- in `let _ = x * x`.
wildcardToName :: LIGO xs -> LIGO xs
wildcardToName = loop go
  where
    go (match -> Just (r, IsWildcard)) = fastMake r (NameDecl "_")
    go it = it

unLetRec
  :: ( Contains Range xs
     , Eq (Product xs)
     )
  => LIGO xs -> ScopeM (LIGO xs)
unLetRec = loopM go
  where
    go = \case
      (match -> Just (r, expr)) -> do
        case expr of
          Let (match -> Just (_, Seq decls)) body -> do
            foldrM joinWithLet body decls
          _ -> pure $ fastMake r expr

      -- TODO: somehow append Unit to the end
      (match -> Just (r, RawContract decls)) ->
        case unconsFromEnd decls of
          Nothing -> pure $ fastMake r (RawContract [])
          Just (initDecls, lastDecl) -> foldrM joinWithLet lastDecl initDecls

      it -> pure it

-- | Turn all 'Seq'uences of nodes into a tree of 'Let's, so that each
-- subsequent node turned out to be in the scope of the previous one.
unSeq
  :: ( Contains Range xs
     , Eq (Product xs)
     )
  => LIGO xs -> ScopeM (LIGO xs)
unSeq = loopM go
  where
    go = \case
      (match -> Just (r, Seq decls)) -> do
        case unconsFromEnd decls of
          Nothing -> pure $ fastMake r (Seq [])
          Just (initDecls, lastDecl) -> foldrM joinWithLet lastDecl initDecls

      it -> pure it

-- | Combine two tree nodes with 'Let', so that the second node turns out to be
-- in the scope of the first node.
joinWithLet
  :: ( Contains Range xs
     , Eq (Product xs)
     , Apply Functor fs
     , Element Expr fs
     )
  => Tree' fs xs -> Tree' fs xs -> ScopeM (Tree' fs xs)
joinWithLet decl body = makeIO r' (Let decl body)
  where
    r' = putElem (getRange decl `merged` getRange body)
        $ extract body

tellEndoList :: Monad m => a -> WriterT (Endo [a]) m ()
tellEndoList = tell . Endo . (<>) . pure

assignDecls
  :: ( Contains  Range     xs
     , Contains [Text]     xs
     , Contains  PreprocessedRange xs
     , Eq (Product xs)
     )
  => LIGO xs
  -> WriterT (Endo [TreeDoesNotContainName]) ScopeM (LIGO (Scope : Bool : Range : xs))
assignDecls = loopM go . fmap (\r -> [] :> False :> getRange r :> r)
  where
    go = \case
      -- TODO (LIGO-318): We get declarations twice because of this branch.
      -- For example, suppose we have some BTypeDecl followed by a BConst. We'd
      -- get the decls for BTypeDecl and put them in BConst's scope. But then,
      -- because of the loopM above, we'd eventually get to the BTypeDecl again
      -- and once more extract its declarations.
      -- Unfortunately, it can't just be trivially removed, because it would
      -- break things, and tests would start failing.
      -- This also means that branches that do non-trivial operations on the
      -- decl node may be incorrect (i.e., do not simply use markAsScope), since
      -- their branches are not properly visited. Again using the example above,
      -- we want to fill BTypeDecl's decl with the parsed type variables, but
      -- it's too bad they won't appear here since we do it below. I'm not sure
      -- what kind of problems may arise.
      (match -> Just (r, Let decl body)) -> do
        imm <- getImmediateDecls decl
        let r' :< body' = body
        let l' :< decl' = decl
        let r'' = putElem (getRange body) $ putElem True $ modElem (imm <>) r'
        let l'' = putElem (getRange decl) $ putElem True l'
        makeIO r (Let (l'' :< decl') (r'' :< body'))

      (match -> Just (r, Lambda args ty body)) -> do
        imms <- foldMapM getImmediateDecls args
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        makeIO r (Lambda args ty (r'' :< body'))

      (match -> Just (r, Alt pat body)) -> do
        imms <- getImmediateDecls pat
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        makeIO r (Alt pat (r'' :< body'))

      (match -> Just (r, BFunction True n params ty b)) -> do
        imms <- foldMapM getImmediateDecls params
        fDecl <- lift $ functionScopedDecl (getElem r) n params ty (Just b)
        imms' <- either ((imms <$) . tellEndoList) (pure . (: imms)) fDecl
        let r' = putElem True $ modElem (imms' <>) r
        makeIO r' (BFunction True n params ty b)

      (match -> Just (r, BFunction False n params ty b)) -> do
        imms <- foldMapM getImmediateDecls params
        fDecl <- lift $ functionScopedDecl (getElem r) n params ty (Just b)
        r' <- either
          ((putElem True r <$) . tellEndoList)
          (\decl -> pure $ putElem True (modElem (decl :) r))
          fDecl
        let b' = b & _extract %~ (putElem True . modElem (imms <>))
        makeIO r' (BFunction False n params ty b')

      node@(match -> Just (r, BTypeDecl t tyVars b)) -> do
        imms <- getImmediateDecls node
        let parsedVars = parseTypeParams =<< tyVars
        let imms' = maybe imms (\vars -> imms & _head %~ fillTypeParams vars) parsedVars
        varDecls <- lift $ fromMaybe [] <$> traverse typeVariableScopedDecl parsedVars

        let r' = putElem True $ modElem (imms' <>) r
        let tyVars' = tyVars & _Just . _extract %~ (putElem True . modElem (varDecls <>))
        let b' = b & _extract %~ (putElem True . modElem (varDecls <>))
        makeIO r' (BTypeDecl t tyVars' b')

      (match -> Just (r, node@BVar{})) -> markAsScope r node
      (match -> Just (r, node@BConst{})) -> markAsScope r node
      (match -> Just (r, node@BParameter{})) -> markAsScope r node
      (match -> Just (r, node@IsVar{})) -> markAsScope r node

      it -> pure it

    markAsScope range node = do
      imms <- getImmediateDecls =<< makeIO range node
      let range' = putElem True (modElem (imms <>) range)
      makeIO range' node

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
  dialect <- ask
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
  dialect <- ask
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
  dialect <- ask
  getTypeName nameNode <<&>> \(PreprocessedRange origin, name) ->
    ScopedDecl
      { _sdName = name
      , _sdOrigin = origin
      , _sdRefs = []
      , _sdDoc = docs
      , _sdDialect = dialect
      , _sdSpec = TypeSpec Nothing (parseTypeDeclSpecifics body)  -- The type variables are filled later
      }

typeVariableScopedDecl :: TypeParams -> ScopeM Scope
typeVariableScopedDecl tyVars = do
  dialect <- ask
  pure case tyVars of
    TypeParam var -> [mkTyVarScope dialect var]
    TypeParams vars -> map (mkTyVarScope dialect) vars
  where
    mkTyVarScope dialect (TypeDeclSpecifics r tv@(TypeVariable name)) =
      let tspec = TypeDeclSpecifics r $ VariableType tv in
      ScopedDecl
        { _sdName = name
        , _sdOrigin = r
        , _sdRefs = []
        , _sdDoc = []
        , _sdDialect = dialect
        , _sdSpec = TypeSpec Nothing tspec
        }

extractScopeTree
  :: LIGO (Scope : Bool : Range : xs)
  -> Cofree [] (Scope, Bool, Range)
extractScopeTree ((decls :> visible :> r :> _) :< fs) =
  (decls, visible, r) :< map extractScopeTree (toList fs)

-- 'Bool' in the node list denotes whether this part of a tree is a scope
compressScopeTree
  :: Cofree [] (Scope, Bool, Range)
  -> [Cofree [] (Scope, Range)]
compressScopeTree = go
  where
    go :: Cofree [] (Scope, Bool, Range) -> [Cofree [] (Scope, Range)]
    go ((_, False, _) :< rest) =
      rest >>= go

    go ((decls, True, r) :< rest) =
      let rest' = rest >>= go
      in [ (decls, r) :< rest'
         | not (null decls) || not (null rest')
         ]

extractScopeForest :: [Cofree [] (Scope, Range)] -> ScopeForest
extractScopeForest = uncurry ScopeForest . runWriter . mapM go
  where
    go
      :: Cofree [] (Scope, Range)
      -> Writer (Map DeclRef ScopedDecl) ScopeTree
    go ((decls, r) :< ts) = do
      let mkDeclRef sd = DeclRef (_sdName sd) (_sdOrigin sd)
      let extracted = Map.fromList $ map (mkDeclRef &&& id) decls
      tell extracted
      let refs = Map.keysSet extracted
      ts' <- mapM go ts
      pure $ (refs, r) :< ts'

mkDecl
  :: (Alternative f, Monad m)
  => m (Either e a)
  -> WriterT (Endo [e]) m (f a)
mkDecl = either ((empty <$) . tellEndoList) (pure . pure) <=< lift

getImmediateDecls
  :: ( PPableLIGO info
     , Contains PreprocessedRange info
     , Eq (Product info)
     )
  => LIGO info -> WriterT (Endo [TreeDoesNotContainName]) ScopeM Scope
getImmediateDecls = \case
  (match -> Just (r, pat)) -> do
    case pat of
      IsVar v ->
        mkDecl $ valueScopedDecl (getElem r) v Nothing Nothing

      IsTuple    xs   -> foldMapM getImmediateDecls xs
      IsRecord   xs   -> foldMapM getImmediateDecls xs
      IsList     xs   -> foldMapM getImmediateDecls xs
      IsSpread   s    -> getImmediateDecls s
      IsWildcard      -> pure []
      IsAnnot    x t  -> (<>) <$> getImmediateDecls x <*> getImmediateDecls t
      IsCons     h t  -> (<>) <$> getImmediateDecls h <*> getImmediateDecls t
      IsConstant _    -> pure []
      IsConstr   _ xs -> foldMapM getImmediateDecls xs
      IsParen    x    -> getImmediateDecls x

  (match -> Just (r, pat)) -> do
    case pat of
      IsRecordField label body ->
        mkDecl $ valueScopedDecl (getElem r) label Nothing (Just body)
      IsRecordCapture label ->
        mkDecl $ valueScopedDecl (getElem r) label Nothing Nothing

  (match -> Just (r, pat)) -> do
    case pat of
      BFunction _ f params t b ->
        mkDecl $ functionScopedDecl (getElem r) f params t (Just b)

      BVar v t b -> mkDecl $ valueScopedDecl (getElem r) v t b

      BConst name typ (Just (layer -> Just (Lambda params _ body))) ->
        mkDecl $ functionScopedDecl (getElem r) name params typ (Just body)

      BConst (layer -> Just (IsParen (layer -> Just (IsTuple names)))) typ (Just (layer -> Just (Tuple vals))) ->
        forMaybe (zip names vals) \(name, val) ->
          mkDecl $ valueScopedDecl (getElem r) name typ (Just val)

      BConst (layer -> Just (IsTuple names)) typ (Just (layer -> Just (Tuple vals))) ->
        forMaybe (zip names vals) \(name, val) ->
          mkDecl $ valueScopedDecl (getElem r) name typ (Just val)

      BConst c t b -> mkDecl $ valueScopedDecl (getElem r) c t b

      BParameter n t ->
        mkDecl $ valueScopedDecl (getElem r) n t Nothing

      BTypeDecl t _ b -> do
        typeDeclMaybe <- mkDecl $ typeScopedDecl (getElem r) t b
        -- Gather all other declarations from the depths of ast, such as type
        -- sum constructors, nested types etc. Then, fill in missing types of
        -- values. It doesn't seem possible that these values will be anything
        -- but directly related to the type constructors. There are two reasons
        -- for that. One is that deeper nested constructors will have their
        -- types already filled by types corresponding to them. Two is that due
        -- to grammar limitations there will be no other values besides
        -- constructors.
        imms <- getImmediateDecls b
        pure case typeDeclMaybe of
          Nothing -> imms
          Just typeDecl -> typeDecl : map (fillTypeIntoCon typeDecl) imms

      BAttribute _ -> pure []
      BInclude _ -> pure []
      BImport _ _ -> pure []
      BModuleDecl _ _ -> pure []
      BModuleAlias _ _ -> pure []

  (match -> Just (_, pat)) -> case pat of
    TRecord typeFields -> foldMapM getImmediateDecls typeFields
    TProduct typs -> foldMapM getImmediateDecls typs
    TSum variants -> foldMapM getImmediateDecls variants
    -- TODO: Currently, we don't handle type variables in type signatures for
    -- terms. LIGO doesn't seem to yet compile contracts with them, and I'm not
    -- sure what are the scoping rules for them (e.g.: whether they are true
    -- type variables or existentials).
    _ -> pure []
    -- there are most probably others, add them as problems arise

  (match -> Just (r, Variant name paramTyp)) -> do
    -- type is Nothing at this stage, but it will be substituted with the
    -- (hopefully) correct type higher in the tree (see 'BTypeDecl' branch).
    constructorDeclMaybe <- mkDecl $ functionScopedDecl (getElem r) name [] Nothing Nothing
    nestedDecls <- maybe (pure []) getImmediateDecls paramTyp
    pure $ maybe nestedDecls (: nestedDecls) constructorDeclMaybe
  _ -> pure []

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
