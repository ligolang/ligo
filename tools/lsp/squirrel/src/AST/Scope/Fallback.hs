{-# LANGUAGE InstanceSigs #-}
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
import Data.Foldable (for_)
import Data.Kind qualified (Type)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashMap.Lazy (HashMap)
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Duplo.Pretty (Doc, pp, ppToText)
import Duplo.Tree hiding (loop)

import AST.Pretty (PPableLIGO)
import AST.Scope.Common
import AST.Scope.ScopedDecl
  ( DeclarationSpecifics (..), Scope, ScopedDecl (..), Type (VariableType)
  , TypeDeclSpecifics (..), TypeParams (..), TypeVariable (..), ValueDeclSpecifics (..)
  , sdName, sdOrigin, sdRefs
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

insertRef :: Text -> Range -> ScopeM' ()
insertRef name refRange = do
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
walk (r :< s) = walk' s r

class HasGo (f :: Data.Kind.Type -> Data.Kind.Type) where
  walk' :: f (LIGO ParsedInfo) -> Product ParsedInfo -> ScopeM' (Maybe (ScopeTree, [DeclRef]))

instance HasGo Name where
  walk' _ _ = pure Nothing

instance HasGo QualifiedName where
  walk' _ _ = pure Nothing

instance HasGo Pattern where
  walk' pattern' _ = case pattern' of
    IsConstr   {} -> pure Nothing
    IsConstant {} -> pure Nothing
    IsVar      {} -> pure Nothing
    IsCons     {} -> pure Nothing
    IsAnnot    {} -> pure Nothing
    IsWildcard {} -> pure Nothing
    IsSpread   {} -> pure Nothing
    IsList     {} -> pure Nothing
    IsTuple    {} -> pure Nothing
    IsRecord   {} -> pure Nothing
    IsParen    {} -> pure Nothing

instance HasGo RecordFieldPattern where
  walk' rfp _ = case rfp of
    IsRecordField   {} -> pure Nothing
    IsRecordCapture {} -> pure Nothing

instance HasGo Constant where
  walk' constant _ = case constant of
    CInt    {} -> pure Nothing
    CNat    {} -> pure Nothing
    CString {} -> pure Nothing
    CFloat  {} -> pure Nothing
    CBytes  {} -> pure Nothing
    CTez    {} -> pure Nothing

instance HasGo FieldAssignment where
  walk' fieldAssignment _ = case fieldAssignment of
    FieldAssignment {} -> pure Nothing
    Spread          {} -> pure Nothing
    Capture         {} -> pure Nothing

instance HasGo MapBinding where
  walk' _ _ = pure Nothing

instance HasGo Alt where
  walk' _ _ = pure Nothing

instance HasGo Expr where
  walk' expr _ = case expr of
    Let        {} -> pure Nothing
    Apply      {} -> pure Nothing
    Constant   {} -> pure Nothing
    Ident      {} -> pure Nothing
    BinOp      {} -> pure Nothing
    UnOp       {} -> pure Nothing
    Op         {} -> pure Nothing
    Record     {} -> pure Nothing
    If         {} -> pure Nothing
    Assign     {} -> pure Nothing
    AssignOp   {} -> pure Nothing
    List       {} -> pure Nothing
    ListAccess {} -> pure Nothing
    Set        {} -> pure Nothing
    Tuple      {} -> pure Nothing
    Annot      {} -> pure Nothing
    Attrs      {} -> pure Nothing
    BigMap     {} -> pure Nothing
    Map        {} -> pure Nothing
    Remove     {} -> pure Nothing
    Case       {} -> pure Nothing
    Skip       {} -> pure Nothing
    Break      {} -> pure Nothing
    Return     {} -> pure Nothing
    SwitchStm  {} -> pure Nothing
    ForLoop    {} -> pure Nothing
    WhileLoop  {} -> pure Nothing
    ForOfLoop  {} -> pure Nothing
    Seq        {} -> pure Nothing
    Block      {} -> pure Nothing
    Lambda     {} -> pure Nothing
    ForBox     {} -> pure Nothing
    Patch      {} -> pure Nothing
    RecordUpd  {} -> pure Nothing
    Michelson  {} -> pure Nothing
    Paren      {} -> pure Nothing

instance HasGo Collection where
  walk' collection _ = case collection of
    CList -> pure Nothing
    CMap  -> pure Nothing
    CSet  -> pure Nothing

instance HasGo TField where
  walk' _ _ = pure Nothing

instance HasGo Variant where
  walk' _ _ = pure Nothing

instance HasGo Skeleton.Type where
  walk' type' _ = case type' of
    TArrow    {} -> pure Nothing
    TRecord   {} -> pure Nothing
    TSum      {} -> pure Nothing
    TProduct  {} -> pure Nothing
    TApply    {} -> pure Nothing
    TString   {} -> pure Nothing
    TWildcard {} -> pure Nothing
    TVariable {} -> pure Nothing
    TParen    {} -> pure Nothing

instance HasGo Binding where
  walk' binding _ = case binding of
    BFunction     {} -> pure Nothing
    BParameter    {} -> pure Nothing
    BVar          {} -> pure Nothing
    BConst        {} -> pure Nothing
    BTypeDecl     {} -> pure Nothing
    BAttribute    {} -> pure Nothing
    BInclude      {} -> pure Nothing
    BImport       {} -> pure Nothing
    BModuleDecl   {} -> pure Nothing
    BModuleAlias  {} -> pure Nothing

instance HasGo RawContract where
  walk' _ _ = pure Nothing

instance HasGo TypeName where
  walk' _ _ = pure Nothing

instance HasGo TypeVariableName where
  walk' _ _ = pure Nothing

instance HasGo FieldName where
  walk' _ _ = pure Nothing

instance HasGo Verbatim where
  walk' _ _ = pure Nothing

instance HasGo Error where
  walk' _ _ = pure Nothing

instance HasGo Ctor where
  walk' _ _ = pure Nothing

instance HasGo NameDecl where
  walk' _ _ = pure Nothing

instance HasGo Preprocessor where
  walk' _ _ = pure Nothing

instance HasGo PreprocessorCommand where
  walk' _ _ = pure Nothing

instance HasGo PatchableExpr where
  walk' _ _ = pure Nothing

instance HasGo ModuleName where
  walk' _ _ = pure Nothing

instance HasGo ModuleAccess where
  walk' _ _ = pure Nothing

instance HasGo Skeleton.TypeParams where
  walk' typeParams _ = case typeParams of
    Skeleton.TypeParam  {} -> pure Nothing
    Skeleton.TypeParams {} -> pure Nothing

instance HasGo CaseOrDefaultStm where
  walk' cods _ = case cods of
    CaseStm    {} -> pure Nothing
    DefaultStm {} -> pure Nothing

instance Apply HasGo RawLigoList => HasGo (Sum RawLigoList) where
  walk' = apply @HasGo walk'

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

typeVariableScopedDecl :: TypeParams -> ScopeM Scope
typeVariableScopedDecl tyVars = do
  dialect <- askDialect
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
