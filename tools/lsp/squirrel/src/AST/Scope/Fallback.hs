module AST.Scope.Fallback
  ( Fallback
  , loop
  , loopM
  , loopM_
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Monad.Writer (Endo (..), Writer, WriterT, execWriter, runWriterT, tell)

import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Duplo.Pretty (Doc, pp, ppToText)
import Duplo.Tree hiding (loop)

import AST.Pretty (PPableLIGO)
import AST.Scope.Common
import AST.Scope.ScopedDecl
  ( DeclarationSpecifics (..), Scope, ScopedDecl (..), Type (VariableType)
  , TypeDeclSpecifics (..), TypeParams (..), TypeVariable (..), ValueDeclSpecifics (..)
  , fillTypeIntoCon, fillTypeParams
  )
import AST.Scope.ScopedDecl.Parser (parseParameters, parseTypeDeclSpecifics)
import AST.Skeleton hiding (Type, TypeParams (..))
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
    (sf, (`appEndo` []) -> errs) <- liftIO $ flip runReaderT dialect $ runWriterT $ getEnv ligo
    let msgs' = map toMsg errs
    pure $ FindContract src sf (msgs <> msgs')

type ScopeM = ReaderT Lang IO

getEnv :: LIGO ParsedInfo -> WriterT (Endo [TreeDoesNotContainName]) ScopeM ScopeForest
getEnv _ = pure (ScopeForest [] Map.empty)

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
