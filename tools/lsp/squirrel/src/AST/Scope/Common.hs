
module AST.Scope.Common where

import           Control.Arrow (second)
import           Control.Monad.State
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure
import           Control.Monad.Writer (WriterT, execWriterT, tell)

import           Data.Monoid         (First (..))
import qualified Data.List   as List
import           Data.Map            (Map)
import qualified Data.Map    as Map
import           Data.Set            (Set)
import qualified Data.Set    as Set
import           Data.Maybe          (listToMaybe, maybeToList)
import           Data.Text           (Text)
import           Data.Foldable       (toList, for_)

import           Duplo.Lattice
import           Duplo.Pretty
import           Duplo.Tree hiding (loop)
import           Duplo.Error

import           AST.Skeleton
import           Cli.Types
import           Parser
import           Product
import           Range

import           Debug.Trace

class HasLigoClient m => HasScopeForest impl m where
  scopeForest :: FilePath -> LIGO Info -> m ScopeForest

instance {-# OVERLAPPABLE #-} Pretty x => Show x where
  show = show . pp

type FullEnv = Product ["vars" := Env, "types" := Env]
type Env     = Map Range [ScopedDecl]

data Category = Variable | Type
  deriving Eq

-- | The type/value declaration.
data ScopedDecl = ScopedDecl
  { _sdName    :: Text
  , _sdOrigin  :: Range
  , _sdBody    :: Maybe Range
  , _sdType    :: Maybe TypeOrKind
  , _sdRefs    :: [Range]
  , _sdDoc     :: [Text]
  }
  deriving Show via PP ScopedDecl

data TypeOrKind
  = IsType (LIGO '[])
  | IsKind Kind
  deriving Show via PP TypeOrKind

elimIsTypeOrKind :: (LIGO '[] -> c) -> (Kind -> c) -> TypeOrKind -> c
elimIsTypeOrKind l r = \case
  IsType t -> l t
  IsKind k -> r k

isType :: TypeOrKind -> Bool
isType = elimIsTypeOrKind (const True) (const False)

isKind :: TypeOrKind -> Bool
isKind = not . isType

instance Eq ScopedDecl where
  sd == sd1 = and
    [ pp (_sdName   sd) == pp (_sdName   sd1)
    ,     _sdOrigin sd  ==     _sdOrigin sd1
    ]

-- | The kind.
data Kind = Star
  deriving Show via PP Kind

instance {-# OVERLAPS #-} Pretty FullEnv where
  pp = block . map aux . Map.toList . mergeFE
    where
      aux (r, fe) =
        pp r `indent` block fe

      mergeFE fe = getTag @"vars" @Env fe Prelude.<> getTag @"types" fe

instance Pretty ScopedDecl where
  pp (ScopedDecl n o _ t refs doc) =
    sexpr "decl" [pp n, pp o, pp t, pp refs, pp doc]

instance Pretty TypeOrKind where
  pp (IsType ty) = pp $ fillInfo ty
  pp (IsKind k)  = pp k

instance Pretty Kind where
  pp _ = "TYPE"

instance Pretty Category where
  pp Variable = "Variable"
  pp Type     = "Type"

void' :: Functor f => f a -> f (Product '[])
void' = fmap $ const Nil

emptyEnv :: FullEnv
emptyEnv = Tag Map.empty :> Tag Map.empty :> Nil

with :: Category -> FullEnv -> (Env -> Env) -> FullEnv
with Variable env f = modTag @"vars"  f env
with Type     env f = modTag @"types" f env

ofCategory :: Category -> ScopedDecl -> Bool
ofCategory Variable ScopedDecl { _sdType = Just (IsKind Star) } = False
ofCategory Variable _                                           = True
ofCategory Type     ScopedDecl { _sdType = Just (IsKind Star) } = True
ofCategory _        _                                           = False

type Info' =
  [ [ScopedDecl]
  , Maybe Category
  , [Text]
  , Range
  , ShowRange
  , CodeSource
  ]

data ScopeForest = ScopeForest
  { sfScopes :: [ScopeTree]
  , sfDecls  :: Map DeclRef ScopedDecl
  }
  deriving Show via PP ScopeForest

type ScopeInfo = [Set DeclRef, Range]
type ScopeTree = Tree' '[[]] ScopeInfo

data DeclRef = DeclRef
  { drName  :: Text
  , drRange :: Range
  }
  deriving Show via PP DeclRef
  deriving stock (Eq, Ord)

instance Pretty DeclRef where
  pp (DeclRef n r) = pp n <.> "@" <.> pp r

withScopeForest
  :: (  ([ScopeTree], Map DeclRef ScopedDecl)
     -> ([ScopeTree], Map DeclRef ScopedDecl)
     )
  -> ScopeForest -> ScopeForest
withScopeForest f (ScopeForest ss ds) = uncurry ScopeForest (f (ss, ds))

instance Pretty ScopeForest where
  pp (ScopeForest sf ds) = go sf `above` decls' ds
    where
      go = sexpr "list" . map go'
      go' :: ScopeTree -> Doc
      go' (only -> (decls :> r :> Nil, list)) =
        sexpr "scope" ([pp r] ++ map pp (Set.toList decls) ++ [go list | not $ null list])

      decls' ds = sexpr "decls" (map pp $ Map.toList ds)

lookupEnv :: Text -> [ScopedDecl] -> Maybe ScopedDecl
lookupEnv name = getFirst . foldMap \decl ->
  First do
    guard (_sdName decl == name)
    return decl

envAtPoint :: Range -> ScopeForest -> [ScopedDecl]
envAtPoint r (ScopeForest sf ds) = do
  let sp = sf >>= spine r >>= Set.toList
  map (ds Map.!) sp

spine :: Range -> ScopeTree -> [Set DeclRef]
spine r (only -> (i, trees)) = if
  | leq r (getRange i) -> foldMap (spine r) trees <> [getElem @(Set DeclRef) i]
  | otherwise -> []

addLocalScopes :: forall impl m. HasScopeForest impl m => FilePath -> LIGO Info -> m (LIGO Info')
addLocalScopes fname tree = do
  forest <- scopeForest @impl fname tree

  let
    defaultHandler f (i :< fs) = do
      fs' <- traverse f fs
      let env = envAtPoint (getRange i) forest
      return ((env :> Nothing :> i) :< fs')

  descent @(Product Info) @(Product Info') @RawLigoList @RawLigoList defaultHandler
    [ Descent \(i, Name t) -> do
        let env = envAtPoint (getRange i) forest
        return (env :> Just Variable :> i, Name t)

    , Descent \(i, NameDecl t) -> do
        let env = envAtPoint (getRange i) forest
        return (env :> Just Variable :> i, NameDecl t)

    , Descent \(i, TypeName t) -> do
        let env = envAtPoint (getRange i) forest
        return (env :> Just Type :> i, Name t)
    ]
    tree
