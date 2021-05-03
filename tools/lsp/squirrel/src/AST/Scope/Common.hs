{-# OPTIONS_GHC -Wno-orphans #-}

module AST.Scope.Common where

import Control.Arrow ((&&&))
import Control.Monad.State
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (First (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree hiding (loop)

import AST.Pretty
import AST.Scope.ScopedDecl (DeclarationSpecifics (..), ScopedDecl (..))
import AST.Skeleton
  (Ctor (..), Lang, Name (..), NameDecl (..), RawLigoList, SomeLIGO, Tree', TypeName (..),
  withNestedLIGO)
import Cli.Types
import Control.Exception.Safe
import Control.Lens.Operators ((&))
import Control.Monad.Reader
import Control.Monad.Trans.Except
import ParseTree
import Parser
import Product
import Range
import Util (nubOrd, zipWithRepeat)

class HasLigoClient m => HasScopeForest impl m where
  scopeForest :: Source -> SomeLIGO Info -> [Msg] -> m (ScopeForest, [Msg])

instance {-# OVERLAPPABLE #-} Pretty x => Show x where
  show = show . pp

type FullEnv = Product ["vars" := Env, "types" := Env]
type Env     = Map Range [ScopedDecl]

data Level = TermLevel | TypeLevel
  deriving stock Eq

instance {-# OVERLAPS #-} Pretty FullEnv where
  pp = block . map aux . Map.toList . mergeFE
    where
      aux (r, fe) =
        pp r `indent` block fe

      mergeFE fe = getTag @"vars" @Env fe Prelude.<> getTag @"types" fe

instance Pretty Level where
  pp TermLevel = "TermLevel"
  pp TypeLevel = "TypeLevel"

void' :: Functor f => f a -> f (Product '[])
void' = fmap $ const Nil

emptyEnv :: FullEnv
emptyEnv = Tag Map.empty :> Tag Map.empty :> Nil

with :: Level -> FullEnv -> (Env -> Env) -> FullEnv
with TermLevel env f = modTag @"vars"  f env
with TypeLevel env f = modTag @"types" f env

ofLevel :: Level -> ScopedDecl -> Bool
ofLevel level decl = case (level, _sdSpec decl) of
  (TermLevel, ValueSpec{}) -> True
  (TypeLevel, TypeSpec{}) -> True
  _ -> False

data ScopeError =
  TreeDoesNotContainName
    Doc  -- ^ pprinted tree (used for simplifying purposes for not stacking
         -- type parameters for `ScopeM` which brings plethora of confusion)
    Range -- ^ location where the error has occurred
    Text -- ^ variable name
  deriving Show via PP ScopeError

instance Pretty ScopeError where
  pp = \case
    (TreeDoesNotContainName tree _ name) -> "Given tree: " <> tree <> " does not contain name: " <> pp name

instance Exception ScopeError

type ScopeM = ExceptT ScopeError (Reader Lang)

type Info' =
  [ [ScopedDecl]
  , Maybe Level
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

-- | Merge two scope forests into one.
mergeScopeForest :: ScopeForest -> ScopeForest -> ScopeForest
mergeScopeForest (ScopeForest sl dl) (ScopeForest sr dr) =
  ScopeForest (descend sl sr) (Map.unionWith mergeRefs dl dr)
  where
    go :: ScopeTree -> ScopeTree -> [ScopeTree]
    go
      l@(only -> (ldecls :> lr :> Nil, ldeepen))
      r@(only -> (rdecls :> rr :> Nil, rdeepen))
      -- These two are likely different things, so we shouldn't merge them.
      | not (lr `intersects` rr) = [l, r]
      -- Merge the scopes if they have different decls within the same range.
      | lr == rr  = [make (mergeDecls ldecls rdecls :> rr :> Nil, descend ldeepen rdeepen)]
      -- The left scope is more local than the right hence try to find where the
      -- right subscope is more local or equal to the left one.
      | leq lr rr = [make (mergeDecls ldecls rdecls :> rr :> Nil, descend [l] rdeepen)]
      -- The right scope is more local than the left hence try to find where the
      -- left subscope is more local or equal to the right one.
      | otherwise = [make (mergeDecls ldecls rdecls :> lr :> Nil, descend ldeepen [r])]
    go _ _ = error "ScopeForest.mergeScopeForest: Impossible"

    descend :: [ScopeTree] -> [ScopeTree] -> [ScopeTree]
    descend xs ys = concat $ zipWithRepeat go xs ys

    -- Merges the references of two 'ScopedDecl's in a right-biased fashion.
    -- In the current implementation, the compiler's scopes will be on the left
    -- and the fallback ones will be on the right.
    mergeRefs :: ScopedDecl -> ScopedDecl -> ScopedDecl
    mergeRefs l r = r
      { _sdRefs = nubOrd (_sdRefs l <> _sdRefs r)
      , _sdDoc  = nubOrd (_sdDoc  l <> _sdDoc  r)
      }

    -- Merge two sets of DeclRefs preferring decls that have a smaller range
    -- (i.e., is more local than the other).
    mergeDecls :: Set DeclRef -> Set DeclRef -> Set DeclRef
    mergeDecls l r
      = Map.unionWith
        (\(DeclRef n lr) (DeclRef _ rr) -> DeclRef n (if leq lr rr then lr else rr))
        (mapFromFoldable drName l)
        (mapFromFoldable drName r)
      & Map.elems
      & Set.fromList

    mapFromFoldable :: (Foldable f, Ord k) => (a -> k) -> f a -> Map k a
    mapFromFoldable f = Map.fromList . map (f &&& id) . toList

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
      go' (only -> (decls :> r :> Nil, list')) =
        sexpr "scope" ([pp r] ++ map pp (Set.toList decls) ++ [go list' | not $ null list'])
      go' _ = error "ScopeForest.pp: Impossible"

      decls' = sexpr "decls" . map pp . Map.toList

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
spine r (only -> (i, trees))
  | leq r (getRange i) = foldMap (spine r) trees <> [getElem @(Set DeclRef) i]
  | otherwise = []

addLocalScopes
  :: forall impl m .
    ( HasScopeForest impl m
    )
  => Source
  -> SomeLIGO Info
  -> [Msg]
  -> m (SomeLIGO Info', [Msg])
addLocalScopes src tree msg = do
  (forest, msg') <- scopeForest @impl src tree msg
  let
    defaultHandler f (i :< fs) = do
      fs' <- traverse f fs
      let env = envAtPoint (getRange i) forest
      return ((env :> Nothing :> i) :< fs')

  scopeTree <- withNestedLIGO tree $
    descent @(Product Info) @(Product Info') @RawLigoList @RawLigoList defaultHandler
    [ Descent \(i, Name t) -> do
        let env = envAtPoint (getRange i) forest
        return (env :> Just TermLevel :> i, Name t)

    , Descent \(i, NameDecl t) -> do
        let env = envAtPoint (getRange i) forest
        return (env :> Just TermLevel :> i, NameDecl t)

    , Descent \(i, Ctor t) -> do
        let env = envAtPoint (getRange i) forest
        return (env :> Just TermLevel :> i, Ctor t)

    , Descent \(i, TypeName t) -> do
        let env = envAtPoint (getRange i) forest
        return (env :> Just TypeLevel :> i, TypeName t)
    ]

  return (scopeTree, msg')
