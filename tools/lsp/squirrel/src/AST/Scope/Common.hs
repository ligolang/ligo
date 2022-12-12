-- Deriving `ToGraph` creates some reduntant constraints warnings which we
-- unforunately have no control over. Disable this warning for now.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module AST.Scope.Common
  ( ParsedContract (..)
  , FindFilepath (..)
  , HasScopeForest (..)
  , Namespace (..)
  , Level (..)
  , Info'
  , ScopeForest (..)
  , ScopeInfo
  , ScopeTree
  , DeclRef (..)
  , MergeStrategy (..)
  , ContractInfo
  , ParsedContractInfo
  , ContractInfo'
  , ContractNotFoundException (..)
  , contractNotFoundException
  , Includes (..)

  , pattern FindContract

  , contractFile
  , contractTree
  , contractMsgs

  , addLigoErrsToMsg

  , cFile
  , cTree
  , cMsgs
  , getContract

  , emptyScopeForest
  , ofLevel
  , mergeScopeForest
  , withScopeForest
  , lookupEnv
  , addScopes
  , lookupContract
  ) where

import Prelude hiding (Element, Product)

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G (edges, gmap, overlay, vertices)
import Algebra.Graph.Class (Graph)
import Algebra.Graph.Export qualified as G (export, literal, render)
import Algebra.Graph.ToGraph (ToGraph)
import Algebra.Graph.ToGraph qualified as G
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.DList (DList, snoc)
import Data.DList qualified as DList (toList)
import Data.Foldable qualified as Foldable (toList)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Katip (LogItem (..), PayloadSelection (..), ToObject, Verbosity (..))
import UnliftIO.Exception (throwIO)
import UnliftIO.MVar (modifyMVar)

import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree hiding (loop)

import AST.Pretty
import AST.Scope.ScopedDecl
  (DeclarationSpecifics (..), Namespace (..), Scope, ScopedDecl (..), ValueDeclSpecifics (..))
import AST.Skeleton
  (Ctor, LIGO, ModuleName, Name, NameDecl, RawLigoList, SomeLIGO, TypeName, TypeVariableName,
  withNestedLIGO)
import Cli.Types
import Diagnostic (Message)
import Log qualified
import Parser (Info, ParsedInfo)
import ParseTree
import Product
import Progress (Progress (..), ProgressCallback, (%))
import Range
import Util (findKey, unionOrd)
import Util.Graph (forAMConcurrently, traverseAMConcurrently)

-- TODO: Many of these datatypes don't make sense to be defined here. Consider
-- moving into different or new modules.
data ParsedContract info = ParsedContract
  { _cFile :: Source -- ^ The path to the contract.
  , _cTree :: info -- ^ The payload of the contract.
  , _cMsgs :: [Message] -- ^ Messages produced by this contract.
  } deriving stock (Show)
    deriving Pretty via ShowPP (ParsedContract info)

-- | Wraps a 'ParsedContract', allowing it to be stored in a container where its
-- comparison will always be on its source file.
newtype FindFilepath info
  = FindFilepath { _getContract :: ParsedContract info }
  deriving stock (Show)
  deriving newtype (Pretty)

instance Eq (FindFilepath info) where
  (==) = (==) `on` contractFile

instance Ord (FindFilepath info) where
  compare = compare `on` contractFile

contractFile :: FindFilepath info -> FilePath
contractFile (FindFilepath pc) = srcPath $ _cFile pc

contractTree :: FindFilepath info -> info
contractTree (FindFilepath pc) = _cTree pc

contractMsgs :: FindFilepath info -> [Message]
contractMsgs (FindFilepath pc) = _cMsgs pc

makeLenses ''ParsedContract
makeLenses ''FindFilepath

addLigoErrsToMsg :: [Message] -> FindFilepath info -> FindFilepath info
addLigoErrsToMsg errs = getContract . cMsgs %~ (errs <>)

class HasLigoClient m => HasScopeForest impl m where
  scopeForest
    :: TempSettings
    -- ^ Settings for the temporary directory to call LIGO.
    -> ProgressCallback m
    -- ^ A callback allowing reporting of the progress to the user.
    -> Includes ParsedContractInfo
    -- ^ Inclusion graph of the parsed contracts.
    -> m (Includes (FindFilepath ScopeForest))
  scopeForest tempSettings reportProgress (Includes graph) = Includes <$> do
    let nContracts = G.vertexCount graph
    -- We use a MVar here since there is no instance of 'MonadUnliftIO' for
    -- 'StateT'. It's best to avoid using this class for stateful monads.
    counter <- newMVar 0
    forAMConcurrently graph \contract -> do
      n <- modifyMVar counter (pure . (succ &&& id))
      reportProgress $
        Progress (n % nContracts) [Log.i|Adding scopes for #{contractFile contract}|]
      scopeContract @impl tempSettings contract

  scopeContract
    :: TempSettings
    -- ^ Settings for the temporary directory to call LIGO.
    -> ParsedContractInfo
    -- ^ Inclusion graph of the parsed contracts.
    -> m (FindFilepath ScopeForest)

data Level = TermLevel | TypeLevel | ModuleLevel
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

instance Pretty Level where
  pp TermLevel   = "term"
  pp TypeLevel   = "type"
  pp ModuleLevel = "module"

ofLevel :: Level -> ScopedDecl -> Bool
ofLevel level decl = case (level, _sdSpec decl) of
  (TermLevel, ValueSpec{}) -> True
  (TypeLevel, TypeSpec{}) -> True
  (ModuleLevel, ModuleSpec{}) -> True
  _ -> False

type Info' = Scope ': Maybe Level ': ParsedInfo

data ScopeForest = ScopeForest
  { sfScopes :: [ScopeTree]
  , sfDecls  :: Map DeclRef ScopedDecl
  }
  deriving stock (Eq, Ord)
  deriving Show via PP ScopeForest

emptyScopeForest :: ScopeForest
emptyScopeForest = ScopeForest [] Map.empty

type ScopeInfo = (Set DeclRef, Range)
type ScopeTree = Cofree [] ScopeInfo

data DeclRef = DeclRef
  { drName  :: Text
  , drRange :: Range
  }
  deriving Show via PP DeclRef
  deriving stock (Eq, Ord)

instance Pretty DeclRef where
  pp (DeclRef n r) = pp n <.> "<-" <.> pp r

data MergeStrategy
  = OnUnion
  | OnIntersection

-- | Merge two scope forests into one.
mergeScopeForest :: MergeStrategy -> ScopeForest -> ScopeForest -> ScopeForest
mergeScopeForest strategy (ScopeForest sl dl) (ScopeForest sr dr) =
  ScopeForest (descend sl sr) (mapStrategy mergeRefs dl dr)
  where
    mapStrategy :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
    mapStrategy = case strategy of
      OnUnion -> Map.unionWith
      OnIntersection -> Map.intersectionWith

    go :: ScopeTree -> ScopeTree -> [ScopeTree]
    go
      l@((ldecls, lr) :< ldeepen)
      r@((rdecls, rr) :< rdeepen)
      -- These two are likely different things, so we shouldn't merge them.
      | not (lr `intersects` rr) = [l, r]
      -- Merge the scopes if they have different decls within the same range.
      | lr == rr  = [(mergeDecls ldecls rdecls, rr) :< descend ldeepen rdeepen]
      -- The left scope is more local than the right hence try to find where the
      -- right subscope is more local or equal to the left one.
      | leq lr rr = [(mergeDecls ldecls rdecls, rr) :< descend [l] rdeepen]
      -- The right scope is more local than the left hence try to find where the
      -- left subscope is more local or equal to the right one.
      | otherwise = [(mergeDecls ldecls rdecls, lr) :< descend ldeepen [r]]

    zipWithMissing, zipWithMatched, zipWithStrategy :: Ord c => (a -> c) -> (a -> a -> b) -> (a -> b) -> [a] -> [a] -> [b]
    zipWithMissing _ _ g [] ys = g <$> ys
    zipWithMissing _ _ g xs [] = g <$> xs
    zipWithMissing p f g xs'@(x : xs) ys'@(y : ys) = case compare (p x) (p y) of
      LT -> g x   : zipWithMissing p f g xs  ys'
      EQ -> f x y : zipWithMissing p f g xs  ys
      GT -> g   y : zipWithMissing p f g xs' ys

    zipWithMatched _ _ _ [] _ = []
    zipWithMatched _ _ _ _ [] = []
    zipWithMatched p f g xs'@(x : xs) ys'@(y : ys) = case compare (p x) (p y) of
      LT ->         zipWithMatched p f g xs  ys'
      EQ -> f x y : zipWithMatched p f g xs  ys
      GT ->         zipWithMatched p f g xs' ys

    zipWithStrategy = case strategy of
      OnUnion -> zipWithMissing
      OnIntersection -> zipWithMatched

    scopeRange :: ScopeTree -> Range
    scopeRange ((_, r) :< _) = r

    descend :: [ScopeTree] -> [ScopeTree] -> [ScopeTree]
    descend xs ys = concat $ zipWithStrategy fst (go `on` snd) (pure . snd) (sortMap xs) (sortMap ys)
      where
        sortMap = sortWith fst . map (scopeRange &&& id)

    -- Merges the references of two 'ScopedDecl's in a right-biased fashion.
    -- In the current implementation, the compiler's scopes will be on the right
    -- and the fallback ones will be on the left.
    mergeRefs :: ScopedDecl -> ScopedDecl -> ScopedDecl
    mergeRefs l r = r
      { _sdRefs = unionOrd (_sdRefs r) (_sdRefs l)
      , _sdDoc  = unionOrd (_sdDoc  r) (_sdDoc  l)
      , _sdSpec = mergeValueSpecs (_sdSpec l) (_sdSpec r)
      }

    -- FromCompiler scope doesn't have correct `_vdsParams`.
    -- We have to take it from Fallback scope.
    -- FIXME (LIGO-679)
    mergeValueSpecs :: DeclarationSpecifics -> DeclarationSpecifics -> DeclarationSpecifics
    mergeValueSpecs (ValueSpec l) (ValueSpec r) = ValueSpec r{ _vdsParams = _vdsParams l }
    mergeValueSpecs _ r = r

    -- Merge two sets of DeclRefs preferring decls that have a smaller range
    -- (i.e., is more local than the other).
    mergeDecls :: Set DeclRef -> Set DeclRef -> Set DeclRef
    mergeDecls l r
      = mapStrategy
        (\(DeclRef n lr) (DeclRef _ rr) -> DeclRef n (if leq lr rr then lr else rr))
        (mapFromFoldable drName l)
        (mapFromFoldable drName r)
      & Map.elems
      & Set.fromList

    mapFromFoldable :: (Foldable f, Ord k) => (a -> k) -> f a -> Map k a
    mapFromFoldable f = Map.fromList . map (f &&& id) . Foldable.toList

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
      go' ((decls, r) :< list') =
        sexpr "scope" (pp r : map pp (toList decls) ++ [go list' | not $ null list'])

      decls' = sexpr "decls" . map (\(a, b) -> pp a <.> ":" `indent` pp b) . toPairs

lookupEnv :: Text -> Range -> Scope -> Maybe ScopedDecl
lookupEnv name pos = getFirst . foldMap \decl@ScopedDecl{..} ->
  First do
    guard (_sdName == name && (_sdOrigin == pos || pos `elem` _sdRefs))
    return decl

-- | return scoped declarations related to the range
envAtPoint :: Range -> ScopeForest -> Scope
envAtPoint pos (ScopeForest sf ds) = do
  let sp = sf >>= DList.toList . spine pos >>= toList
  map (ds Map.!) sp
  where
    -- find all nodes that spans the range and take their references
    spine :: Range -> ScopeTree -> DList (Set DeclRef)
    spine r ((decls, r') :< trees)
      | leq r r' = foldMap (spine r) trees `snoc` decls
      | otherwise = mempty

addLocalScopes
  :: SomeLIGO ParsedInfo
  -> ScopeForest
  -> SomeLIGO Info'
addLocalScopes tree forest =
  let
    getPreRange xs = let PreprocessedRange r = getElem xs in r
    defaultHandler f (i :< fs) = do
      fs' <- traverse f fs
      let env = envAtPoint (getPreRange i) forest
      return ((env :> Nothing :> i) :< fs')

    insertScope
      :: (Element f RawLigoList, Pretty (f (LIGO xs)))
      => Level
      -> Product ParsedInfo
      -> f (LIGO xs)
      -> Identity (Product Info', f (LIGO xs))
    insertScope level i name = do
      let env = envAtPoint (getPreRange i) forest
      -- TODO: This is temporary solution developed in LIGO-700, and should be fixed in LIGO-830
      let declaredScope = Map.lookup (DeclRef (ppToText name) (getRange i)) (sfDecls forest)
      return ((maybeToList declaredScope <> env) :> Just level :> i, name)

    insertTerm, insertType, insertModule
      :: (Element f RawLigoList, Pretty (f (LIGO xs)))
      => Product ParsedInfo
      -> f (LIGO xs)
      -> Identity (Product Info', f (LIGO xs))
    insertTerm   = insertScope TermLevel
    insertType   = insertScope TypeLevel
    insertModule = insertScope ModuleLevel
  in
  runIdentity $ withNestedLIGO tree $
    descent' @(Product ParsedInfo) @(Product Info') @RawLigoList @RawLigoList defaultHandler
    [ Descent @Name insertTerm
    , Descent @NameDecl insertTerm
    , Descent @Ctor insertTerm
    , Descent @TypeName insertType
    , Descent @TypeVariableName insertType
    , Descent @ModuleName insertModule
    ]

addScopes
  :: forall impl m
   . HasScopeForest impl m
  => TempSettings
  -> ProgressCallback m
  -> Includes ParsedContractInfo
  -> m (Includes ContractInfo')
addScopes tempSettings reportProgress graph = do
  -- Bottom-up: add children forests into their parents
  forestGraph <- scopeForest @impl tempSettings reportProgress graph
  let
    universe = nubForest $ foldr (mergeScopeForest OnUnion . contractTree) emptyScopeForest $ G.vertexList forestGraph
    -- Traverse the graph, uniting decls at each intersection, essentially
    -- propagating scopes
    addScope (_getContract -> sf) = do
      let src = _cFile sf
      let fp = srcPath src
      pc <- maybe (contractNotFoundException fp graph) pure (lookupContract fp graph)
      pure $ FindContract src
        (addLocalScopes (contractTree pc) (mergeScopeForest OnIntersection (_cTree sf) universe))
        (_cMsgs sf)
  Includes <$> traverseAMConcurrently addScope (getIncludes forestGraph)
  where
    nubRef sd = sd
      { _sdRefs = ordNub (_sdRefs sd)
      , _sdDoc  = ordNub (_sdDoc  sd)
      }
    nubForest f = f
      { sfScopes = ordNub (sfScopes f)
      , sfDecls  = Map.map nubRef (sfDecls f)
      }

-- | Attempt to find a contract in some adjacency map. O(log n)
lookupContract :: FilePath -> Includes (FindFilepath a) -> Maybe (FindFilepath a)
lookupContract fp g = fst <$> findKey contractFile fp (G.adjacencyMap g)

pattern FindContract :: Source -> info -> [Message] -> FindFilepath info
pattern FindContract f t m = FindFilepath (ParsedContract f t m)
{-# COMPLETE FindContract #-}

type ContractInfo       = FindFilepath (SomeLIGO Info)
type ParsedContractInfo = FindFilepath (SomeLIGO ParsedInfo)
type ContractInfo'      = FindFilepath (SomeLIGO Info')

contractNotFoundException :: MonadIO m => FilePath -> Includes (FindFilepath info) -> m a
contractNotFoundException fp (Includes g) =
  throwIO $ ContractNotFoundException fp $ Includes $ G.gmap contractFile g

data ContractNotFoundException = ContractNotFoundException
  { cnfeMissingFile :: FilePath
  , cnfeIncludedFiles :: Includes FilePath
  } deriving stock (Show)

instance Exception ContractNotFoundException where
  displayException ContractNotFoundException {cnfeMissingFile, cnfeIncludedFiles} =
    "Could not find contract '" <> cnfeMissingFile <> "'.\n"
    <> "Searched graph:\n"
    <> G.render (G.export vDoc eDoc cnfeIncludedFiles)
    where
      vDoc x   = G.literal x <> "\n"
      eDoc x y = G.literal x <> " -> " <> G.literal y <> "\n"

-- TODO: We should preferrably export it from AST.Includes, but it would create
-- cyclic imports.
newtype Includes info = Includes
  { getIncludes :: AdjacencyMap info
  } deriving stock (Eq, Show)
    deriving newtype (Graph, ToGraph)

instance (Ord info, FromJSON info) => FromJSON (Includes info) where
  parseJSON = withObject "Includes" \v -> Includes <$> do
    G.overlay
      <$> (G.vertices <$> v .: "vertices")
      <*> (G.edges    <$> v .: "edges")

instance (Ord info, ToJSON info) => ToJSON (Includes info) where
  toJSON includes = object
    [ "vertices" .= vertices
    , "edges" .= edges
    ]
    where
      vertices = toJSON $ G.vertexList includes
      edges = toJSON $ G.edgeList includes

deriving anyclass instance (Ord info, ToJSON info, ToObject info) => ToObject (Includes info)

instance (LogItem info, Ord info, ToJSON info) => LogItem (Includes info) where
  payloadKeys verbosity _includes
    | verbosity >= V3 = AllKeys
    | otherwise       = SomeKeys []
