module AST.Capabilities.Completion
  ( CompleterM (..)
  , Completion (..)
  , CompleterEnv (..)
  , NameCompletion (..)
  , TypeCompletion (..)
  , DocCompletion (..)
  , completionName
  , complete
  , getPossibleCompletions
  , toCompletionItem
  , withCompleterM
  ) where

import Prelude hiding (Type)

import Algebra.Graph.AdjacencyMap qualified as G
import Control.Lens (element)
import Data.Char (isUpper)
import Data.HashSet qualified as HashSet (filter, toList)
import Data.List (isSubsequenceOf, nubBy)
import Data.Text qualified as Text
import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree
import Language.LSP.Types (CompletionDoc (..), CompletionItem (..), CompletionItemKind (..))
import System.FilePath
import Text.Regex.TDFA ((=~))
import Unsafe qualified

import AST.Capabilities.Find
  (CanSearch, TypeDefinitionRes (..), dereferenceTspec, findModuleDecl, findNodeAtPoint,
  typeDefinitionOf)
import AST.Pretty (PPableLIGO, docToText)
import AST.Scope
import AST.Scope.ScopedDecl
  (Accessor, DeclarationSpecifics (..), Scope, ScopedDecl (..), Type (..), TypeDeclSpecifics (..),
  TypeField (..), ValueDeclSpecifics (..), _RecordType, _TypeSpec, accessField, lppDeclCategory,
  lppLigoLike, sdSpec, tdsInit)
import AST.Skeleton hiding (Type)
import Log (Log, LogT, Logger, Namespace (..), getLogEnv, runKatipContextT)
import ParseTree
import Product
import Range
import Util (unconsFromEnd)

data CompleterEnv xs = CompleterEnv
  { ceRange  :: Range
  , ceTree   :: SomeLIGO xs
  , ceSource :: Source
  , ceGraph  :: Includes FilePath
  }

newtype CompleterM xs a = CompleterM
  { runCompleterM :: LogT (ReaderT (CompleterEnv xs) IO) a
  } deriving newtype (Functor, Applicative, Monad, MonadIO,
                      MonadReader (CompleterEnv xs), Log, Logger)

withCompleterM :: (MonadIO m, Logger m) => CompleterEnv xs -> CompleterM xs a -> m a
withCompleterM complEnv action = do
  env <- getLogEnv
  liftIO $ runReaderT
     (runKatipContextT env () (Log.Namespace ["Completion"]) $ runCompleterM action)
     complEnv

newtype NameCompletion = NameCompletion { getNameCompletion :: Text } deriving newtype (Eq, Show)
newtype TypeCompletion = TypeCompletion { getTypeCompletion :: Text } deriving newtype (Eq, Show)
newtype DocCompletion  = DocCompletion  { getDocCompletion  :: Text } deriving newtype (Eq, Show)

data Completion
  = Completion
      (Maybe CompletionItemKind)
      NameCompletion
      (Maybe TypeCompletion)
      DocCompletion
  | ImportCompletion
      NameCompletion
  | CompletionKeyword NameCompletion
  deriving stock (Eq, Show)

completionName :: Completion -> NameCompletion
completionName (Completion _ name _ _) = name
completionName (ImportCompletion name) = name
completionName (CompletionKeyword name) = name

type CompletionLIGO info =
  ( CanSearch info
  , PPableLIGO info
  )

complete :: CompletionLIGO xs => CompleterM xs (Maybe [Completion])
complete = do
  pos <- asks ceRange
  tree <- asks ceTree

  let maybeNode = findNodeAtPoint pos tree
  case maybeNode of
    Just node -> do
      let scope = getElem (extract node)
      let nameLevel = getElem (extract node)
      possibleCompl <- getPossibleCompletions scope nameLevel
      return $ possibleCompl
        <&> nubBy ((==) `on` getNameCompletion . completionName)
        <&> filter (filterFunc node)
    Nothing -> return Nothing
  where
    filterFunc node compl = case compl of
      ImportCompletion{} -> True
      otherCompl -> (isSubseqOf (ppToText node) . getNameCompletion . completionName) otherCompl

getPossibleCompletions
  :: CompletionLIGO xs
  => Scope -> Maybe Level -> CompleterM xs (Maybe [Completion])
getPossibleCompletions scope level = do
  pos <- asks ceRange
  tree <- asks ceTree
  source <- asks ceSource

  let
    cmf = completeModuleField scope pos tree
    cfta = completeFieldTypeAware scope pos tree
    cfs = completeFromScope scope level pos tree
    c = cmf <|> cfta <|> cfs
  ci <- completeImport pos source
  return $ mconcat
    [ ci
    , c
    , completeKeyword pos tree
    ]

parseAccessor :: PPableLIGO xs => LIGO xs -> Accessor
parseAccessor node = case reads (toString textValue) of
  [(num, "")] -> Left num
  _ -> Right textValue
  where
    textValue = ppToText node

completeImport :: forall xs. Range -> Source -> CompleterM xs (Maybe [Completion])
completeImport (Range (sl, sc, _) _ _) (Source fp _ fileText) = do
  let l = Text.take (fromIntegral @_ @Int sc - 1) $
            fromMaybe "" $
              lines fileText ^? element (fromIntegral @_ @Int sl - 1)
  if l =~ ("^#[ \t]*(include|import)[ \t]*\"$" :: Text)
  then possibleImportedFiles
  else pure Nothing
  where
    possibleImportedFiles :: CompleterM xs (Maybe [Completion])
    possibleImportedFiles = do
      (Includes graph) <- asks ceGraph
      if not (G.isEmpty graph)
      then Just . catMaybes <$> mapM makeCompletion (G.vertexList graph)
      else pure Nothing

    makeCompletion :: FilePath -> CompleterM xs (Maybe Completion)
    makeCompletion curFp =
      if curFp == fp
      then pure Nothing
      else pure $ Just $ ImportCompletion $ NameCompletion $ toText (fp `relativeTo` curFp)

    relativeTo :: FilePath -> FilePath -> FilePath
    relativeTo path1 path2 = go splitP1 splitP2
      where
        fn2 = takeFileName path2
        splitP1 = Unsafe.init $ splitDirectories path1
        splitP2 = Unsafe.init $ splitDirectories path2

        go :: [FilePath] -> [FilePath] -> FilePath
        go (f1:f1s) (f2:f2s) = if f1 == f2
                               then go f1s f2s
                               else let rel = joinPath $ flip replicate "../" $ length f1s in rel ++ joinPath f2s

        go [] rest = let pathToDir = joinPath rest
                      in if not (null pathToDir) then pathToDir ++ "/" ++ fn2 else fn2
        go more [] = joinPath (flip replicate "../" $ length more) ++ fn2

completeKeyword :: CompletionLIGO xs => Range -> SomeLIGO xs -> Maybe [Completion]
completeKeyword pos tree@(SomeLIGO dialect _) = do
  cover <- findNodeAtPoint pos tree
  let completions = HashSet.filter (ppToText cover `isSubseqOf`) keywords
  case HashSet.toList completions of
    [] -> Nothing
    completions' -> Just $ CompletionKeyword . NameCompletion <$> completions'
  where
    keywords :: HashSet Text
    keywords = case dialect of
      Caml   -> cameLIGOKeywords
      Pascal -> pascaLIGOKeywords
      Reason -> reasonLIGOKeywords
      Js     -> jsLIGOKeywords

completeFieldTypeAware
  :: CompletionLIGO xs => Scope -> Range -> SomeLIGO xs -> Maybe [Completion]
completeFieldTypeAware scope pos tree@(SomeLIGO dialect nested) = do
  QualifiedName{ qnSource, qnPath } <- asum (map layer covers)
  -- throwing away the last field, because it's the field we are trying to complete
  (finished, _unfinished) <- unconsFromEnd qnPath
  let accessors = map parseAccessor finished
  firstTspec <- toTspec (typeDefinitionOf (getRange qnSource) tree)
  finalTspec <- foldM accessAndDereference firstTspec accessors
  finalFields <- finalTspec ^? tdsInit . _RecordType
  pure (map mkCompletion finalFields)
  where
    covers = spineTo (leq pos . getRange) nested

    toTspec TypeNotFound = Nothing
    toTspec (TypeDeclared decl) = decl ^? sdSpec . _TypeSpec . _2  -- TODO (LIGO-331): Check this case
    toTspec (TypeInlined tspec) = Just tspec

    accessAndDereference :: TypeDeclSpecifics Type -> Accessor -> Maybe (TypeDeclSpecifics Type)
    accessAndDereference tspec accessor =
      dereferenceTspec scope <$> accessField tspec accessor

    mkCompletion :: TypeField -> Completion
    mkCompletion field = Completion
      (Just CiField)
      (NameCompletion $ _tfName field)
      (TypeCompletion . docToText . lppLigoLike dialect <$> _tfTspec field)
      (DocCompletion "")

extractType :: ScopedDecl -> Maybe Type
extractType ScopedDecl{_sdSpec} = case _sdSpec of
  TypeSpec _ TypeDeclSpecifics{_tdsInit} -> Just _tdsInit
  ModuleSpec _ -> Nothing
  ValueSpec ValueDeclSpecifics{_vdsTspec} -> _tdsInit <$> _vdsTspec

-- TODO: support completing module aliases, such as `module A = B._`, where `_`
-- is where the cursor is.
completeModuleField
  :: CompletionLIGO xs => Scope -> Range -> SomeLIGO xs -> Maybe [Completion]
completeModuleField scope pos tree@(SomeLIGO dialect nested) = do
  ModuleAccess{maPath} <- asum (map layer covers)
  let lastModuleName = Unsafe.last maPath
  moduleDecl <- findModuleDecl (getRange $ extract lastModuleName) tree
  let namespace = _sdNamespace moduleDecl <> AST.Scope.Namespace [_sdName moduleDecl]
  let decls = filter (\decl -> _sdNamespace decl == namespace) scope
  pure $ mkCompletion <$> decls
  where
    covers = spineTo (leq pos . getRange) nested

    mkCompletion :: ScopedDecl -> Completion
    mkCompletion decl = Completion
      (completionKind decl)
      (NameCompletion $ _sdName decl)
      (TypeCompletion . docToText . lppLigoLike dialect <$> extractType decl)
      (DocCompletion "")

completeFromScope
  :: CompletionLIGO xs => Scope -> Maybe Level -> Range -> SomeLIGO xs -> Maybe [Completion]
completeFromScope scope level pos (SomeLIGO _ nested) = Just
  [ asCompletion decl
  | decl <- scope
  , decl `fitsLevel` level
  , _sdNamespace decl == currentNamespace
  ]
  where
    nodes = spineTo (leq pos . getRange) nested
    currentNamespace = AST.Scope.Namespace $ foldr appendNamespace [] nodes
    appendNamespace node namespace = fromMaybe namespace do
      BModuleDecl moduleDecl _ <- layer node
      ModuleName moduleName <- layer moduleDecl
      pure $ moduleName : namespace

isLikelyConstr :: Text -> Bool
isLikelyConstr = maybe False (isUpper . fst) . Text.uncons

completionKind :: ScopedDecl -> Maybe CompletionItemKind
completionKind ScopedDecl {_sdName, _sdSpec} = case _sdSpec of
  TypeSpec _typeParams spec ->
    Just $ completeFromTSpec spec
  ModuleSpec _mspec ->
    Just CiModule
  ValueSpec ValueDeclSpecifics {_vdsParams, _vdsTspec} ->
    let completion = completeFromTSpec <$> _vdsTspec in
    bool
      (maybe completion (bool (Just CiFunction) completion . null) _vdsParams)
      (Just CiConstructor)
      (isLikelyConstr _sdName)

-- LSP is pretty OOP-centric; common FP data structures such as sum types or
-- tuples are not there. We improvised some of them for now.
completeFromTSpec :: TypeDeclSpecifics Type -> CompletionItemKind
completeFromTSpec TypeDeclSpecifics {_tdsInit} = case _tdsInit of
  RecordType _ -> CiStruct
  VariantType _ -> CiEnum
  TupleType _ -> CiStruct
  ApplyType _ _ -> CiFunction
  AliasType _ -> CiVariable
  ArrowType _ _ -> CiFunction
  VariableType _ -> CiTypeParameter
  ParenType t -> completeFromTSpec t

defCompletionItem :: Text -> CompletionItem
defCompletionItem label = CompletionItem
  { _label = label
  , _kind = Nothing
  , _detail = Nothing
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertTextFormat = Nothing
  , _textEdit = Nothing
  , _insertText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _xdata = Nothing
  , _tags = Nothing
  , _insertTextMode = Nothing
  }

toCompletionItem :: Completion -> CompletionItem
toCompletionItem c@(Completion cKind (NameCompletion cName) cType _) =
  (defCompletionItem cName)
    { _kind = cKind
    , _detail = (\(TypeCompletion cType') -> ": " <> cType') <$> cType
    , _documentation = mkDoc c
    }
toCompletionItem c@(ImportCompletion (NameCompletion cName)) =
  (defCompletionItem cName)
    { _kind = Just CiFile
    , _detail = Just cName
    , _insertText  = Just cName
    , _documentation = mkDoc c
    }
toCompletionItem (CompletionKeyword (NameCompletion cName)) =
  (defCompletionItem cName)
    { _kind = Just CiKeyword
    }

mkDoc :: Completion -> Maybe CompletionDoc
mkDoc (CompletionKeyword (NameCompletion _cName)) = Nothing
mkDoc (ImportCompletion (NameCompletion cName)) =
  Just . CompletionDocString $
    "Completion"
    <> " import of file " <> cName
mkDoc (Completion _cKind (NameCompletion cName) cType (DocCompletion cDoc)) =
  Just . CompletionDocString $
    cName
    <> maybe "" (\(TypeCompletion cType') -> " is of type " <> cType') cType <> ". "
    <> cDoc

asCompletion :: ScopedDecl -> Completion
asCompletion sd = Completion
  (completionKind sd)
  (NameCompletion $ ppToText (_sdName sd))
  (Just $ TypeCompletion $ docToText (lppDeclCategory sd))
  (DocCompletion  $ ppToText (fsep $ map pp $ _sdDoc sd))

isSubseqOf :: Text -> Text -> Bool
isSubseqOf l r = isSubsequenceOf (toString l) (toString r)

fitsLevel :: ScopedDecl -> Maybe Level -> Bool
fitsLevel decl = maybe True (`ofLevel` decl)
