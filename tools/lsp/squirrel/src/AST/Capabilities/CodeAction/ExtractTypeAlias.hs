-- | Extract type alias capability.
-- This capability allows for user to extract any type from a contract by
-- making a type alias for it and replacing all the occurences of the aliased
-- type with its alias.
--
-- For instance, having:

-- ```
-- #include "a.ligo"
-- #include "b.ligo"
--
-- function id (const i : int) : int * int is (i, i)
--
-- function f (const r : int * int) : int is 2
-- ```
--
-- with cursor pointing at `*` in `int * int` and selecting `extract type alias`
-- refactoring method this produces a `CodeAction` that replaces parts of the
-- contract tree to
--
-- ```
-- #include "a.ligo"
-- #include "b.ligo"
-- type x is int * int
--
-- function id (const i : int) : x is (i, i)
--
-- function f (const r : x) : int is 2
-- ```
--
-- note that type alias is added *after* the block with includes. I (awkure) decided
-- that's its more convenient this way.

module AST.Capabilities.CodeAction.ExtractTypeAlias
  ( typeExtractionCodeAction
  , extractedTypeNameAlias
  ) where

import Prelude hiding (Product (..), Sum, Type)

import Control.Monad.Trans.Writer (execWriter, tell)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Duplo
  (Cofree (..), Sum, Visit (..), extract, fastMake, inject, layer, leq, match, spineTo, visit')
import Language.LSP.Types qualified as J

import AST.Pretty
import AST.Scope
import AST.Skeleton
import Parser
import Product
import Range

extractedTypeNameAlias :: String
extractedTypeNameAlias = "extractedTypeName"

-- | Construct all code actions regarding type extraction for the contract.
typeExtractionCodeAction :: Range -> J.Uri -> SomeLIGO Info' -> [J.CodeAction]
typeExtractionCodeAction at uri (SomeLIGO dialect tree) = fromMaybe [] do
  (i, RawContract xs) <- match tree
  let
    isPreprocessor = isJust . layer @Preprocessor
    typeAliasName = genTypeName tree
    filteredSubtree = dropWhile isPreprocessor xs
    filteredSubtreeRange = getRange $ maybe i extract $ listToMaybe filteredSubtree
    strippedTree = fastMake i (RawContract filteredSubtree)
  extractedTree <- listToMaybe $ spineTo (leq at . getRange) strippedTree
  let
    -- When we have a type declaration like @type 'a id = 'a@, it's trivial to find type variables,
    -- but not so much when we have `let (type a) (x : a) : a = x`, as @a@ might be either a type
    -- name or a type variable. To make things worse, it's possible that the type declaration might
    -- even be nested inside a variable declaration, thus capturing unquoted type variables!
    --   To deal with it, we visit the entire spine of the tree, finding all type variable
    -- declarations, and use it to declare parametric types.
    --   We get every single type name and type variable name that is used, and also every single
    -- type name and type variable name that is declared in the spine and take this intersection.
    -- This allows us to handle two corner cases:
    -- * We declare a type variable but never use it, in which case we don't need to declare it.
    -- * We use a type variable but never find its declaration, in which case we keep the semantics
    -- that this is still undefined.
    --   This way, something must be defined and used to be bound. Note that even in the first case,
    -- this doesn't matter, as LIGO has no type applications and will yield an error if any type is
    -- underspecified.
    declTypeVars = extractInScopeTypeVariablesFrom at strippedTree
    usedTypeVars = extractTypeNames extractedTree
    typeVars = HS.intersection declTypeVars usedTypeVars
  (info, exactType, typeNode) <- case extractedTree of
    (match -> Just (info, TypeName tn)) -> Just (info, Right tn, Left tn)
    typeNode@(match @Type -> Just (info, exactType)) -> Just (info, Left exactType, Right typeNode)
    _ -> Nothing  -- Matched everything but type, ignore
  let
    typeEdits = makeReplaceTypeEdits typeAliasName exactType strippedTree
    replaceRange = getRange info
    typeAlias = constructTypeAlias dialect typeAliasName typeVars typeNode filteredSubtreeRange
  pure $ mkCodeAction uri replaceRange (typeEdits <> [typeAlias])

extractInScopeTypeVariablesFrom :: Range -> LIGO Info' -> HashSet Text
extractInScopeTypeVariablesFrom r =
  foldMap extractInScopeTypeVariables . spineTo ((r `leq`) . getRange)
  where
    getTypes = foldMap extractTypeNames

    extractInScopeTypeVariables (layer -> Just node) = case node of
      BFunction _ _ tys _ _ _ -> getTypes tys
      BVar _ tys _ _ -> getTypes tys
      BConst _ tys _ _ -> getTypes tys
      BTypeDecl _ tys _ -> getTypes (maybeToList tys)
      _ -> mempty
    extractInScopeTypeVariables (layer -> Just node) = case node of
      Lambda _ tys _ _ -> getTypes tys
      _ -> mempty
    extractInScopeTypeVariables _ = mempty

extractTypeNames :: LIGO Info' -> HashSet Text
extractTypeNames = execWriter . visit'
  [ Visit \_ (TypeVariableName typeVar) -> tell $ HS.singleton typeVar
  , Visit \_ (TypeName typeName) -> tell $ HS.singleton typeName
  ]

-- | Generate fresh type alias that is not found
-- in the given tree.
-- TODO: Somewhat the tree, passed to `typeExtractionCodeAction`
-- lacks of `ScopedDecl`s which is strange since we parse trees
-- in our ASTMap with scopes as well. So I (awkure) decided
-- that we return blank `extractedTypeNameAlias` instead for now
-- since user may want to rename the type anyway.
genTypeName :: LIGO Info' -> Text
genTypeName _tree =
  -- let
  --   decls = getElem @[SD.ScopedDecl] $ extract tree
  --   findInTree proposedName = flip Foldable.find decls \case
  --     SD.ScopedDecl{_sdName = givenName, _sdSpec = SD.TypeSpec _}
  --       | proposedName == givenName -> True
  --     _ -> False
  --   name
  --     = head
  --     . filterOutFirst (isJust . findInTree)
  --     $ ("t"<>) . T.pack . show @Integer <$> [0..]
  toText extractedTypeNameAlias

-- | Reconstructs type definition node from given alias name and
-- either if it's a typename or some other complex type.
constructTypeAlias
  :: Lang
  -> Text -- ^ Given type alias
  -> HashSet Text -- ^ Type variables in the node
  -> Either Text (LIGO Info') -- ^ Either type name or type node
  -> Range -- ^ Range of the topmost level of the stripped tree
  -> J.TextEdit
constructTypeAlias dialect alias typeVars t Range{_rStart = (sl, sc, _)} =
  J.TextEdit
    { _range = toLspRange $ point sl sc
    , _newText = (<>"\n") . show . lppDialect @(LIGO Info') dialect $
        case t of
          Left typeName ->
            defaultState :< inject @Binding
              (BTypeDecl
                (defaultState :< inject @TypeName (TypeName alias))
                typeVarNode
                (defaultState :< mkTypeVar typeName))
          Right typeNode ->
            defaultState :< inject @Binding
              (BTypeDecl
                (defaultState :< inject @TypeName (TypeName alias))
                typeVarNode
                (rewriteTypes typeNode))
    }
  where
    mkTypeVar :: Text -> Sum RawLigoList v
    mkTypeVar name
      | name `HS.member` typeVars = inject $ TypeVariableName name
      | otherwise                 = inject $ TypeName name

    -- | If the given 'TypeName' is bound as a parametric type, rewrite it as a 'TypeVariableName'.
    rewriteTypes :: LIGO Info' -> LIGO Info'
    rewriteTypes = loop \case
      (match -> Just (i, TypeName name)) ->
        if name `HS.member` typeVars then fastMake i $ TypeVariableName name else fastMake i $ TypeName name
      x -> x

    typeVarNode :: Maybe (LIGO Info')
    typeVarNode = case (defaultState :<) . inject . TypeVariableName <$> HS.toList typeVars of
      []     -> Nothing
      [tVar] -> Just $ defaultState :< inject (QuotedTypeParam tVar)
      tVars  -> Just $ defaultState :< inject (QuotedTypeParams tVars)

defaultState :: Product Info'
defaultState = [] :> Nothing :> PreprocessedRange (point 1 1) :> [] :> [] :> point 1 1 :> CodeSource "" :> Nil

-- | Diagnostics collected for every type that allows for
-- type extraction code action to be clicked.
mkDiagnostics :: Range -> J.Diagnostic
mkDiagnostics r = J.Diagnostic
  { _range = toLspRange r
  , _severity = Just J.DsHint
  , _code = Nothing
  , _source = Nothing
  , _message = "extract type alias"
  , _tags = Nothing
  , _relatedInformation = Nothing
  }

-- | Constructs code action for type extraction.
mkCodeAction :: J.Uri -> Range -> [J.TextEdit] -> [J.CodeAction]
mkCodeAction uri replaceRange typeEdits =
  [ J.CodeAction
    { _title = "extract type definition"
    , _kind = Just J.CodeActionRefactorExtract
    , _diagnostics = Just $ J.List [mkDiagnostics replaceRange]
    , _isPreferred = Just False
    , _disabled = Nothing
    , _edit = Just J.WorkspaceEdit
        {
          _changes = Just $ HM.fromList [(uri, J.List typeEdits)]
          -- TODO: It's implied to use `_documentChanges` instead of `_changes` in case
          -- we are changing only one file, but, as it happens, vscode editor rejects
          -- these changes without any proper explanation why. Meaning that even though
          -- client received response of this code action, the text itself remains unchanged.
        , _documentChanges = Nothing
        , _changeAnnotations = Nothing
        }
    , _command = Nothing
    , _xdata = Nothing
    }
  ]

-- | Construct edits for replacing existing type that matches either
-- the given constructed type or type alias.
makeReplaceTypeEdits
  :: Text -- New type name to be replaced with
  -> Either (Type (LIGO Info')) Text -- Either it's a constructed type or a type alias
  -> LIGO Info'
  -> [J.TextEdit]
makeReplaceTypeEdits newTypeName (Left typeNode) =
  execWriter . visit'
    [ Visit @Type \(getRange -> r) -> \case
        typeNode' | typeNode == typeNode' ->
          tell [J.TextEdit { _range = toLspRange r, _newText = newTypeName }]
        _ -> pass
    ]
makeReplaceTypeEdits newTypeName (Right oldTypeName) =
  execWriter . visit'
    [ Visit @TypeName \(getRange -> r) -> \case
        TypeName typeName' | oldTypeName == typeName' ->
          tell [J.TextEdit { _range = toLspRange r, _newText = newTypeName }]
        _ -> pass
    ]
