module AST.Capabilities.Completion where

import Language.LSP.Types (CompletionDoc (..), CompletionItem (..), CompletionItemKind (..))

import Control.Lens ((^?))
import Control.Monad (foldM)
import Data.Foldable (asum)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (isSubsequenceOf, nubBy)
import Data.Text (Text)
import Data.Text qualified as Text
import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree

import AST.Capabilities.Find
  (TypeDefinitionRes (..), dereferenceTspec, findNodeAtPoint, typeDefinitionOf)
import AST.Pretty (PPableLIGO, docToText)
import AST.Scope
import AST.Scope.ScopedDecl
  (Accessor, DeclarationSpecifics (..), Scope, ScopedDecl (..), TypeDeclSpecifics (..),
  TypeField (..), _RecordType, _TypeSpec, accessField, lppDeclCategory, lppLigoLike, sdSpec,
  tdsInit)
import AST.Skeleton
import Product
import Range
import Util (unconsFromEnd)

data Completion = Completion
  { cName :: Text
  , cType :: Text
  , cDoc  :: Text
  }
  deriving stock (Eq, Show)

type CompletionLIGO info =
  ( Eq (Product info)
  , Modifies (Product info)
  , Contains Range info
  , Contains Scope info
  , Contains (Maybe Level) info
  , PPableLIGO info
  )

complete :: CompletionLIGO xs => Range -> SomeLIGO xs -> Maybe [Completion]
complete pos tree = do
  node <- findNodeAtPoint pos tree
  let scope = getElem (extract node)
  let nameLevel = getElem (extract node)
  getPossibleCompletions scope nameLevel pos tree
    <&> nubBy ((==) `on` cName)
    <&> filter (isSubseqOf (ppToText node) . cName)

getPossibleCompletions
  :: CompletionLIGO xs
  => Scope -> Maybe Level -> Range -> SomeLIGO xs -> Maybe [Completion]
getPossibleCompletions scope level pos tree = asum completers
  where
    completers =
      [ completeField scope pos tree
      , completeFromScope scope level
      ]

parseAccessor :: PPableLIGO xs => LIGO xs -> Accessor
parseAccessor node = case reads (Text.unpack textValue) of
  [(num, "")] -> Left num
  _ -> Right textValue
  where
    textValue = ppToText node

completeField
  :: CompletionLIGO xs => Scope -> Range -> SomeLIGO xs -> Maybe [Completion]
completeField = completeFieldTypeAware

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
    toTspec (TypeDeclared decl) = decl ^? sdSpec . _TypeSpec
    toTspec (TypeInlined tspec) = Just tspec

    accessAndDereference :: TypeDeclSpecifics -> Accessor -> Maybe TypeDeclSpecifics
    accessAndDereference tspec accessor =
      dereferenceTspec scope <$> accessField tspec accessor

    mkCompletion field = Completion
      { cName = _tfName field
      , cType = docToText (lppLigoLike dialect (_tfTspec field))
      , cDoc = ""
      }

completeFromScope :: Scope -> Maybe Level -> Maybe [Completion]
completeFromScope scope level
  = Just [asCompletion decl | decl <- scope, decl `fitsLevel` level]

toCompletionItem :: Completion -> CompletionItem
toCompletionItem c@Completion
  { cName = cName
  , cType = cType
  , cDoc  = _cDoc
  } = CompletionItem
  { _label = cName
  , _kind = Just CiFunction -- TODO
  , _detail = Just $ ":: " <> cType -- TODO: more elaborate info
  , _documentation = Just $ mkDoc c
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

mkDoc :: Completion -> CompletionDoc
mkDoc Completion
  { cName = cName
  , cType = cType
  , cDoc  = cDoc
  } = CompletionDocString $
  cName <> " is of type " <> cType <> ". " <> cDoc

asCompletion :: ScopedDecl -> Completion
asCompletion sd = Completion
  { cName = ppToText (_sdName sd)
  , cType = docToText (lppDeclCategory sd)
  , cDoc  = ppToText (fsep $ map pp $ _sdDoc sd)
  }

isSubseqOf :: Text -> Text -> Bool
isSubseqOf l r = isSubsequenceOf (Text.unpack l) (Text.unpack r)

fitsLevel :: ScopedDecl -> Maybe Level -> Bool
fitsLevel _ Nothing = True
fitsLevel decl (Just level) = case (_sdSpec decl, level) of
  (ValueSpec{}, TermLevel) -> True
  (TypeSpec{}, TypeLevel) -> True
  _ -> False
