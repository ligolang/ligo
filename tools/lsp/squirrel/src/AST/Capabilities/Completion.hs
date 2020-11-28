module AST.Capabilities.Completion where

import Language.LSP.Types (CompletionDoc (..), CompletionItem (..), CompletionItemKind (..))

import Data.Function (on)
import Data.List (isSubsequenceOf, nubBy)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree

import AST.Pretty (docToText)
import AST.Scope
import AST.Scope.ScopedDecl (DeclarationSpecifics (..), ScopedDecl (..), lppDeclCategory)
import AST.Skeleton
import Product
import Range

data Completion = Completion
  { cName :: Text
  , cType :: Text
  , cDoc  :: Text
  }
  deriving (Show)

complete
  :: ( Eq (Product xs)
     , Modifies (Product xs)
     , Contains Range xs
     , Contains [ScopedDecl] xs
     , Contains (Maybe Level) xs
     )
  => Range
  -> LIGO xs
  -> Maybe [Completion]
complete r tree = do
  let l = spineTo (leq r . getElem) tree
  word <- listToMaybe l
  let scope = getElem (extract word)
  let nameLevel = getElem (extract word)
  return
    $ filter (isSubseqOf (ppToText word) . cName)
    $ nubBy ((==) `on` cName)
    $ map asCompletion
    $ filter (`fitsLevel` nameLevel)
    $ scope

toCompletionItem :: Completion -> CompletionItem
toCompletionItem c@Completion
  { cName = cName
  , cType = cType
  , cDoc  = _cDoc
  } = CompletionItem
  { _label = cName
  , _kind = Just $ CiFunction -- TODO
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
