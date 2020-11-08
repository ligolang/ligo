{-# LANGUAGE RecordWildCards #-}

module AST.Capabilities.SignatureHelp
  ( LSP.ParameterInformation (..)
  , LSP.SignatureInformation (..)
  , findSignatures
  , getSignatureHelp
  , makeSignatureLabel
  ) where

import qualified Language.Haskell.LSP.Types as LSP
  (List (..), ParameterInformation (..), SignatureHelp (..), SignatureInformation (..))

import Control.Monad (void)
import Data.Foldable (asum)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text (intercalate)
import Duplo.Lattice (leq)
import Duplo.Pretty (fsep, pp, ppToText)
import Duplo.Tree (match, spineTo)

import AST.Capabilities.Find (CanSearch)
import AST.Scope (Parameter (..), ScopedDecl (..))
import AST.Scope.Common (Category (Variable), lookupEnv, ofCategory)
import AST.Skeleton (Expr (Apply), LIGO)
import Product (Product, getElem)
import Range (Range)

-- | Find a 'ScopedDecl' of a function that is applied at the given position.
findNestingFunction
  :: CanSearch xs => LIGO xs -> Range -> Maybe ScopedDecl
findNestingFunction tree position = do
  (callInfo, fName) <- asum (map extractFunctionCall covers)
  let termEnv = filter (ofCategory Variable) (getElem callInfo)
  lookupEnv fName termEnv
  where
    covers = spineTo (leq position . getElem) tree

-- | If the given tree is a function application, extract it's information
-- characteristics and the function's name.
extractFunctionCall :: LIGO xs -> Maybe (Product xs, Text)
extractFunctionCall tree = do
  (i, Apply name _) <- match tree
  pure (i, ppToText (void name))

-- | Find all function signatures (one in this implementation) that could be
-- applied at the given position. A function signature includes its label which
-- is what will represent the function, its documentation comments and its
-- parameters (if present, they must be a part of the label). Parameters might
-- be highlighted by the editor.
findSignatures
  :: CanSearch xs => LIGO xs -> Range -> [LSP.SignatureInformation]
findSignatures tree position = maybeToList do
  ScopedDecl{..} <- findNestingFunction tree position
  params <- _sdParams
  let label = makeSignatureLabel _sdName paramLabels
      paramLabels = map parPresentation params
  pure LSP.SignatureInformation
    { _label = label
    , _documentation = Just (ppToText (fsep (map pp _sdDoc)))
    , _parameters = Just (map toLspParameter paramLabels)
    }

-- | Make a function signature label by a function name and its parameters.
makeSignatureLabel :: Text -> [Text] -> Text
makeSignatureLabel name params
  = name <> " (" <> Text.intercalate ", "  params <> ")"

-- | Make a 'ParameterInformation' by a parameter's name. For now, we don't
-- support parameter docs.
toLspParameter :: Text -> LSP.ParameterInformation
toLspParameter name = LSP.ParameterInformation name Nothing

-- | Find all function signatures at the given position and wrap them up in a
-- 'SignatureHelp'.
--
-- See 'findSignatures'.
getSignatureHelp
  :: CanSearch xs => LIGO xs -> Range -> LSP.SignatureHelp
getSignatureHelp tree position = LSP.SignatureHelp
  { _signatures = LSP.List (findSignatures tree position)
  , _activeSignature = Just 0
  , _activeParameter = Nothing
  }
