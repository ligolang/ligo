{-# LANGUAGE RecordWildCards #-}

module AST.Capabilities.SignatureHelp
  ( LSP.ParameterInformation (..)
  , LSP.SignatureInformation (..)
  , findSignature
  , getSignatureHelp
  , makeSignatureLabel
  , toLspParameter
  ) where

import Language.LSP.Types qualified as LSP
  (List (..), ParameterInformation (..), ParameterLabel (..), SignatureHelp (..),
   SignatureHelpDoc (..), SignatureInformation (..))

import Control.Lens (_1, _2, _Just, (^..), (^?))
import Control.Monad (void)
import Data.Foldable (asum)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate, unwords)
import Duplo.Lattice (leq)
import Duplo.Pretty (fsep, pp, ppToText)
import Duplo.Tree (match, spineTo)

import AST.Capabilities.Find (CanSearch)
import AST.Scope.Common (Level (TermLevel), lookupEnv, ofLevel)
import AST.Scope.ScopedDecl (Parameter (..), ScopedDecl (..), _ValueSpec, vdsParams)
import AST.Skeleton (Expr (Apply), LIGO, Lang (..))
import Product (Contains, Product, getElem)
import Range (Range (..), getRange)

-- | Find a 'ScopedDecl' of a function that is applied at the given position and the active
-- parameter number (zero if no parameters).
findNestingFunction
  :: CanSearch xs => LIGO xs -> Range -> Maybe (ScopedDecl, Int)
findNestingFunction tree position = do
  (callInfo, fName, paramRanges) <- asum (map extractFunctionCall covers)
  let termEnv = filter (ofLevel TermLevel) (getElem callInfo)
  decl <- lookupEnv fName termEnv
  pure (decl, activeParamNo paramRanges)
  where
    covers = spineTo (leq position . getElem) tree
    activeParamNo paramRanges
      = fromMaybe 0 (findIndex (`notToTheLeftOf` position) paramRanges)
    Range _ finish _ `notToTheLeftOf` Range start _ _ = finish >= start

-- | If the given tree is a function application, extract it's information
-- characteristics, the function's name and applied parameters' ranges.
extractFunctionCall
  :: Contains Range xs => LIGO xs -> Maybe (Product xs, Text, [Range])
extractFunctionCall tree = do
  (i, Apply name params) <- match tree
  pure (i, ppToText (void name), map getRange params)

-- | Find all function signatures (one in this implementation) that could be
-- applied at the given position. A function signature includes its label which
-- is what will represent the function, its documentation comments and its
-- parameters (if present, they must be a part of the label). Parameters might
-- be highlighted by the editor.
findSignature
  :: CanSearch xs => LIGO xs -> Range -> Maybe (LSP.SignatureInformation, Int)
findSignature tree position = do
  (ScopedDecl{..}, activeNo) <- findNestingFunction tree position
  params <- _sdSpec ^? _ValueSpec . vdsParams . _Just
  let label = makeSignatureLabel _sdDialect _sdName paramLabels
      paramLabels = map parPresentation params
  let sigInfo = LSP.SignatureInformation
        { _label = label
        , _documentation = Just (LSP.SignatureHelpDocString $ ppToText (fsep (map pp _sdDoc)))
        , _parameters = Just . LSP.List $ map toLspParameter paramLabels
        , _activeParameter = Nothing
        }
  pure (sigInfo, activeNo)

-- | Make a function signature label by a dialect, a function name and its parameters.
makeSignatureLabel :: Lang -> Text -> [Text] -> Text
makeSignatureLabel Pascal name params
  = "function " <> name <> " (" <> Text.intercalate "; " params <> ")"
makeSignatureLabel Caml name params
  = "let " <> name <> " " <> Text.unwords params
makeSignatureLabel Reason name params
  = "let " <> name <> " = " <> Text.unwords params

-- | Make a 'ParameterInformation' by a parameter's name. For now, we don't
-- support parameter docs.
toLspParameter :: Text -> LSP.ParameterInformation
toLspParameter name = LSP.ParameterInformation (LSP.ParameterLabelString name) Nothing

-- | Find all function signatures at the given position and wrap them up in a
-- 'SignatureHelp'.
--
-- See 'findSignatures'.
getSignatureHelp
  :: CanSearch xs => LIGO xs -> Range -> LSP.SignatureHelp
getSignatureHelp tree position = LSP.SignatureHelp
  { _signatures = LSP.List (sigResult ^.. _Just . _1)
  , _activeSignature = Just 0
  , _activeParameter = sigResult ^? _Just . _2
  }
  where
    sigResult = findSignature tree position
