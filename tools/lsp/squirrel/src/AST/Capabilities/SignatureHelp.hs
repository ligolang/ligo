{-# LANGUAGE RecordWildCards #-}

module AST.Capabilities.SignatureHelp
  ( LSP.ParameterInformation (..)
  , LSP.SignatureInformation (..)
  , findSignature
  , getSignatureHelp
  , makeSignatureLabel
  , toLspParameters
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
import Duplo.Tree (layer, match, spineTo)

import AST.Capabilities.Find (CanSearch)
import AST.Scope.Common (Level (TermLevel), lookupEnv, ofLevel)
import AST.Scope.ScopedDecl (IsLIGO, Parameter (..), Pattern (..), ScopedDecl (..), lppLigoLike, _ValueSpec, vdsParams)
import AST.Skeleton (Expr (Apply, Paren, Tuple), LIGO, Lang (..))
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
  pure (i, ppToText (void name), map getRange $ extractTupleArguments params)

-- | If the function has only a single argument consisting of a tuple, then we
-- consider the tuple's positions to be the arguments and extract them here.
extractTupleArguments :: [LIGO xs] -> [LIGO xs]
extractTupleArguments = \case
  [arg] -> case layer arg of
    Just (Paren (layer -> Just (Tuple xs))) -> xs
    Just (Tuple xs) -> xs
    _ -> [arg]
  args -> args

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
  let label = makeSignatureLabel _sdDialect _sdName (map (ppToText . lppLigoLike _sdDialect) params)
  let sigInfo = LSP.SignatureInformation
        { _label = label
        , _documentation = Just (LSP.SignatureHelpDocString $ ppToText (fsep (map pp _sdDoc)))
        , _parameters = Just . LSP.List $ toLspParameters _sdDialect params
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

toLspParameters :: Lang -> [Parameter] -> [LSP.ParameterInformation]
toLspParameters dialect = \case
  [ParameterPattern (IsAnnot (IsTuple pats) _typ)] -> go pats
  [ParameterPattern (IsTuple pats)] -> go pats
  pats -> go pats
  where
    go :: IsLIGO a => [a] -> [LSP.ParameterInformation]
    go = map (toLspParameter . ppToText . lppLigoLike dialect)

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
