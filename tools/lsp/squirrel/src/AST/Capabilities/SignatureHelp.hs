{-# LANGUAGE RecordWildCards #-}

module AST.Capabilities.SignatureHelp
  ( getSignatureHelp
  , findSignatures
  , LSP.SignatureInformation (..)
  ) where

import qualified Language.Haskell.LSP.Types as LSP
  (List (..), SignatureHelp (..), SignatureInformation (..))

import Control.Monad (void)
import Data.Foldable (asum)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Duplo.Lattice (leq)
import Duplo.Pretty (fsep, pp, ppToText)
import Duplo.Tree (match, spineTo)

import AST.Capabilities.Find (CanSearch)
import AST.Scope (ScopedDecl (..))
import AST.Scope.Common (Category (Variable), lookupEnv, ofCategory)
import AST.Skeleton (Expr (Apply), LIGO)
import Product (Product, getElem)
import Range (Range)

findNestingFunction
  :: CanSearch xs => LIGO xs -> Range -> Maybe ScopedDecl
findNestingFunction tree position = do
  (callInfo, fName, _) <- asum (map extractFunctionCall covers)
  let termEnv = filter (ofCategory Variable) (getElem callInfo)
  lookupEnv fName termEnv
  where
    covers = spineTo (leq position . getElem) tree

extractFunctionCall :: LIGO xs -> Maybe (Product xs, Text, [LIGO xs])
extractFunctionCall (match -> Just (i,  Apply name body))
  = Just (i, ppToText (void name), body)
extractFunctionCall _ = Nothing

findSignatures
  :: CanSearch xs => LIGO xs -> Range -> [LSP.SignatureInformation]
findSignatures tree position = maybeToList do
  ScopedDecl{..} <- findNestingFunction tree position
  pure LSP.SignatureInformation
    { _label = _sdName
    , _documentation = Just (ppToText (fsep $ map pp _sdDoc))
    , _parameters = Nothing
    }

getSignatureHelp
  :: CanSearch xs => LIGO xs -> Range -> LSP.SignatureHelp
getSignatureHelp tree position = LSP.SignatureHelp
  { _signatures = LSP.List (findSignatures tree position)
  , _activeSignature = Nothing
  , _activeParameter = Nothing
  }
