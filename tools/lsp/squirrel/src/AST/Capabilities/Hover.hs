{-# LANGUAGE RecordWildCards #-}

-- | Hover code capability

module AST.Capabilities.Hover where

import qualified Language.Haskell.LSP.Types as LSP

import AST.Capabilities.Find
import AST.Scope
import AST.Skeleton

import Duplo.Pretty
import Product
import Range
import Data.Text (pack, intercalate)

hoverDecl
  :: CanSearch xs
  => Range
  -> LIGO xs
  -> Maybe LSP.Hover
hoverDecl at tree = do
  decl <- findScopedDecl at tree
  Just $ LSP.Hover
    { _contents = mkContents decl
    , _range = Just $ toLspRange at
    }

mkContents :: ScopedDecl -> LSP.HoverContents
mkContents ScopedDecl
  { _sdType   = ppToText -> _sdType
  , _sdName   = ppToText -> _sdName
  , _sdDoc    = ppToText -> _sdDoc
  , _sdOrigin = pack . show -> _sdOrigin
  -- TODO: more documentation
  } = LSP.HoverContents $ LSP.MarkupContent
  { _kind = LSP.MkMarkdown
  , _value = contentDoc
  } where
    contentDoc = intercalate "\n"
      [ _sdName <> " :: " <> _sdType
      , "\n"
      , "*defined at*"
      , _sdOrigin
      , "\n"
      , _sdDoc
      ]
