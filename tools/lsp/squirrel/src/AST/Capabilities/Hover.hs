{-# LANGUAGE RecordWildCards #-}

-- | Hover code capability

module AST.Capabilities.Hover where

import Language.LSP.Types qualified as LSP

import AST.Capabilities.Find
import AST.Pretty (docToText)
import AST.Scope.ScopedDecl (ScopedDecl (..), lppDeclCategory)
import AST.Skeleton

import Data.Text (intercalate, pack)
import Duplo.Pretty
import Range

hoverDecl
  :: CanSearch xs
  => Range
  -> SomeLIGO xs
  -> Maybe LSP.Hover
hoverDecl at tree = do
  decl <- findScopedDecl at tree
  Just $ LSP.Hover
    { _contents = mkContents decl
    , _range = Just $ toLspRange at
    }

mkContents :: ScopedDecl -> LSP.HoverContents
mkContents decl@ScopedDecl{ .. } = LSP.HoverContents $ LSP.MarkupContent
  { _kind = LSP.MkMarkdown
  , _value = contentDoc
  }
  where
    contentDoc = intercalate "\n"
      [ ppToText _sdName <> " :: " <> docToText (lppDeclCategory decl)
      , "\n"
      , "*defined at*"
      , pack (show _sdOrigin)
      , "\n"
      , ppToText _sdDoc
      ]
