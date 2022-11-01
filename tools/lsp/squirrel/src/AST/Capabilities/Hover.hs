-- | Hover code capability

module AST.Capabilities.Hover
  ( hoverDecl
  ) where

import Language.LSP.Types qualified as LSP

import AST.Capabilities.Find
import AST.Scope.ScopedDecl (DeclarationSpecifics (..), ScopedDecl (..), lppDeclCategory)
import AST.Skeleton

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
    contentDoc = ppToText $ mconcat
      [ case _sdSpec of
        TypeSpec{} -> "type " <> lppDeclCategory decl
        ModuleSpec{} -> "module " <> pp _sdName
        ValueSpec{} -> pp _sdName <> " : " <> lppDeclCategory decl
      , "\n\n"
      , "*defined at* " <> pp _sdOrigin
      , if null _sdDoc
        then ""
        else "\n\n" <> pp _sdDoc
      ]
