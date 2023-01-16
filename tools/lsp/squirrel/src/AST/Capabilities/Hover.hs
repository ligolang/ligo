-- | Hover code capability

module AST.Capabilities.Hover
  ( hoverDecl
  ) where

import Prelude hiding (Type)

import Language.LSP.Types qualified as LSP

import AST.Capabilities.Find
import AST.Pretty (blockComment)
import AST.Scope.ScopedDecl
  (DeclarationSpecifics (..), Module (..), ModuleDeclSpecifics (..), Namespace (..),
  QuotedTypeParams (..), ScopedDecl (..), Type (..), TypeDeclSpecifics (..),
  ValueDeclSpecifics (..), lppLigoLike)
import AST.Skeleton (Lang (..), SomeLIGO)

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
mkContents ScopedDecl{..} = LSP.HoverContents LSP.MarkupContent
  { _kind = LSP.MkMarkdown
  , _value = ppToText contentDoc
  }
  where
    -- TODO (LIGO-447): Display function parameters.
    -- TODO (LIGO-887): Print more information about declarations.
    contentDoc :: Doc
    contentDoc = mconcat
      [ "```", dialectName, "\n"
      , case _sdSpec of
        TypeSpec tparams tspec -> typeDoc tparams tspec
        ModuleSpec mspec -> moduleDoc mspec
        ValueSpec vspec -> valueDoc vspec
      , "\n```"
      , "\n\n"
      , "*defined at* " <> pp _sdOrigin
      , if null _sdDoc
        then ""
        else "\n\n" <> pp _sdDoc
      ]

    name :: Doc
    name = pp _sdName

    dialectName :: Doc
    dialectName =
      case _sdDialect of
        Pascal -> "pascaligo"
        Caml   -> "cameligo"
        Js     -> "jsligo"

    typeDoc :: Maybe QuotedTypeParams -> TypeDeclSpecifics Type -> Doc
    typeDoc tparams tspec = typeHeader <+> typeBody
      where
        typeParams = maybe mempty (lppLigoLike _sdDialect) tparams
        typeHeader = "type" <+> case _sdDialect of
          Pascal -> name <.> typeParams <+> "is"
          Caml   -> typeParams <+> name <+> "="
          Js     -> name <.> typeParams <+> "="
        typeBody = lppLigoLike _sdDialect tspec

    moduleDoc :: ModuleDeclSpecifics -> Doc
    moduleDoc mspec = moduleHeader <+> moduleBody
      where
        moduleEqual = case _sdDialect of
          Pascal -> "is"
          Caml   -> "="
          Js     -> "="
        moduleHeader = case _sdDialect of
          Pascal -> "module"    <+> name
          Caml   -> "module"    <+> name
          Js     -> "namespace" <+> name
        moduleBody = case _mdsInit mspec of
          ModuleDecl -> case _sdDialect of
            Pascal -> moduleEqual <+> "{" <+> blockComment Pascal "..." <+> "}"
            Caml   -> moduleEqual <+> "struct" <+> blockComment Caml "..." <+> "end"
            Js     -> "{" <+> blockComment Js "..." <+> "};"
          ModuleAlias (Namespace ns) ->
            moduleEqual <+> hcat (punctuate "." $ map pp $ toList ns)

    valueDoc :: ValueDeclSpecifics -> Doc
    valueDoc vspec = name <+> ":" <+> valueType
      where
        valueType = maybe
          (blockComment _sdDialect "unknown")
          (lppLigoLike _sdDialect)
          (_vdsTspec vspec)
