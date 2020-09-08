{-# LANGUAGE RecordWildCards #-}

module AST.Capabilities.DocumentSymbol where

import Control.Monad.Catch.Pure (MonadCatch)
import Control.Monad.Writer.Strict
import Data.Maybe (fromMaybe)
import Data.Text
import Duplo (match)
import Duplo.Pretty
import Duplo.Tree (Visit (Visit), visit)
import Language.Haskell.LSP.Types (SymbolInformation (..))
import qualified Language.Haskell.LSP.Types as J

import AST.Capabilities.Find
import AST.Scope
import AST.Skeleton
import Product
import Range
-- | Extract document symbols for some specific parsed ligo contract which
-- is realisable by @haskell-lsp@ client.
extractDocumentSymbols
  :: forall m.
     (MonadCatch m)
  => J.Uri
  -> LIGO Info'
  -> m [SymbolInformation]
extractDocumentSymbols uri tree = execWriterT . visit handlers $ tree
  where
    handlers =
      [ Visit @Binding $ \case
          (_, Function _ (match @Name -> Just (getElem @Range -> r, _)) _ _ _)->
            tellScopedDecl
              r
              J.SkFunction
              (\_ -> Nothing)

          -- TODO: currently we do not count imports as declarations in scopes
          (_, Include (match @Constant -> Just (getElem @Range -> r, _))) ->
            tellSymbolInfo
              r
              J.SkNamespace
              ("some import at " <> pack (show r))

          (_, TypeDecl (match @TypeName -> Just (getElem @Range -> r, _)) _) ->
            tellScopedDecl
              r
              J.SkTypeParameter
              (\_ -> Nothing)

          (_, Const (match @Name -> Just (getElem @Range -> r, _)) _ _) ->
            tellScopedDecl
              r
              J.SkConstant
              (\ScopedDecl {_sdName} -> Just ("const " <> _sdName))

          (_, Var (match @Name -> Just (getElem @Range -> r, _)) _ _) ->
            tellScopedDecl
              r
              J.SkVariable
              (\ScopedDecl {_sdName} -> Just ("var " <> _sdName))

          _ -> pure ()
      ]

    -- | Tries to find scoped declaration and apply continuation to it or
    -- ignore the declaration if not found.
    withScopedDecl
      :: Range
      -> (ScopedDecl -> WriterT [SymbolInformation] m ())
      -> WriterT [SymbolInformation] m ()
    withScopedDecl r f = maybe (pure ()) f (findScopedDecl r tree)

    -- | Tell to the writer symbol information that we may find in scope or
    -- just ignore it and return `[]`.
    tellScopedDecl
      :: Range
      -> J.SymbolKind
      -> (ScopedDecl -> Maybe Text)
      -> WriterT [SymbolInformation] m ()
    tellScopedDecl range kind mkName =
      withScopedDecl range $ \sd@ScopedDecl{..} ->
        tell
          [ SymbolInformation
              { _name = fromMaybe _sdName (mkName sd)
              , _deprecated = Nothing
              , _kind = kind
              , _containerName = matchContainerName kind
              , _location = J.Location uri $ toLSPRange range
              }
          ]

    -- | Tell to the writer some arbitrary symbol info. This is similar to
    -- @tellScopedDecl@ but it does not search for symbol in scope and always
    -- returns non-empty value.
    tellSymbolInfo
      :: Range
      -> J.SymbolKind
      -> Text
      -> WriterT [SymbolInformation] m ()
    tellSymbolInfo range kind name =
        tell
          [ SymbolInformation
              { _name = name
              , _deprecated = Nothing
              , _kind = kind
              , _containerName = matchContainerName kind
              , _location = J.Location uri $ toLSPRange range
              }
          ]

    -- | Helper function that associates container name with its container type
    -- defined for the sole purpose of minimising the amount of arguments for `tellScopedDecl`.
    matchContainerName = \case
      J.SkTypeParameter -> Just "type"
      J.SkNamespace -> Just "import"
      J.SkConstant -> Just "const declaration"
      J.SkVariable -> Just "var declaration"
      J.SkFunction -> Just "function"
      _ -> Nothing
