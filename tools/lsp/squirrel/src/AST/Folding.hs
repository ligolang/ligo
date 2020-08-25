{-# LANGUAGE RecordWildCards #-}

-- | Code folding utilities

module AST.Folding where

import qualified Language.Haskell.LSP.Types as J

import Duplo.Tree

import AST.Scope
import AST.Skeleton
import Control.Monad.Catch.Pure (MonadCatch)

import Control.Monad.Writer.Strict
import Product
import Range

-- | Fold the given ast by visiting its nodes and applying provided
-- handler to them. This is used primarly to call `Core.sendFunc`
-- at each specific node we face.
-- TODO: may affect perfomance, if so we need to use `Endo` instead.
foldingAST
  :: (MonadCatch m)
  => LIGO Info'
  -> m [Range]
foldingAST = execWriterT . visit handlers
  where
    handlers =
      [ Visit @Binding $ \case
          (getElem @Range -> r, Function {}) -> tell [r]
          (getElem @Range -> r, TypeDecl {}) -> tell [r]
          -- TODO: include blocks?
          _ -> pure ()
      ]

toFoldingRange :: Range -> J.FoldingRange
toFoldingRange Range
  { rStart  = (_startLine, Just -> _startCharacter, _)
  , rFinish = (_endLine, Just -> _endCharacter, _)
  } = J.FoldingRange
  { _startLine
  , _startCharacter
  , _endLine
  , _endCharacter
  , _kind = Just J.FoldingRangeRegion
  }

