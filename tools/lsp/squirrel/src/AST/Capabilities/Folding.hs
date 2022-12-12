-- | Code folding utilities

module AST.Capabilities.Folding
  ( foldingAST
  , toFoldingRange
  ) where

import AST.Scope
import AST.Skeleton
import Control.Monad.Writer.Strict (execWriter, tell)
import Duplo.Tree
import Language.LSP.Types qualified as J

import Range

-- | Fold the given ast by visiting its nodes and applying provided
-- handler to them. This is used primarly to call `Core.sendFunc`
-- at each specific node we face.
-- TODO: may affect perfomance, if so we need to use `Endo` instead.
foldingAST :: LIGO Info' -> [Range]
foldingAST = execWriter . visit' handlers
  where
    handlers =
      [ Visit @Binding \(getRange -> r) -> \case
          BFunction {} -> tell [r]
          BTypeDecl {} -> tell [r]
          BModuleDecl {} -> tell [r]
          _ -> pass
      , Visit @Expr \(getRange -> r) -> \case
          If {} -> tell [r]
          Case {} -> tell [r]
          SwitchStm {} -> tell [r]
          Seq {} -> tell [r]
          Lambda {} -> tell [r]
          ForLoop {} -> tell [r]
          ForOfLoop {} -> tell [r]
          WhileLoop {} -> tell [r]
          ForBox {} -> tell [r]
          _ -> pass
      ]

toFoldingRange :: Range -> J.FoldingRange
toFoldingRange Range
  { _rStart  = (_startLine, Just -> _startCharacter, _)
  , _rFinish = (_endLine, Just -> _endCharacter, _)
  } = J.FoldingRange
  { _startLine = _startLine - 1
  , _startCharacter = pred <$> _startCharacter
  , _endLine = _endLine - 1
  , _endCharacter = pred <$> _endCharacter
  , _kind = Just J.FoldingRangeRegion
  }
