module AST.CodeAction
  ( collectCodeActions
  ) where

import Language.LSP.Types qualified as J

import AST.Capabilities.CodeAction.ExtractTypeAlias
import AST.Scope
import AST.Skeleton
import Range

collectCodeActions
  :: Range
  -> J.CodeActionContext
  -> J.Uri
  -> SomeLIGO Info'
  -> [J.CodeAction]
collectCodeActions at _con = typeExtractionCodeAction at
