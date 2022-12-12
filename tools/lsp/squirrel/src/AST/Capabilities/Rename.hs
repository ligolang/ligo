-- | Rename request implementation.
module AST.Capabilities.Rename
  ( renameDeclarationAt
  , prepareRenameDeclarationAt
  ) where

import Data.HashMap.Strict qualified as HM
import Data.List (groupBy)
import Language.LSP.Types qualified as J

import AST.Capabilities.Find (CanSearch, definitionOf, findScopedDecl)
import AST.Scope.ScopedDecl (ScopedDecl (ScopedDecl, _sdRefs))
import AST.Skeleton (SomeLIGO)
import Range (Range, _rFile, toLspRange)
import Util (toUri)

-- | Rename the declaration at the given position.
-- The position is given as a range, because that is how we do it, haha :/.
renameDeclarationAt
  :: CanSearch xs
  => Range -> SomeLIGO xs -> Text -> Maybe J.WorkspaceEditMap
renameDeclarationAt pos tree newName =
  case findScopedDecl pos tree of
    Nothing -> Nothing
    Just ScopedDecl{_sdRefs} -> Just $
      -- XXX: _sdRefs includes the declaration itself too,
      -- so we do not add _sdOrigin.
      HM.fromList $ mapMaybe extractGroup $ groupBy ((==) `on` _rFile) $ sortOn _rFile _sdRefs
  where
    extractGroup :: [Range] -> Maybe (J.Uri, J.List J.TextEdit)
    extractGroup []         = Nothing
    extractGroup xs@(x : _) = Just (toUri x, J.List $ flip J.TextEdit newName . toLspRange <$> xs)

-- | Like 'renameDeclarationAt' but does not actually rename anything,
-- only looks up the symbol being renamed and returns either @Nothing@
-- or its declaration range.
prepareRenameDeclarationAt
  :: CanSearch xs
  => Range -> SomeLIGO xs -> Maybe Range
prepareRenameDeclarationAt = definitionOf
