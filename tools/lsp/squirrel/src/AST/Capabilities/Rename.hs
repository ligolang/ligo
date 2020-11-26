-- | Rename request implementation.
module AST.Capabilities.Rename
  ( RenameDeclarationResult (..)
  , renameDeclarationAt
  , prepareRenameDeclarationAt
  ) where

import Data.Text (Text)
import qualified Language.LSP.Types as J

import AST.Capabilities.Find (CanSearch, findScopedDecl)
import AST.Scope.ScopedDecl (ScopedDecl (ScopedDecl, _sdOrigin, _sdRefs))
import AST.Skeleton (SomeLIGO)
import Range (Range, toLspRange)


-- | Result of trying to rename declaration.
data RenameDeclarationResult = Ok [J.TextEdit] | NotFound
  deriving stock (Eq, Show)


-- | Rename the declaration at the given position.
-- The position is given as a range, becuase that is how we do it, haha :/.
renameDeclarationAt
  :: CanSearch xs
  => Range -> SomeLIGO xs -> Text -> RenameDeclarationResult
renameDeclarationAt pos tree newName =
    case findScopedDecl pos tree of
      Nothing -> NotFound
      Just ScopedDecl{_sdRefs} -> Ok $
        -- XXX: _sdRefs includes the declaration itself too,
        -- so we do not add _sdOrigin.
        map (\r -> J.TextEdit (toLspRange r) newName) _sdRefs

-- | Like 'renameDeclarationAt' but does not actually rename anything,
-- only looks up the symbol being renamed and returns either @Nothing@
-- or its declaration range.
prepareRenameDeclarationAt
  :: CanSearch xs
  => Range -> SomeLIGO xs -> Maybe Range
prepareRenameDeclarationAt pos tree =
  _sdOrigin <$> findScopedDecl pos tree
