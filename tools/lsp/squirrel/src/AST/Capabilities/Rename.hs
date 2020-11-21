-- | Rename request implementation.
module AST.Capabilities.Rename
  ( renameDeclarationAt
  ) where

import Control.Lens ((%~), _Just)
import Data.Function ((&))
import Data.Text (Text)
import qualified Language.Haskell.LSP.Types as J

import AST.Capabilities.Find (CanSearch, referencesOf)
import AST.Skeleton (LIGO)
import Range (Range, toLspRange)


-- | Rename the declaration at the given position.
-- The position is given as a range, becuase that is how we do it, haha :/.
renameDeclarationAt
  :: CanSearch xs
  => Range -> LIGO xs -> Text -> Maybe [J.TextEdit]
renameDeclarationAt pos tree newName =
    allReferences & _Just . traverse %~ \x -> J.TextEdit (toLspRange x) newName
  where
    allReferences :: Maybe [Range]
    {- XXX: referencesOf returns the declaration itself too.
    allReferences = referencesOf pos tree <> fmap (\x -> [x]) (definitionOf pos tree)
    -}
    allReferences = referencesOf pos tree
