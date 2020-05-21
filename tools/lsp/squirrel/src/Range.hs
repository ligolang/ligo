
module Range where

import Control.Lens

import Pretty

-- | A continuous location in text.
data Range = Range
  { rStart  :: (Int, Int, Int)  -- ^ [Start: line, col, byte-offset...
  , rFinish :: (Int, Int, Int)  -- ^ ... End: line, col, byte-offset).
  }
  deriving (Show) via PP Range

-- | TODO: Ugh. Purge it.
diffRange :: Range -> Range -> Range
diffRange (Range ws wf) (Range ps _) = Range (max ws ps) wf

instance Pretty Range where
  pp (Range (ll, lc, _) (rl, rc, _)) =
    brackets do
      int ll <> ":" <> int lc <> "-" <> int rl <> ":" <> int rc

class HasRange a where
  location :: Lens' a Range