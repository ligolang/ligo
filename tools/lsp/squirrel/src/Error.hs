
module Error where

import Data.Text (Text)

import Pretty
import Range

-- | Parse error.
data Error
  = Expected
    { eMsg   :: Text   -- ^ Description of what was expected.
    , eWhole :: Text   -- ^ Offending text.
    , eRange :: Range  -- ^ Location of the error.
    }
  deriving (Show) via PP Error

instance Pretty Error where
  pp (Expected msg found r) = "░" <> pp msg <> pp r <> "▒" <> pp found <> "▓"

