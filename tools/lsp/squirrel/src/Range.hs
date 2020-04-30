
module Range where

import PrettyPrint

data Range = Range
  { rStart  :: (Int, Int, Int)
  , rFinish :: (Int, Int, Int)
  }
  deriving stock (Show)

diffRange :: Range -> Range -> Range
diffRange (Range ws wf) (Range ps _) = Range (max ws ps) wf

instance Pretty Range where
  pp (Range (ll, lc, _) (rl, rc, _)) =
    brackets do
      int ll <> ":" <> int lc <> "-" <> int rl <> ":" <> int rc