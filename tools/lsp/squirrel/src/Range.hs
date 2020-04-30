
module Range where

data Range = Range
  { rStart  :: (Int, Int)
  , rFinish :: (Int, Int)
  }
  deriving stock (Show)

diffRange :: Range -> Range -> Range
diffRange (Range ws wf) (Range ps _) = Range (max ws ps) wf

