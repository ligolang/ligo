module Progress
  ( Progress (..)
  , ProgressCallback
  , noProgress
  , (%)
  ) where

import Data.Ratio qualified ((%))
import Data.Text (Text)
import Data.Word (Word32)

-- | Represents how much of some task was done, and what was done last. Should
-- be normalized between 0 and 100.
data Progress = Progress
  { pTotal :: Word32
  , pMessage :: Text
  }

type ProgressCallback m = Progress -> m ()

noProgress :: Applicative m => ProgressCallback m
noProgress _ = pure ()

-- | Calculate the percentage for some fraction, normalized between 0 and 100.
(%) :: (Integral a, Integral b) => a -> a -> b
n % t = round $ n Data.Ratio.% t * 100
