
module Stubbed where

import Control.Lens

import Data.Text (Text, pack)

import Error

-- | For types that have a default replacer with an `Error`.
class Stubbed a where
  stubbing :: Error -> a

instance Stubbed Text where
  stubbing = pack . show

-- | This is bad, but I had to.
--
--   TODO: Find a way to remove this instance.
--         I probably need a wrapper around '[]'.
--
instance Stubbed [a] where
  stubbing = const []

-- | `Nothing` would be bad default replacer.
instance Stubbed a => Stubbed (Maybe a) where
  stubbing = Just . stubbing

