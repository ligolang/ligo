
module Stubbed
  ( Stubbed (..)
  )
  where

import Control.Lens

import Data.Text (Text, pack)

import Error

-- | For types that have a default replacer with an `Error`.
class Stubbed a where
  stub :: Error -> a

instance Stubbed Text where
  stub = pack . show

-- | This is bad, but I had to.
--
--   TODO: Find a way to remove this instance.
--         I probably need a wrapper around '[]'.
--
--         Or I need a @fields@ parser combinator.
--
instance Stubbed [a] where
  stub = const []

-- | Is `Just` `.` @stubbing@.
instance Stubbed a => Stubbed (Maybe a) where
  stub = Just . stub

