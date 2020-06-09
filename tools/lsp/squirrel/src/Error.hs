
{- | Parsing errors and utilities.
-}

module Error
  ( Error(..)
  , HasErrors (..)
  , Stubbed (..)
  )
  where

import Data.Text (Text, pack)

import Pretty

-- | Parse error.
data Error info
  = Expected
    { eMsg   :: Text   -- ^ Description of what was expected.
    , eWhole :: Text   -- ^ Offending text.
    , eInfo  :: info   -- ^ Location of the error.
    }
  deriving (Show) via PP (Error info)
  deriving stock (Eq, Functor, Foldable, Traversable)

instance Pretty1 Error where
  pp1 (Expected msg found r) = "░" <> pp msg <> r <> "▒" <> pp found <> "▓"

-- | Ability to contain `Error`s.
class HasErrors h info | h -> info where
  errors :: h -> [Error info]

-- | For types that have a default replacer with an `Error`.
class Stubbed a i where
  stub :: Error i -> a

instance Pretty i => Stubbed Text i where
  stub = pack . show

-- | This is bad, but I had to.
--
--   TODO: Find a way to remove this instance.
--         I probably need a wrapper around '[]'.
--
--         Or I need a @fields@ parser combinator.
--
instance Stubbed [a] i where
  stub = const []

-- | Is `Just` `.` @stubbing@.
instance Stubbed a i => Stubbed (Maybe a) i where
  stub = Just . stub

