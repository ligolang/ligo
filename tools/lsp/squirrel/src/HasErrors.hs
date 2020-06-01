module HasErrors
  ( HasErrors(..)
  )
  where

import Error

-- | Ability to contain `Error`s.
class HasErrors h where
  errors :: h -> [Error]
