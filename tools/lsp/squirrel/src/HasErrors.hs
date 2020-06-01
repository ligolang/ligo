module HasErrors where

import Error

class HasErrors h where
  errors :: h -> [Error]
