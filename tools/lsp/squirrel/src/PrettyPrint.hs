
module PrettyPrint
  ( module PrettyPrint
  , module Text.PrettyPrint
  )
  where

import Text.PrettyPrint hiding ((<>))

class Pretty p where
  pp :: p -> Doc