
module PrettyPrint
  ( module PrettyPrint
  , module Text.PrettyPrint
  )
  where

import Data.Text

import Text.PrettyPrint hiding ((<>))

class Pretty p where
  pp :: p -> Doc

instance Pretty Text where
  pp = text . unpack