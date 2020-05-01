
module Pretty
  ( module Pretty
  , module Text.PrettyPrint
  )
  where

import Data.Text

import Text.PrettyPrint hiding ((<>))

newtype PP a = PP { unPP :: a }

instance Pretty a => Show (PP a) where
  show = show . pp . unPP

class Pretty p where
  pp :: p -> Doc

instance Pretty Text where
  pp = text . unpack

wrap [l, r] a = hang (hang l 2 r) 0 r