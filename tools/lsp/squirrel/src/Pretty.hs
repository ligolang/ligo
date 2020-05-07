{-
  Pretty printer, based on GHC one.
-}

module Pretty
  ( module Pretty
  , module Text.PrettyPrint
  )
  where

import Data.Text

import Text.PrettyPrint hiding ((<>))

-- | With this, one can `data X = ...; derive Show via PP X`
newtype PP a = PP { unPP :: a }

instance Pretty a => Show (PP a) where
  show = show . pp . unPP

-- | Pretty-printable types.
class Pretty p where
  pp :: p -> Doc

-- | Common instance.
instance Pretty Text where
  pp = text . unpack

-- | TODO: tuple, not list; actually /use/ it.
wrap [l, r] a = hang (hang l 2 r) 0 r