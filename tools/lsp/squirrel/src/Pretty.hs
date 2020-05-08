{-
  Pretty printer, based on GHC one.
-}

module Pretty
  ( module Pretty
  , module Text.PrettyPrint
  )
  where

import qualified Data.Text as Text
import Data.Text (Text)

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
  pp = text . Text.unpack

tuple :: Pretty p => [p] -> Doc
tuple = parens . train ","

list :: Pretty p => [p] -> Doc
list = brackets . train ";"

infixr 2 `indent`
indent a b = hang a 2 b

infixr 1 `above`
above a b = hang a 0 b

train sep = fsep . punctuate sep . map pp

block :: Pretty p => [p] -> Doc
block = vcat . map pp

sepByDot :: Pretty p => [p] -> Doc
sepByDot = cat . map (("." <>) . pp)

mb :: Pretty a => (Doc -> Doc) -> Maybe a -> Doc
mb f = maybe empty (f . pp)

sparseBlock :: Pretty a => [a] -> Doc
sparseBlock = vcat . punctuate "\n" . map (($$ empty) . pp)