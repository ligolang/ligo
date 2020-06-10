
module Product where

import qualified Data.Text as Text

import Pretty

data Product xs where
  Nil  :: Product '[]
  Cons :: { pHead :: x, pTail :: Product xs } -> Product (x : xs)

instance Pretty (Product xs) => Show (Product xs) where
  show = show . PP

class Contains x xs where
  getElem :: Product xs -> x
  putElem :: x -> Product xs -> Product xs

instance {-# OVERLAPS #-} Contains x (x : xs) where
  getElem   (Cons x _)  = x
  putElem x (Cons _ xs) = Cons x xs

instance Contains x xs => Contains x (y : xs) where
  getElem   (Cons _ xs) = getElem xs
  putElem x (Cons y xs) = Cons y (putElem x xs)

modifyElem :: Contains x xs => (x -> x) -> Product xs -> Product xs
modifyElem f xs = putElem (f $ getElem xs) xs

instance Pretty (Product '[]) where
  pp _ = "{}"

instance (Pretty x, Pretty (Product xs)) => Pretty (Product (x : xs)) where
  pp (Cons x xs) =
    if Text.null $ Text.strip ppx
    then pp xs
    else pp ppx <+> "&" <+> pp xs
    where
      ppx = ppToText x