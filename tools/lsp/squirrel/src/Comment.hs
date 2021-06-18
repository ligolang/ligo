
{- | Comments and utilities.
-}

module Comment
  ( HasComments(..)
  , c
  )
  where

import Data.Text qualified as Text
import           Data.Text   (Text)

import Duplo.Pretty

import Product

-- | Ability to contain comments.
class HasComments c where
  getComments :: c -> [Text.Text]

-- | Wrap some @Doc@ with a comment.
c :: HasComments i => i -> Doc -> Doc
c i d =
  case getComments i of
    [] -> d
    cc -> block (map removeSlashN cc) $$ d
  where
    removeSlashN txt =
      if "\n" `Text.isSuffixOf` txt
      then Text.init txt
      else txt

-- | Narrator: /But there was none/.
instance HasComments () where
  getComments () = []

instance (Contains [Text] xs) => HasComments (Product xs) where
  getComments = getElem
