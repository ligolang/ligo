
module AST.Completion where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (listToMaybe)
import Data.List (isSubsequenceOf, nub)

import Duplo.Tree
import Duplo.Lattice
import Duplo.Pretty

import AST.Types
import AST.Scope
import AST.Parser
import Range
import Product

import Debug.Trace


complete
  :: ( Eq (Product xs)
     , Modifies (Product xs)
     , Contains Range xs
     , Contains [ScopedDecl] xs
     , Contains (Maybe Category) xs
     )
  => Range
  -> LIGO (Product xs)
  -> Maybe [Text]
complete r tree = do
  let l = spineTo (leq r . getElem) tree
  word <- listToMaybe l
  let scope = getElem (extract word)
  let cat   = getElem (extract word)
  return
    $ filter (isSubseqOf (ppToText word))
    $ nub
    $ map (ppToText . _sdName)
    $ filter (fits cat . catFromType)
    $ scope

isSubseqOf :: Text -> Text -> Bool
isSubseqOf l r = isSubsequenceOf (Text.unpack l) (Text.unpack r)

fits :: Maybe Category -> Category -> Bool
fits  Nothing _  = True
fits (Just c) c' = c == c'

catFromType :: ScopedDecl -> Category
catFromType = maybe Variable (either (const Variable) (const Type)) . _sdType