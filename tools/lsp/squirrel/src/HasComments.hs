
module HasComments where

import qualified Data.Text as Text

import Pretty

class HasComments c where
  getComments :: c -> [Text.Text]

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

instance HasComments () where
  getComments () = []