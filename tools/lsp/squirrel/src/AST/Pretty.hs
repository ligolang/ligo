
module AST.Pretty () where

import AST.Types
import PrettyPrint
import Parser

-- instance Pretty (Contract i) where
--   pp (Contract _ decls) =
--     hang "(* contract *)" 2 do
--       vcat $ map (($$ empty) . pp) decls

--   pp (WrongContract err) =
--     pp err

-- instance Pretty Error where
--   pp

-- instance Pretty (Declaration i) where
--   pp (

-- wrap [l, r] a = hang (hang l 2 r) 0 r