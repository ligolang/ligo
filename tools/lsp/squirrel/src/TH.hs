
module TH () where

import Control.Applicative

import Language.Haskell.TH.Syntax (Q)

instance Semigroup a => Semigroup (Q a) where (<>)   = liftA2 (<>)
instance Monoid    a => Monoid    (Q a) where mempty = pure mempty

