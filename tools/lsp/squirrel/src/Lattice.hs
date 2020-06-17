
{- | The property the @Tree@ @info@ should abide.
-}

module Lattice
  ( Lattice(..)
  , partOrder
  )
  where

-- | A range should have this property to be used for navigation.
class Lattice l where
  (?>) :: l -> l -> Bool
  (<?) :: l -> l -> Bool

  (?>) = flip (<?)
  (<?) = flip (?>)

  {-# minimal (?>) | (<?) #-}

partOrder :: Lattice l => l -> l -> Ordering
partOrder a b | a <? b && b <? a = EQ
partOrder a b | a <? b           = LT
partOrder a b |           b <? a = GT
partOrder a b                    = error "partOrder: Non-orderable"

