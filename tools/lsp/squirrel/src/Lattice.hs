
{- | The property the @Tree@ @info@ should abide.
-}

module Lattice
  ( Lattice(..)
  )
  where

-- | A range should have this property to be used for navigation.
class Lattice l where
  (?>) :: l -> l -> Bool
  (<?) :: l -> l -> Bool

  (?>) = flip (<?)
  (<?) = flip (?>)

  {-# minimal (?>) | (<?) #-}