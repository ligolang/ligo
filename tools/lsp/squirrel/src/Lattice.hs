
module Lattice where

class Lattice l where
  (?>) :: l -> l -> Bool
  (<?) :: l -> l -> Bool

  (?>) = flip (<?)
  (<?) = flip (?>)

  {-# minimal (?>) | (<?) #-}