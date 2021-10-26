module Main
  ( main
  ) where

import Test.Common.Tasty (TestWithLigo (WithLigo), defaultMain)

-- n.b.: Also change ../flake.nix if changing the dependency on ligo.
main :: IO ()
main = defaultMain WithLigo
