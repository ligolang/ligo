module Test.Common.Tasty
  ( TestWithLigo (..)
  , defaultMain
  ) where

import System.Environment (setEnv)

import Driver (main)

data TestWithLigo = WithoutLigo | WithLigo

-- | A default main function that can be used by ligo-squirrel.
defaultMain :: TestWithLigo -> IO ()
defaultMain testWithLigo = do
  setEnv "LIGO_BINARY_PATH" case testWithLigo of
    WithoutLigo -> "/dev/null"
    WithLigo    -> "ligo"
  main
