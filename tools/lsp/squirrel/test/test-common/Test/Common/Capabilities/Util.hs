module Test.Common.Capabilities.Util
  ( contractsDir
  ) where

import System.Directory (canonicalizePath)
import System.IO.Unsafe (unsafePerformIO)

contractsDir :: FilePath
contractsDir = unsafePerformIO $ canonicalizePath "./test/contracts"
{-# NOINLINE contractsDir #-}
