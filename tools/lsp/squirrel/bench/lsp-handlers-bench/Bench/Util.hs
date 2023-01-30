module Bench.Util
 ( baseDAO
 , projectWithOneBigFile
 , benchLspSession
 , getDoc
 , Doc(..)
 , insertToDoc
 , insertToDocKeystrokes
 ) where

import Criterion
import Data.Default (def)
import Language.LSP.Test qualified as LSP
import Language.LSP.Types qualified as LSP
import System.Directory (canonicalizePath)
import System.IO.Unsafe (unsafePerformIO)

import AST.Scope (ScopingSystem (..))
import Config (Config (..))

import Test.Common.LSP (openLigoDoc, runHandlersTestWithConfig)

baseDAO :: FilePath
baseDAO = unsafePerformIO $ canonicalizePath "bench/submodules/baseDAO/src/"
{-# NOINLINE baseDAO #-}

projectWithOneBigFile :: FilePath
projectWithOneBigFile = unsafePerformIO $ canonicalizePath "bench/projects/one_big_file/"
{-# NOINLINE projectWithOneBigFile #-}

benchLspSession
  :: NFData a
  => String
  -> FilePath
  -> LSP.Session a
  -> ScopingSystem
  -> Benchmark
benchLspSession description project session scopingSystem =
  bench description $ nfIO $ runHandlersTestWithConfig config project session
  where
    config = def {_cScopingSystem = scopingSystem}

getDoc :: Doc -> LSP.Session LSP.TextDocumentIdentifier
getDoc (OpenedDoc tdi) = pure tdi
getDoc (FileDoc path) = openLigoDoc path

data Doc
  = FileDoc String
  -- ^ Document on disc to be opened (path relative to project)
  | OpenedDoc LSP.TextDocumentIdentifier
  -- ^ Document that was already opened in this session
  deriving stock Show

type instance PrettyShow Doc = ()

insertToDoc :: Doc -> (LSP.UInt, LSP.UInt) -> Text -> LSP.Session ()
insertToDoc doc (line, col) text = getDoc doc >>= \tdi ->
  LSP.changeDoc tdi [LSP.TextDocumentContentChangeEvent (Just $ LSP.Range pos pos) Nothing text]
    where pos = LSP.Position (line - 1) (col - 1)

-- | Like insertToDoc but simulates typing
insertToDocKeystrokes :: Doc -> (LSP.UInt, LSP.UInt) -> Text -> LSP.Session ()
insertToDocKeystrokes doc (line, col) text = getDoc doc >>= \tdi ->
    for_ (zip (toString text) [col..]) $
      \(char, activeCol) -> insertToDoc (OpenedDoc tdi) (line, activeCol) (one char)
