-- For some reason, GHC thinks the HasCallStack constraint is redundant.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Test.Common.Util
  ( tempTemplate
  , contractsDir
  , getContractsWithExtension
  , readContract
  , readContractWithMessages
  , supportedExtensions
  , renderNoLineLengthLimit
  ) where

import Data.List (isSuffixOf)
import Duplo.Pretty (Doc, Style (..), renderStyle, style)
import System.Directory (listDirectory)
import System.FilePath (takeDirectory, (</>))

import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Util.AST.Common
import Language.LIGO.Debugger.Util.AST.Includes (insertPreprocessorRanges)
import Language.LIGO.Debugger.Util.AST.Parser (parsePreprocessed)
import Language.LIGO.Debugger.Util.AST.Skeleton (SomeLIGO)
import Language.LIGO.Debugger.Util.Extension (supportedExtensions)
import Language.LIGO.Debugger.Util.ParseTree (pathToSrc)
import Language.LIGO.Debugger.Util.Parser (ParsedInfo)

tempTemplate :: String
tempTemplate = ".ligo-test"

contractsDir :: FilePath
contractsDir = "test" </> "contracts"
{-# NOINLINE contractsDir #-}

getContractsWithExtension :: String -> [FilePath] -> FilePath -> IO [FilePath]
getContractsWithExtension ext ignore dir = listDirectory dir
                                <&> filter (ext `isSuffixOf`)
                                <&> map (dir </>)
                                <&> filter (`notElem` ignore)

readContract :: FilePath -> IO (SomeLIGO ParsedInfo)
readContract filepath = do
  pp <- readContractWithMessages filepath
  ppRanges <- insertPreprocessorRanges pp
  pure (contractTree ppRanges)

readContractWithMessages :: FilePath -> IO ContractInfo
readContractWithMessages filepath = do
  src <- pathToSrc filepath
  let temp = TempSettings (takeDirectory filepath) $ GenerateDir tempTemplate
  parsePreprocessed temp src

renderNoLineLengthLimit :: Doc -> Text
renderNoLineLengthLimit = toText . renderStyle style{lineLength = maxBound}
