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
import System.FilePath ((</>))

import Language.LIGO.AST.Common
import Language.LIGO.AST.Includes (insertPreprocessorRanges)
import Language.LIGO.AST.Parser (parsePreprocessed)
import Language.LIGO.AST.Skeleton (SomeLIGO)
import Language.LIGO.Extension (supportedExtensions)
import Language.LIGO.ParseTree (pathToSrc)
import Language.LIGO.Parser (ParsedInfo)

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
readContractWithMessages = pathToSrc >=> parsePreprocessed

renderNoLineLengthLimit :: Doc -> Text
renderNoLineLengthLimit = toText . renderStyle style{lineLength = maxBound}
