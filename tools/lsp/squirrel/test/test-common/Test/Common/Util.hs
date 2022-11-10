-- For some reason, GHC thinks the HasCallStack constraint is redundant.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Test.Common.Util
  ( ScopeTester
  , tempTemplate
  , testDir
  , contractsDir
  , getContractsWithExtension
  , readContract
  , readContractWithMessages
  , readContractWithScopes
  , parseContractsWithDependencies
  , parseContractsWithDependenciesScopes
  , parseDirectoryWithScopes
  , supportedExtensions
  ) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, isSuffixOf)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (liftString)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)

import AST.Includes (Includes (..), includesGraph, insertPreprocessorRanges)
import AST.Parser (parseContracts, parsePreprocessed, parseWithScopes)
import AST.Scope
  ( ContractInfo, ContractInfo', HasScopeForest, Info', ParsedContractInfo
  , addScopes, contractTree
  )
import AST.Skeleton (SomeLIGO)
import Cli.Types (TempDir (..), TempSettings (..))
import Extension (supportedExtensions)
import Log (NoLoggingT (..))
import Parser (ParsedInfo)
import ParseTree (pathToSrc)
import Progress (noProgress)

type ScopeTester impl = (HasCallStack, HasScopeForest impl (NoLoggingT IO))

tempTemplate :: String
tempTemplate = ".ligo-test"

testDir, contractsDir :: FilePath
testDir =
  $(
    let
      getDir :: IO FilePath
      getDir = getEnv "TEST_DIR" `catch` \e ->
        if isDoesNotExistError e
        then pure $ ".." </> ".." </> ".." </> "src" </> "test"
        else throwIO e
    in liftIO getDir >>= liftString
  )
contractsDir = testDir </> "contracts"

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
readContractWithMessages filepath = runNoLoggingT $ do
  src <- pathToSrc filepath
  let temp = TempSettings (takeDirectory filepath) $ GenerateDir tempTemplate
  parsePreprocessed temp src

readContractWithScopes
  :: forall parser. ScopeTester parser
  => FilePath -> IO (SomeLIGO Info')
readContractWithScopes filepath =
  contractTree <$> parseWithScopes @parser filepath

parseContractsWithDependencies
  :: TempSettings
  -> FilePath
  -> IO (Includes ParsedContractInfo)
parseContractsWithDependencies tempSettings top =
  let ignore = not . any (tempTemplate `isPrefixOf`) . splitDirectories in
  runNoLoggingT $ includesGraph
    =<< parseContracts (parsePreprocessed tempSettings) noProgress ignore top

parseContractsWithDependenciesScopes
  :: forall impl
   . ScopeTester impl
  => TempSettings
  -> FilePath
  -> IO (Includes ContractInfo')
parseContractsWithDependenciesScopes tempSettings =
  runNoLoggingT
  . addScopes @impl tempSettings noProgress
  <=< parseContractsWithDependencies tempSettings

parseDirectoryWithScopes
  :: forall impl. ScopeTester impl
  => FilePath -> IO (Includes ContractInfo')
parseDirectoryWithScopes dir = do
  let temp = TempSettings dir $ GenerateDir tempTemplate
  parseContractsWithDependenciesScopes @impl temp dir
