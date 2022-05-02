module Test.Common.Util
  ( ScopeTester
  , testDir
  , contractsDir
  , getContractsWithExtension
  , readContract
  , readContractWithMessages
  , readContractWithScopes
  , parseContractsWithDependencies
  , parseContractsWithDependenciesScopes
  , supportedExtensions
  ) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Language.Haskell.TH.Syntax (liftString)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)

import AST.Includes (Includes, includesGraph, insertPreprocessorRanges)
import AST.Parser (parseContracts, parsePreprocessed, parseWithScopes)
import AST.Scope.Common
  ( ContractInfo, ContractInfo', HasScopeForest, Info', ParsedContractInfo
  , addScopes, contractTree
  )
import AST.Skeleton (SomeLIGO)
import Extension (supportedExtensions)
import Log (NoLoggingT (..))
import Parser (ParsedInfo)
import ParseTree (pathToSrc)
import Progress (noProgress)

type ScopeTester impl = HasScopeForest impl (NoLoggingT IO)

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
readContractWithMessages filepath = do
  src <- pathToSrc filepath
  runNoLoggingT $ parsePreprocessed src

readContractWithScopes
  :: forall parser. ScopeTester parser
  => FilePath -> IO (SomeLIGO Info')
readContractWithScopes filepath =
  contractTree <$> parseWithScopes @parser filepath

parseContractsWithDependencies
  :: FilePath
  -> IO (Includes ParsedContractInfo)
parseContractsWithDependencies top =
  includesGraph =<<
    parseContracts (runNoLoggingT . parsePreprocessed) noProgress (const True) top

parseContractsWithDependenciesScopes
  :: forall impl
   . ScopeTester impl
  => FilePath
  -> IO (Includes ContractInfo')
parseContractsWithDependenciesScopes =
  runNoLoggingT
  . addScopes @impl noProgress <=< parseContractsWithDependencies
