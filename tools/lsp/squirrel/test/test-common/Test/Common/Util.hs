module Test.Common.Util
  ( ScopeTester
  , contractsDir
  , getContractsWithExtension
  , readContract
  , readContractWithMessages
  , readContractWithScopes
  , supportedExtensions
  , withoutLogger
  ) where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Language.Haskell.TH.Syntax (liftString)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)

import AST.Includes (insertPreprocessorRanges)
import AST.Parser (Source (Path), parsePreprocessed, parseWithScopes)
import AST.Scope.Common (HasScopeForest, Info', contractTree, _cMsgs, _cTree, _getContract)
import AST.Skeleton (SomeLIGO)

import Extension (supportedExtensions)
import Log (NoLoggingT, withoutLogger)
import Parser (ParsedInfo, Msg)

type ScopeTester impl = HasScopeForest impl (NoLoggingT IO)

contractsDir :: FilePath
contractsDir =
  $(
    let
      getDir :: IO FilePath
      getDir = getEnv "CONTRACTS_DIR" `catch` \e ->
        if isDoesNotExistError e
        then pure "../../../src/test/contracts"
        else throwIO e
    in liftIO getDir >>= liftString
  )

getContractsWithExtension :: String -> [FilePath] -> FilePath -> IO [FilePath]
getContractsWithExtension ext ignore dir = listDirectory dir
                                <&> filter (ext `isSuffixOf`)
                                <&> map (dir </>)
                                <&> filter (`notElem` ignore)

readContract :: FilePath -> IO (SomeLIGO ParsedInfo)
readContract filepath = withoutLogger \runLogger -> do
  pp <- runLogger $ parsePreprocessed (Path filepath)
  ppRanges <- insertPreprocessorRanges pp
  pure (contractTree ppRanges)

readContractWithMessages :: FilePath -> IO (SomeLIGO ParsedInfo, [Msg])
readContractWithMessages filepath = withoutLogger \runLogger ->
  (_cTree &&& _cMsgs) . _getContract
    <$> (insertPreprocessorRanges =<< runLogger (parsePreprocessed $ Path filepath))

readContractWithScopes
  :: forall parser. ScopeTester parser
  => FilePath -> IO (SomeLIGO Info')
readContractWithScopes filepath = withoutLogger \runLogger ->
  contractTree <$> runLogger (parseWithScopes @parser $ Path filepath)
