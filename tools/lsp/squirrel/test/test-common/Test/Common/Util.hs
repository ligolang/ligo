module Test.Common.Util
  ( contractsDir
  , getContractsWithExtension
  , readContract
  , readContractWithMessages
  , readContractWithScopes
  , supportedExtensions
  ) where

import Control.Arrow ((&&&))
import Control.Exception.Safe (catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Language.Haskell.TH.Syntax (liftString)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

import AST.Includes (insertPreprocessorRanges)
import AST.Parser (Source (Path), parsePreprocessed, parseWithScopes)
import AST.Scope.Common (HasScopeForest, Info', contractTree, _cMsgs, _cTree, _getContract)
import AST.Skeleton (SomeLIGO)

import Extension (supportedExtensions)
import Parser (ParsedInfo, Msg)

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
readContract filepath = do
  pp <- parsePreprocessed (Path filepath)
  ppRanges <- insertPreprocessorRanges pp
  pure (contractTree ppRanges)

readContractWithMessages :: FilePath -> IO (SomeLIGO ParsedInfo, [Msg])
readContractWithMessages filepath =
  (_cTree &&& _cMsgs) . _getContract <$> (insertPreprocessorRanges =<< parsePreprocessed (Path filepath))

readContractWithScopes
  :: forall parser. HasScopeForest parser IO
  => FilePath -> IO (SomeLIGO Info')
readContractWithScopes filepath
  = contractTree <$> parseWithScopes @parser (Path filepath)
