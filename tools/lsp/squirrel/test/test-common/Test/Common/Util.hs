module Test.Common.Util
  ( contractsDir
  , getContractsWithExtension
  , readContract
  , readContractWithMessages
  , readContractWithScopes
  , supportedExtensions
  ) where

import Control.Exception.Safe (catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Language.Haskell.TH.Syntax (liftString)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

import AST (parse)
import AST.Parser (Source (Path), parseWithScopes)
import AST.Scope.Common (HasScopeForest, Info')
import AST.Skeleton (SomeLIGO)
import Parser (Info, Msg)

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

readContract :: FilePath -> IO (SomeLIGO Info)
readContract filepath = fst <$> parse (Path filepath)

readContractWithMessages :: FilePath -> IO (SomeLIGO Info, [Msg])
readContractWithMessages filepath = parse (Path filepath)

readContractWithScopes
  :: forall parser. HasScopeForest parser IO
  => FilePath -> IO (SomeLIGO Info')
readContractWithScopes filepath
  = fst <$> parseWithScopes @parser (Path filepath)

supportedExtensions :: [FilePath]
supportedExtensions = [".ligo", ".mligo", ".religo"]
