{-# LANGUAGE PolyKinds #-}
module Test.Common.Util
  ( contractsDir
  , getContractsWithExtension
  , getResponseResult
  , openLigoDoc
  , readContract
  , readContractWithMessages
  , readContractWithScopes
  , runHandlersTest
  , supportedExtensions
  ) where

import Control.Arrow ((&&&))
import Control.Exception.Safe (catch, throwIO)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Language.Haskell.TH.Syntax (liftString)
import Language.LSP.Test (Session, fullCaps, openDoc, runSession)
import Language.LSP.Types (ResponseMessage, ResponseResult, TextDocumentIdentifier)
import Language.LSP.Types.Lens qualified as LSP (result)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

import AST (parse)
import AST.Parser (Source (Path), parseWithScopes)
import AST.Scope.Common (HasScopeForest, Info', contractTree, _cMsgs, _cTree, _getContract)
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

getResponseResult :: ResponseMessage m -> ResponseResult m
getResponseResult rsp =
  case rsp ^. LSP.result of
    Right x -> x
    Left _ -> error "Should be able to parse ResponseMessage"

openLigoDoc :: FilePath -> Session TextDocumentIdentifier
openLigoDoc fp = openDoc fp "ligo"

readContract :: FilePath -> IO (SomeLIGO Info)
readContract filepath = contractTree <$> parse (Path filepath)

readContractWithMessages :: FilePath -> IO (SomeLIGO Info, [Msg])
readContractWithMessages filepath = (_cTree &&& _cMsgs) . _getContract <$> parse (Path filepath)

readContractWithScopes
  :: forall parser. HasScopeForest parser IO
  => FilePath -> IO (SomeLIGO Info')
readContractWithScopes filepath
  = contractTree <$> parseWithScopes @parser (Path filepath)

runHandlersTest :: FilePath -> Session a -> IO a
runHandlersTest = runSession "ligo-squirrel" fullCaps

supportedExtensions :: [FilePath]
supportedExtensions = [".ligo", ".mligo", ".religo"]
