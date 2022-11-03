module Test.Common.Util.Parsers
  ( checkFile
  ) where

import System.FilePath (takeDirectory)

import AST.Scope
  (ContractInfo, HasScopeForest, addShallowScopes, contractFile, pattern FindContract)
import Cli.Types (TempDir (..), TempSettings (..))
import Diagnostic (Message)
import Parser (collectTreeErrors)
import Progress (noProgress)

import Test.Common.FixedExpectations (Expectation, HasCallStack, expectationFailure)
import Test.Common.Util (readContractWithMessages)

getScopedMsgs :: forall impl. HasScopeForest impl IO => ContractInfo -> IO [Message]
getScopedMsgs c = do
  let temp = TempSettings (takeDirectory $ contractFile c) $ GenerateDir ".temp"
  FindContract _file tree' msgs'' <- addShallowScopes @impl temp noProgress c
  pure $ collectTreeErrors tree' <> msgs''

checkFile
  :: forall parser
   . (HasCallStack, HasScopeForest parser IO)
  => Bool
  -> FilePath
  -> Expectation
checkFile True path = do
  c@(FindContract _file tree msgs) <- readContractWithMessages path
  let msgs' = collectTreeErrors tree <> msgs
  case msgs' of
    [] -> getScopedMsgs @parser c >>= \case
      [] -> pure ()
      msgs'' -> expectationFailure $
        "Scoping failed, but it shouldn't have. " <>
        "Messages: " <> show msgs'' <> "."
    _ -> expectationFailure $
      "Parsing failed, but it shouldn't have. " <>
      "Messages: " <> show msgs' <> "."
checkFile False path = do
  c@(FindContract _file tree msgs) <- readContractWithMessages path
  let msgs' = collectTreeErrors tree <> msgs
  case msgs' of
    [] -> expectationFailure "Parsing succeeded, but it shouldn't have."
    _ -> getScopedMsgs @parser c >>= \case
      [] -> expectationFailure "Scoping succeeded, but it shouldn't have."
      _ -> pure ()
