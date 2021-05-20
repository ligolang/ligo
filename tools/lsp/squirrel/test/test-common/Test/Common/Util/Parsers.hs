module Test.Common.Util.Parsers
  ( checkFile
  ) where

import Control.Exception.Safe (try)
import Duplo (HandlerFailed (..))

import Test.Common.FixedExpectations (Expectation, HasCallStack, expectationFailure)
import Test.Common.Util (readContractWithMessages)

checkFile :: HasCallStack => Bool -> FilePath -> Expectation
checkFile True path = do
  res <- try (readContractWithMessages path)
  case res of
    Left (err :: HandlerFailed) -> expectationFailure $
      "Parsing failed, but it shouldn't have. " <>
      "Error: " <> show err <> "."
    Right (_tree, msgs) -> case msgs of
      (_ : _) -> expectationFailure $
        "Parsing failed, but it shouldn't have. " <>
        "Messages: " <> show msgs <> "."
      [] -> pure ()
checkFile False path = do
  res <- try @_ @HandlerFailed (readContractWithMessages path)
  case res of
    Right (_tree, []) -> expectationFailure
      "Parsing succeeded, but it shouldn't have."
    _ -> pure ()
