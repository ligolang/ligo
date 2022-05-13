module Test.Common.Util.Parsers
  ( checkFile
  ) where

import AST.Scope (pattern FindContract, HasScopeForest, addShallowScopes)
import Parser (collectTreeErrors)
import Progress (noProgress)

import Test.Common.FixedExpectations (Expectation, HasCallStack, expectationFailure)
import Test.Common.Util (readContractWithMessages)

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
    _ : _ -> expectationFailure $
      "Parsing failed, but it shouldn't have. " <>
      "Messages: " <> show msgs' <> "."
    [] -> do
      FindContract _file tree' msgs'' <- addShallowScopes @parser noProgress c
      let msgs''' = collectTreeErrors tree' <> msgs''
      case msgs''' of
        _ : _ -> expectationFailure $
          "Scoping failed, but it shouldn't have. " <>
          "Messages: " <> show msgs''' <> "."
        [] -> pure ()
checkFile False path = do
  c@(FindContract _file tree msgs) <- readContractWithMessages path
  let msgs' = collectTreeErrors tree <> msgs
  case msgs' of
    [] -> expectationFailure "Parsing succeeded, but it shouldn't have."
    _ : _ -> do
      FindContract _file tree' msgs'' <- addShallowScopes @parser noProgress c
      let msgs''' = collectTreeErrors tree' <> msgs''
      case msgs''' of
        [] -> expectationFailure "Scoping succeeded, but it shouldn't have."
        _ : _ -> pure ()
