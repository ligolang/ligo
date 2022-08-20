module AST.Scope.Standard
  ( Standard
  ) where

import Data.Foldable (toList)
import UnliftIO.Exception (Handler (..), catches, displayException)

import AST.Scope.Common
  ( pattern FindContract, FindFilepath (..), HasScopeForest (..), MergeStrategy (..)
  , ParsedContract (..), ScopeForest, mergeScopeForest
  )
import AST.Scope.Fallback (Fallback)
import AST.Scope.FromCompiler (FromCompiler)
import Cli.Impl
import Cli.Json (fromLigoErrorToMsg)
import Cli.Types (HasLigoClient)
import Diagnostic (Message)
import Log (Log, i)
import Log qualified

data Standard

data FromCompilerStatus
  = Scopes (FindFilepath ScopeForest)
  | LigoErrors [Message]
  | Failure

instance (HasLigoClient m, Log m) => HasScopeForest Standard m where
  scopeContract tempSettings contract = do
    compilerStatus <- (Scopes <$> scopeContract @FromCompiler tempSettings contract) `catches`
      [ Handler \(LigoDecodedExpectedClientFailureException errs warns _) -> do
          -- catch only errors that we expect from ligo and try to use fallback parser
          pure $ LigoErrors $ fromLigoErrorToMsg <$> toList errs <> warns
      , Handler \(_ :: SomeLigoException) ->
          pure Failure
      , Handler \(e :: IOError) -> do
        -- Likely LIGO isn't installed or was not found.
        $(Log.err) [i|Couldn't call LIGO, failed with #{displayException e}|]
        pure Failure
      ]
    FindFilepath fallback <- scopeContract @Fallback tempSettings contract
    let src = _cFile $ _getContract contract
    pure case compilerStatus of
      Scopes (FindFilepath compiler) -> FindContract src
        (mergeScopeForest OnUnion (_cTree fallback) (_cTree compiler))
        (_cMsgs fallback <> _cMsgs compiler)
      LigoErrors msgs -> FindContract src
        (_cTree fallback)
        (msgs <> _cMsgs fallback)
      Failure -> FindFilepath fallback
