module AST.Scope.Standard
  ( Standard
  ) where

import Algebra.Graph.AdjacencyMap qualified as G
import Data.Foldable (toList)
import UnliftIO.Exception (Handler (..), catches, displayException)

import AST.Scope.Common
  ( pattern FindContract, FindFilepath (..), HasScopeForest (..), Includes (..)
  , ParsedContract (..), MergeStrategy (..), addLigoErrsToMsg, contractNotFoundException
  , lookupContract, mergeScopeForest
  )
import AST.Scope.Fallback (Fallback)
import AST.Scope.FromCompiler (FromCompiler)

import Cli.Impl
import Cli.Json (fromLigoErrorToMsg)
import Cli.Types (HasLigoClient)

import Log (Log, i)
import Log qualified
import ParseTree (srcPath)
import Util.Graph (traverseAMConcurrently)

data Standard

instance (HasLigoClient m, Log m) => HasScopeForest Standard m where
  scopeForest reportProgress pc = do
    fbForest <- scopeForest @Fallback reportProgress pc
    tryMergeWithFromCompiler fbForest `catches`
      [ Handler \(LigoDecodedExpectedClientFailureException errs warns _) -> do
          -- catch only errors that we expect from ligo and try to use fallback parser
          let addErrs = addLigoErrsToMsg (fromLigoErrorToMsg <$> toList errs <> warns)
          pure $ Includes $ G.gmap addErrs $ getIncludes fbForest
      , Handler \(_ :: SomeLigoException) ->
          pure fbForest
      , Handler \(e :: IOError) -> do
        -- Likely LIGO isn't installed or was not found.
        $(Log.err) [i|Couldn't call LIGO, failed with #{displayException e}|]
        pure fbForest
      ]
    where
      tryMergeWithFromCompiler fbForest = do
        lgForest <- scopeForest @FromCompiler reportProgress pc
        merge lgForest fbForest

      merge l f = Includes <$> flip traverseAMConcurrently (getIncludes l) \(FindFilepath lf) -> do
        let src = _cFile lf
        let fp = srcPath src
        FindFilepath ff <- maybe (contractNotFoundException fp f) pure (lookupContract fp f)
        pure $ FindContract
          src
          (mergeScopeForest OnUnion (_cTree ff) (_cTree lf))
          (_cMsgs ff <> _cMsgs lf)
