module AST.Scope.Standard
  ( Standard
  ) where

import Algebra.Graph.AdjacencyMap qualified as G
import Control.Lens ((%~))
import UnliftIO.Exception (Handler (..), catches, displayException, throwIO)

import AST.Scope.Common
  ( pattern FindContract, FindFilepath (..), ContractNotFoundException (..)
  , HasScopeForest (..), Includes (..), ParsedContract (..), MergeStrategy (..), cMsgs
  , getContract, lookupContract, mergeScopeForest
  )
import AST.Scope.Fallback (Fallback)
import AST.Scope.FromCompiler (FromCompiler)

import Cli.Impl
import Cli.Json (fromLigoErrorToMsg)
import Cli.Types (HasLigoClient)

import Duplo.Lattice (Lattice (leq))
import Log (Log, i)
import Log qualified
import Parser (Msg)
import ParseTree (srcPath)
import Util.Graph (traverseAMConcurrently)

data Standard

instance (HasLigoClient m, Log m) => HasScopeForest Standard m where
  scopeForest reportProgress pc = do
    fbForest <- scopeForest @Fallback reportProgress pc
    lgForest <- scopeForest @FromCompiler reportProgress pc `catches`
      [ Handler \(LigoDecodedExpectedClientFailureException err _) ->
          -- catch only errors that we expect from ligo and try to use fallback parser
          pure $ addLigoErrToMsg fbForest $ fromLigoErrorToMsg err
      , Handler \(_ :: SomeLigoException) ->
          pure fbForest
      , Handler \(e :: IOError) -> do
        -- Likely LIGO isn't installed or was not found.
        $(Log.err) [i|Couldn't call LIGO, failed with #{displayException e}|]
        pure fbForest
      ]
    merge lgForest fbForest
    where
      addLigoErrToMsg (Includes forest) err =
        Includes $ G.gmap (getContract . cMsgs %~ (`rewriteAt` err)) forest

      merge l f = Includes <$> flip traverseAMConcurrently (getIncludes l) \(FindFilepath lf) -> do
        let src = _cFile lf
        let fp = srcPath src
        FindFilepath ff <- maybe (throwIO $ ContractNotFoundException fp f) pure (lookupContract fp f)
        pure $ FindContract
          src
          (mergeScopeForest OnUnion (_cTree ff) (_cTree lf))
          (_cMsgs ff <> _cMsgs lf)

      -- | Rewrite error message at the most local scope or append it to the end.
      rewriteAt :: [Msg] -> Msg -> [Msg]
      rewriteAt at what@(from, _) = filter (not . (from `leq`) . fst) at <> [what]
