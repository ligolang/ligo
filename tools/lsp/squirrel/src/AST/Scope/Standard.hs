module AST.Scope.Standard
  ( Standard
  ) where

import Algebra.Graph.AdjacencyMap qualified as G
import Control.Exception.Safe
import Control.Lens ((%~))
import Control.Monad.IO.Unlift (MonadUnliftIO)

import AST.Scope.Common
  ( pattern FindContract, FindFilepath (..), ContractNotFoundException (..)
  , HasScopeForest (..), Includes (..), ParsedContract (..), MergeStrategy (..), cMsgs
  , getContract, lookupContract, mergeScopeForest
  )
import AST.Scope.Fallback (Fallback)
import AST.Scope.FromCompiler (FromCompiler)
import AST.Skeleton (Error (Error))

import Cli.Impl
import Cli.Json (fromLigoErrorToMsg)
import Cli.Types (HasLigoClient)

import Duplo.Lattice (Lattice (leq))
import Log (Log)
import Parser (Msg)
import ParseTree (srcPath)
import Range (point)
import Util.Graph (traverseAMConcurrently)

data Standard

instance (HasLigoClient m, Log m, MonadUnliftIO m) => HasScopeForest Standard m where
  scopeForest reportProgress pc = do
    lgForest <- scopeForest @FromCompiler reportProgress pc `catches`
      [ Handler \case
          -- catch only errors that we expect from ligo and try to use fallback parser
          LigoDecodedExpectedClientFailureException err _ -> addLigoErrToMsg $ fromLigoErrorToMsg err
      , Handler \case
          LigoUnexpectedCrashException err _ -> addLigoErrToMsg (point 1 1, Error err [])
      , Handler \case
          -- all other errors such as "Not found in $PATH" and other exceptions are ignored
          (_ :: SomeException) -> fallbackForest
      ]
    fbForest <- fallbackForest
    merge lgForest fbForest
    where
      fallbackForest = scopeForest @Fallback reportProgress pc

      addLigoErrToMsg err =
        Includes . G.gmap (getContract . cMsgs %~ (`rewriteAt` err)) . getIncludes <$> fallbackForest

      merge l f = Includes <$> flip traverseAMConcurrently (getIncludes l) \(FindFilepath lf) -> do
        let src = _cFile lf
        let fp = srcPath src
        FindFilepath ff <- maybe (throwM $ ContractNotFoundException fp f) pure (lookupContract fp f)
        pure $ FindContract
          src
          (mergeScopeForest OnUnion (_cTree ff) (_cTree lf))
          (_cMsgs ff <> _cMsgs lf)

      -- | Rewrite error message at the most local scope or append it to the end.
      rewriteAt :: [Msg] -> Msg -> [Msg]
      rewriteAt at what@(from, _) = filter (not . (from `leq`) . fst) at <> [what]
