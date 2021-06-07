{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}

module RIO
  ( RIO
  , RioEnv
  , run

  , preload
  , load
  , collectErrors
  , forceFetch
  , fetch
  ) where

{- TODO: break this module into file loading, diagnostics, haskell-lsp wrappers
         and other parts when it grows too big.
-}

import Prelude hiding (log)

import Control.Arrow
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, runReaderT)

import Data.Function (on)
import Data.List (nubBy)
import qualified Data.Map as Map
import qualified Data.SortedList as List
import Data.String.Interpolate (i)

import qualified Language.LSP.Diagnostics as D
import qualified Language.LSP.Server as J
import qualified Language.LSP.Types as J
import qualified Language.LSP.VFS as V

import Duplo.Tree (collect)

import AST
import ASTMap (ASTMap)
import qualified ASTMap
import Cli
import Config (Config (..))
import Duplo.Lattice (Lattice (leq))
import qualified Log
import Product
import Range

type RioEnv =
  Product
    '[ ASTMap J.NormalizedUri (SomeLIGO Info', [Msg]) RIO
     ]

newtype RIO a = RIO
  { _unRio :: ReaderT RioEnv (J.LspM Config) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader RioEnv
    , MonadThrow
    , MonadCatch
    , MonadUnliftIO
    , J.MonadLsp Config.Config
    )

-- FIXME: LspT does not provide these instances, even though it is just a ReaderT.
-- (Do not forget to remove -Wno-orphans.)
deriving newtype instance MonadThrow m => MonadThrow (J.LspT config m)
deriving newtype instance MonadCatch m => MonadCatch (J.LspT config m)

instance HasLigoClient RIO where
  getLigoClientEnv = fmap (LigoClientEnv . _cLigoBinaryPath) J.getConfig


run :: (J.LanguageContextEnv Config.Config, RioEnv) -> RIO a -> IO a
run (lcEnv, env) (RIO action) = J.runLspT lcEnv $ runReaderT action env

fetch, forceFetch :: J.NormalizedUri -> RIO (SomeLIGO Info', [Msg])
fetch uri = asks getElem >>= ASTMap.fetchCurrent uri
forceFetch uri = do
  tmap <- asks getElem
  ASTMap.invalidate uri tmap
  ASTMap.fetchCurrent uri tmap

diagnostic :: Int -> J.NormalizedUri -> J.TextDocumentVersion -> [J.Diagnostic] -> RIO ()
diagnostic n nuri ver diags = do
  let diags' = D.partitionBySource diags <> emptyDiags
  Log.debug "LOOP.DIAG" [i|Diags #{diags'}|]
  J.publishDiagnostics n nuri ver diags'

emptyDiags :: D.DiagnosticsBySource
emptyDiags = Map.fromList [(Just "ligo-lsp", List.toSortedList [])]

preload
  :: J.Uri
  -> RIO Source
preload uri = do
  let Just fin = J.uriToFilePath uri  -- FIXME: non-exhaustive
  mvf <- J.getVirtualFile (J.toNormalizedUri uri)
  return case mvf of
    Just vf -> Text fin (V.virtualFileText vf)
    Nothing -> Path fin

load
  :: J.Uri
  -> RIO (SomeLIGO Info', [Msg])
load uri = do
  src <- preload uri
  ligoEnv <- getLigoClientEnv
  Log.debug "LOAD" [i|running with env #{ligoEnv}|]
  parseWithScopes @Standard src

collectErrors
  :: (J.NormalizedUri -> RIO (SomeLIGO Info', [Msg]))
  -> J.NormalizedUri
  -> Maybe Int
  -> RIO ()
collectErrors fetcher uri version = do
  (tree, errs) <- fetcher uri
  diagnostic 100 uri version
    $ map errorToDiag
    $ nubBy (leq `on` fst)
    $ errs <> collectTreeErrors tree

errorToDiag :: (Range, Error a) -> J.Diagnostic
errorToDiag (getRange -> (Range (sl, sc, _) (el, ec, _) _), Error what _) =
  J.Diagnostic
    (J.Range begin end)
    (Just J.DsError)
    Nothing
    (Just "ligo-lsp")
    what
    (Just $ J.List[])
    Nothing
  where
    begin = J.Position (sl - 1) (sc - 1)
    end   = J.Position (el - 1) (ec - 1)

-- Parse whole directory for ligo contracts and collect the results.
-- This ignores every other file which is not a contract.
-- TODO: convert FilePath to URI
-- parseContracts :: FilePath -> RIO [ParsedContract]
-- parseContracts top = let
--   exclude p = p /= "." && p /= ".." in do
--   ds <- liftIO $ getDirectoryContents top
--   contracts <- forM (filter exclude ds) $ \d -> do
--     let p = top </> d
--     yes <- liftIO $ doesDirectoryExist p
--     if yes
--       then parseContracts p
--       else do
--         contract <- RIO.forceFetch (J.toNormalizedUri p)
--         case contract of
--           Right (tree, errs) ->
--             return $ [ParsedContract p tree $ errs <> collectTreeErrors tree]
--           Left _ -> return []
--   return (concat contracts)

collectTreeErrors :: SomeLIGO Info' -> [Msg]
collectTreeErrors = map (getElem *** void) . collect . (^. nestedLIGO)

-- TODO: uncomment when it will be used
-- data ParsedContract = ParsedContract
--   { cPath :: FilePath
--   , cTree :: LIGO Info'
--   , cErr  :: [Msg]
--   }
