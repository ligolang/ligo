
module RIO
  ( RIO
  , run
  , load
  , freshID
  , respond
  , collectErrors
  , forceFetch
  , fetch
  , respondWith
  , log
  , liftLsp
  ) where

{- TODO: break this module into file loading, diagnostics, haskell-lsp wrappers
         and other parts when it grows too big.
-}

import           Prelude hiding (log)

import           Control.Arrow
import           Control.Exception.Safe                        (MonadCatch, MonadThrow)
import           Control.Monad
import           Control.Monad.Reader                          (ReaderT, liftIO, asks, MonadIO, MonadReader, runReaderT)

import qualified Data.Map                              as Map
import qualified Data.Text                             as Text
import           Data.Text                                     (Text)
import qualified Data.SortedList                       as List
import           Data.String.Interpolate                       (i)

import           Language.LSP.Diagnostics
import           Language.LSP.Messages         as Msg
import           Language.LSP.VFS
import qualified Language.LSP.Core             as Core
import qualified Language.LSP.Types            as J
import qualified Language.LSP.Utility          as U

-- import           System.Directory (getDirectoryContents, doesDirectoryExist)
-- import           System.FilePath

import           Duplo.Error
import           Duplo.Tree   (collect)

import           AST
import           ASTMap (ASTMap)
import qualified ASTMap
import           Cli
import qualified Config
import qualified Log
import           Product
import           Range

send :: Core.LspFuncs Config.Config -> FromServerMessage -> IO ()
send = Core.sendFunc

nextID :: Core.LspFuncs Config.Config -> IO J.LspId
nextID = Core.getNextReqId

type RioEnv =
  Product
    '[ ASTMap J.NormalizedUri (LIGO Info', [Msg]) RIO
     , Core.LspFuncs Config.Config
     , LigoClientEnv
     ]

newtype RIO a = RIO
  { _unRio :: ReaderT RioEnv IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader RioEnv
    , MonadThrow
    , MonadCatch
    , HasLigoClient
    )

run :: RioEnv -> RIO a -> IO a
run env (RIO action) = runReaderT action env

liftLsp :: (Core.LspFuncs Config.Config -> IO a) -> RIO a
liftLsp f = do
  liftIO . f =<< asks getElem

fetch, forceFetch :: J.NormalizedUri -> RIO (LIGO Info', [Msg])
fetch uri = asks getElem >>= ASTMap.fetchCurrent uri
forceFetch uri = do
  tmap <- asks getElem
  ASTMap.invalidate uri tmap
  ASTMap.fetchCurrent uri tmap

respond :: Msg.FromServerMessage -> RIO ()
respond msg = do
  funs <- asks getElem
  liftIO $ send funs msg

diagnostic :: Int -> J.NormalizedUri -> J.TextDocumentVersion -> [J.Diagnostic] -> RIO ()
diagnostic n nuri ver diags = do
  funs <- asks $ getElem @(Core.LspFuncs Config.Config)
  let diags' = partitionBySource diags <> emptyDiags
  Log.debug "LOOP.DIAG" [i|Diags #{diags'}|]
  liftIO $ Core.publishDiagnosticsFunc funs n nuri ver diags'

emptyDiags :: DiagnosticsBySource
emptyDiags = Map.fromList [(Just "ligo-lsp", List.toSortedList [])]

freshID :: RIO J.LspId
freshID = do
  funs <- asks getElem
  liftIO do
    nextID funs

log :: String -> RIO ()
log = liftIO . U.logs

respondWith
  :: J.RequestMessage J.ClientMethod req rsp
  -> (J.ResponseMessage rsp -> FromServerMessage)
  -> rsp
  -> RIO ()
respondWith req wrap rsp = respond $ wrap $ Core.makeResponseMessage req rsp

preload
  :: J.Uri
  -> RIO Source
preload uri = do
  let Just fin = J.uriToFilePath uri
  mvf <- liftLsp \funs ->
    Core.getVirtualFileFunc funs (J.toNormalizedUri uri)
  return case mvf of
    Just vf -> Text fin (virtualFileText vf)
    Nothing -> Path fin

load
  :: J.Uri
  -> RIO (LIGO Info', [Msg])
load uri = do
  src <- preload uri
  ligoEnv <- asks $ getElem @LigoClientEnv
  Log.debug "LOAD" [i|running with env #{ligoEnv}|]
  parseWithScopes' src

collectErrors
  :: (J.NormalizedUri -> RIO (LIGO Info', [Msg]))
  -> J.NormalizedUri
  -> Maybe Int
  -> RIO ()
collectErrors fetcher uri version = do
  (tree, errs) <- fetcher uri
  diagnostic 100 uri version
    $ map errorToDiag
    $ errs <> collectTreeErrors tree

errorToDiag :: (Range, Err Text a) -> J.Diagnostic
errorToDiag (getRange -> (Range (sl, sc, _) (el, ec, _) _), Err what) =
  J.Diagnostic
    (J.Range begin end)
    (Just J.DsError)
    Nothing
    (Just "ligo-lsp")
    (Text.pack [i|Parse error: Unexpected #{what}|])
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

collectTreeErrors :: LIGO Info' -> [Msg]
collectTreeErrors = map (getElem *** void) . collect

-- TODO: uncomment when it will be used
-- data ParsedContract = ParsedContract
--   { cPath :: FilePath
--   , cTree :: LIGO Info'
--   , cErr  :: [Msg]
--   }
