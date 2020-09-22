
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as E
import Control.Lens
import Control.Monad

import Data.Default
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Language.Haskell.LSP.Control as CTRL
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages as Msg
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J
import qualified Language.Haskell.LSP.Utility as U
import Language.Haskell.LSP.VFS

import System.Directory
import System.Exit
import System.FilePath
import qualified System.Log as L
import System.Posix.Files

import Duplo.Error
import Duplo.Pretty
import Duplo.Tree (collect)

import AST hiding (def)
import qualified AST.Capabilities as Ligo
import qualified Config
import Extension
import Parser
import Product
import Range

-- import           Error

main :: IO ()
main = do
  -- return ()
  -- for_ [1.. 100] \_ -> do
  --   print . length . show . pp =<< sample' "../../../src/test/recognises/loop.ligo"
  errCode <- mainLoop
  exit errCode

mainLoop :: IO Int
mainLoop = do
    chan <- atomically newTChan :: IO (TChan FromClientMessage)

    let
      callbacks = Core.InitializeCallbacks
        { Core.onInitialConfiguration = Config.getInitialConfig
        , Core.onConfigurationChange = Config.getConfigFromNotification
        , Core.onStartup = \lFuns -> do
            _ <- forkIO $ eventLoop lFuns chan
            return Nothing
        }

    Core.setupLogger Nothing [] L.EMERGENCY
    CTRL.run callbacks (lspHandlers chan) lspOptions Nothing
  `catches`
    [ Handler \(e :: SomeException) -> do
        print e
        return 1
    ]

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

lspOptions :: Core.Options
lspOptions = def
  { Core.textDocumentSync       = Just syncOptions
  , Core.executeCommandCommands = Just ["lsp-hello-command"]
  }

lspHandlers :: TChan FromClientMessage -> Core.Handlers
lspHandlers rin =
  def
    { Core.initializedHandler = Just $ passHandler rin NotInitialized
    , Core.definitionHandler = Just $ passHandler rin ReqDefinition
    , Core.referencesHandler = Just $ passHandler rin ReqFindReferences
    , Core.didOpenTextDocumentNotificationHandler = Just $ passHandler rin NotDidOpenTextDocument
    , Core.didSaveTextDocumentNotificationHandler = Just $ passHandler rin NotDidSaveTextDocument
    , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
    , Core.didCloseTextDocumentNotificationHandler = Just $ passHandler rin NotDidCloseTextDocument
    , Core.cancelNotificationHandler = Just $ passHandler rin NotCancelRequestFromClient
    , Core.responseHandler = Just $ responseHandlerCb rin
      -- , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
      -- , Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
    , Core.completionHandler = Just $ passHandler rin ReqCompletion
      -- , Core.completionResolveHandler                 = Just $ passHandler rin ReqCompletionItemResolve
    , Core.foldingRangeHandler = Just $ passHandler rin ReqFoldingRange
    , Core.hoverHandler = Just $ passHandler rin ReqHover
    , Core.documentSymbolHandler = Just $ passHandler rin ReqDocumentSymbols
    }

passHandler :: TChan FromClientMessage -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (c notification)

responseHandlerCb :: TChan FromClientMessage -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

send :: Core.LspFuncs Config.Config -> FromServerMessage -> IO ()
send = Core.sendFunc

nextID :: Core.LspFuncs Config.Config -> IO J.LspId
nextID = Core.getNextReqId

eventLoop :: Core.LspFuncs Config.Config -> TChan FromClientMessage -> IO ()
eventLoop funs chan = do
  forever do
    msg <- atomically (readTChan chan)

    U.logs [i|Client: ${msg}|]

    case msg of
      RspFromClient {} -> do
        return ()

      NotInitialized _notif -> do
        let
          registration = J.Registration
            "lsp-haskell-registered"
            J.WorkspaceExecuteCommand
            Nothing
          registrations = J.RegistrationParams $ J.List [registration]

        rid <- nextID funs
        send funs
          $ ReqRegisterCapability
          $ fmServerRegisterCapabilityRequest rid registrations

      NotDidOpenTextDocument notif -> do
        let
          doc = notif
            ^.J.params
             .J.textDocument
             .J.uri

          ver = notif
            ^.J.params
             .J.textDocument
             .J.version

        collectErrors funs
          (J.toNormalizedUri doc)
          (J.uriToFilePath doc)
          (Just ver)

      NotDidChangeTextDocument notif -> do
        let
          doc = notif
            ^.J.params
             .J.textDocument
             .J.uri

        collectErrors funs
          (J.toNormalizedUri doc)
          (J.uriToFilePath doc)
          (Just 0)

      ReqDefinition req -> do
        stopDyingAlready funs req do
          let uri = req^.J.params.J.textDocument.J.uri
          let pos = posToRange $ req^.J.params.J.position
          tree <- loadFromVFS funs uri
          case definitionOf pos tree of
            Just defPos -> do
              respondWith funs req RspDefinition $ J.MultiLoc [J.Location uri $ rangeToLoc defPos]
            Nothing -> do
              respondWith funs req RspDefinition $ J.MultiLoc []

      ReqFindReferences req -> do
        stopDyingAlready funs req do
          let uri = req^.J.params.J.textDocument.J.uri
          let pos = posToRange $ req^.J.params.J.position
          tree <- loadFromVFS funs uri
          case referencesOf pos tree of
            Just refs -> do
              let locations = J.Location uri . rangeToLoc <$> refs
              respondWith funs req RspFindReferences $ J.List locations
            Nothing -> do
              respondWith funs req RspFindReferences $ J.List []

      ReqCompletion req -> do
        stopDyingAlready funs req $ do
          U.logs $ "got completion request: " <> show req
          let uri = req ^. J.params . J.textDocument . J.uri
          let pos = posToRange $ req ^. J.params . J.position
          tree <- loadFromVFS funs uri
          let completions = fmap toCompletionItem . fromMaybe [] $ complete pos tree
          respondWith funs req RspCompletion . J.Completions . J.List $ completions

      -- Additional callback executed after completion was made, currently no-op
      ReqCompletionItemResolve req -> do
        stopDyingAlready funs req $ do
          U.logs $ "got completion resolve request: " <> show req
          respondWith funs req RspCompletionItemResolve (req ^. J.params)

      ReqFoldingRange req -> do
        stopDyingAlready funs req $ do
          let uri = req ^. J.params . J.textDocument . J.uri
          tree <- loadFromVFS funs uri
          let response =
                RspFoldingRange
                  . Core.makeResponseMessage req
                  . J.List
              handler =
                Core.sendFunc funs
                  . response
                  . fmap toFoldingRange
          actions <- foldingAST tree
          handler actions

      ReqHover req -> do
        stopDyingAlready funs req do
          let uri = req ^. J.params . J.textDocument . J.uri
          let pos = posToRange $ req ^. J.params . J.position
          tree <- loadFromVFS funs uri
          let response =
                RspHover $ Core.makeResponseMessage req (hoverDecl pos tree)
          Core.sendFunc funs response

      ReqDocumentSymbols req -> do
        let uri = req ^. J.params . J.textDocument . J.uri
        tree <- loadFromVFS funs uri
        result <- extractDocumentSymbols uri tree
        respondWith funs req RspDocumentSymbols (J.DSSymbolInformation $ J.List result)

      _ -> U.logs "unknown msg"

respondWith
  :: Core.LspFuncs Config.Config
  -> J.RequestMessage J.ClientMethod req rsp
  -> (J.ResponseMessage rsp -> FromServerMessage)
  -> rsp
  -> IO ()
respondWith funs req wrap rsp = Core.sendFunc funs $ wrap $ Core.makeResponseMessage req rsp

stopDyingAlready :: Core.LspFuncs Config.Config -> J.RequestMessage m a b -> IO () -> IO ()
stopDyingAlready funs req = flip catch \(e :: SomeException) -> do
  Core.sendErrorResponseS (Core.sendFunc funs) (req^.J.id.to J.responseId) J.InternalError
    $ fromString
    $ "this happened: " ++ show e

posToRange :: J.Position -> Range
posToRange (J.Position l c) = Range (l + 1, c + 1, 0) (l + 1, c + 1, 0) ""

rangeToLoc :: Range -> J.Range
rangeToLoc (Range (a, b, _) (c, d, _) _) =
  J.Range
    (J.Position (a - 1) (b - 1))
    (J.Position (c - 1) (d - 1))

loadFromVFS
  :: Core.LspFuncs Config.Config
  -> J.Uri
  -> IO (LIGO Info')
loadFromVFS funs uri = do
  Core.getVirtualFileFunc funs
    (J.toNormalizedUri uri)
    >>= \case
      Just vf -> do
        let txt = virtualFileText vf
        let Just fin = J.uriToFilePath uri
        (tree, _) <- parse (Text fin txt)
        return $ addLocalScopes tree
      Nothing -> do
        loadByURI uri

loadByURI
  :: J.Uri
  -> IO (LIGO Info')
loadByURI uri = do
  case J.uriToFilePath uri of
    Just fin -> do
      (tree, _) <- parse (Path fin)
      return $ addLocalScopes tree
    Nothing -> do
      error $ "uriToFilePath " ++ show uri ++ " has failed. We all are doomed."

collectErrors
  :: Core.LspFuncs Config.Config
  -> J.NormalizedUri
  -> Maybe FilePath
  -> Maybe Int
  -> IO ()
collectErrors funs uri path version = do
  case path of
    Just fin -> do
      (tree, errs) <- parse (Path fin)
      Core.publishDiagnosticsFunc funs 100 uri version
        $ partitionBySource
        $ map errorToDiag (errs <> collectTreeErrors tree)

    Nothing -> error "TODO: implement URI file loading"

collectTreeErrors :: LIGO Info -> [Msg]
collectTreeErrors = map (getElem *** void) . collect

data ParsedContract = ParsedContract
  { cPath :: FilePath
  , cTree :: LIGO Info
  , cErr  :: [Msg]
  }

-- | Parse whole directory for ligo contracts and collect the results.
-- This ignores every other file which is not a contract.
parseContracts :: FilePath -> IO [ParsedContract]
parseContracts top = let
  exclude p = p /= "." && p /= ".." in do
  ds <- getDirectoryContents top
  contracts <- forM (filter exclude ds) $ \d -> do
    let p = top </> d
    s <- getFileStatus p
    if isDirectory s
      then parseContracts p
      else do
        putStrLn $ "parsing: " ++ show p
        contract <- try @UnsupportedExtension $ parse (Path p)
        case contract of
          Right (tree, errs) ->
            return $ [ParsedContract p tree (errs <> collectTreeErrors tree)]
          Left _ -> return []
  return (concat contracts)

errorToDiag :: (Range, Err Text a) -> J.Diagnostic
errorToDiag (getRange -> (Range (sl, sc, _) (el, ec, _) _), Err what) =
  J.Diagnostic
    (J.Range begin end)
    (Just J.DsError)
    Nothing
    (Just "ligo-lsp")
    (Text.pack [i|Expected #{what}|])
    (Just $ J.List[])
  where
    begin = J.Position (sl - 1) (sc - 1)
    end   = J.Position (el - 1) (ec - 1)

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith (ExitFailure n)
