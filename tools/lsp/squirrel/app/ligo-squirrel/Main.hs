
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe (handleAny)
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Data.Default
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T

import qualified Language.LSP.Control as CTRL
import qualified Language.LSP.Core as Core
import Language.LSP.Messages as Msg
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import qualified Language.LSP.Utility as U


import System.Exit
import qualified System.Log as L

import AST
import qualified ASTMap
import Cli.Types
import qualified Config
import Language.LSP.Util (MessageDescription (..), describeFromClientMessage)
import qualified Log
import Product
import Range
import RIO (RIO)
import qualified RIO

main :: IO ()
main = do
  Log.setLogLevel Log.ERROR
  exit =<< mainLoop

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
        Log.err "INIT" (show e)
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
    , Core.selectionRangeHandler = Just $ passHandler rin ReqSelectionRange
    , Core.hoverHandler = Just $ passHandler rin ReqHover
    , Core.documentSymbolHandler = Just $ passHandler rin ReqDocumentSymbols
    , Core.renameHandler = Just $ passHandler rin ReqRename
    -- , Core.prepareRenameHandler = Just $ passHandler rin ReqPrepareRename -- TODO: call ligo compiller on rename request
    }

passHandler :: TChan FromClientMessage -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (c notification)

responseHandlerCb :: TChan FromClientMessage -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

eventLoop :: Core.LspFuncs Config.Config -> TChan FromClientMessage -> IO ()
eventLoop funs chan = do
  astMap <- ASTMap.empty $ RIO.load . J.fromNormalizedUri
  Just (Config.Config { _cLigoBinaryPath = _lceClientPath }) <- Core.config funs
  forever do
    msg <- atomically do
      readTChan chan

    Log.debug "LOOP" [i|START message: #{show msg}|]

    async $ handleAny (sendErrorResponse msg) $ do
      -- TODO: client freezes when trying to extract LigoEnv from extension
      RIO.run (astMap :> funs :> def { _lceClientPath } :> Nil) do
      -- RIO.run (astMap :> funs :> def :> Nil) do
        case msg of
          RspFromClient            {}    -> return ()
          NotInitialized           notif -> handleInitialized                  notif
          NotDidOpenTextDocument   notif -> handleDidOpenTextDocument          notif
          NotDidChangeTextDocument notif -> handleDidChangeTextDocument        notif
          ReqDefinition            req   -> handleDefinitionRequest            req
          ReqFindReferences        req   -> handleFindReferencesRequest        req
          ReqCompletion            req   -> handleCompletionRequest            req
          ReqDocumentSymbols       req   -> handleDocumentSymbolsRequest       req
          ReqFoldingRange          req   -> handleFoldingRangeRequest          req
          ReqSelectionRange        req   -> handleSelectionRangeRequest        req
          ReqHover                 req   -> handleHoverRequest                 req
          ReqRename                req   -> handleRenameRequest                req

          -- Additional callback executed after completion was made, currently no-op
          ReqCompletionItemResolve req   -> handleCompletionItemResolveRequest req

          _ -> liftIO do
            Log.err "LOOP" $ "unknown msg: " <> show (describeFromClientMessage msg)

      Log.debug "LOOP" [i|DONE message: #{take 50 $ show msg}|]
 where
  sendErrorResponse :: FromClientMessage -> SomeException -> IO ()
  sendErrorResponse msg e = case describeFromClientMessage msg of
    MessageRequest _ reqId ->
      Core.sendErrorResponseS (Core.sendFunc funs) (J.responseId reqId)
        J.InternalError (T.pack $ displayException e)
    MessageResponse method respId ->
      Core.sendErrorShowS (Core.sendFunc funs) $
        "Error processing response `" <> method <> "` #" <> (T.pack $ show respId) <> "."
    MessageNotification method ->
      Core.sendErrorShowS (Core.sendFunc funs) $
        "Error processing `" <> method <> "`."


handleInitialized :: J.InitializedNotification -> RIO ()
handleInitialized _notif = do
  let
    registration = J.Registration
      "lsp-haskell-registered"
      J.WorkspaceExecuteCommand
      Nothing
    registrations = J.RegistrationParams $ J.List [registration]

  rid <- RIO.freshID
  RIO.respond
    $ ReqRegisterCapability
    $ fmServerRegisterCapabilityRequest rid registrations

handleDidOpenTextDocument :: J.DidOpenTextDocumentNotification -> RIO ()
handleDidOpenTextDocument notif = do
  let doc = notif^.J.params.J.textDocument.J.uri
  let ver = notif^.J.params.J.textDocument.J.version
  RIO.collectErrors RIO.forceFetch (J.toNormalizedUri doc) (Just ver)

handleDidChangeTextDocument :: J.DidChangeTextDocumentNotification -> RIO ()
handleDidChangeTextDocument notif = do
  tmap <- asks getElem
  let uri = notif^.J.params.J.textDocument.J.uri.to J.toNormalizedUri
  ASTMap.invalidate uri tmap
  RIO.collectErrors (flip ASTMap.fetchBundled tmap) uri (Just 0)

handleDefinitionRequest :: J.DefinitionRequest -> RIO ()
handleDefinitionRequest req = do
    let uri = req^.J.params.J.textDocument.J.uri
    let pos = fromLspPosition $ req^.J.params.J.position
    (tree, _) <- RIO.fetch $ J.toNormalizedUri uri
    case AST.definitionOf pos tree of
      Just defPos -> RIO.respondWith req RspDefinition $ J.MultiLoc [J.Location uri $ toLspRange defPos]
      Nothing     -> RIO.respondWith req RspDefinition $ J.MultiLoc []

handleFindReferencesRequest :: J.ReferencesRequest -> RIO ()
handleFindReferencesRequest req = do
    let uri  = req^.J.params.J.textDocument.J.uri
    let nuri = J.toNormalizedUri uri
    let pos  = fromLspPosition $ req^.J.params.J.position
    (tree, _) <- RIO.fetch nuri
    case AST.referencesOf pos tree of
      Just refs -> do
        let locations = J.Location uri . toLspRange <$> refs
        RIO.respondWith req RspFindReferences $ J.List locations
      Nothing -> do
        RIO.respondWith req RspFindReferences $ J.List []

handleCompletionRequest :: J.CompletionRequest -> RIO ()
handleCompletionRequest req = do
    RIO.log $ "got completion request: " <> show req
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let pos = fromLspPosition $ req ^. J.params . J.position
    (tree, _) <- RIO.fetch uri
    let completions = fmap toCompletionItem . fromMaybe [] $ complete pos tree
    RIO.respondWith req RspCompletion . J.Completions . J.List $ completions

handleCompletionItemResolveRequest :: J.CompletionItemResolveRequest -> RIO ()
handleCompletionItemResolveRequest req = do
    RIO.log $ "got completion resolve request: " <> show req
    RIO.respondWith req RspCompletionItemResolve (req ^. J.params)

handleFoldingRangeRequest :: J.FoldingRangeRequest -> RIO ()
handleFoldingRangeRequest req = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    (tree, _) <- RIO.fetch uri
    actions <- foldingAST tree
    RIO.liftLsp \funs -> do
      Core.sendFunc funs
        $ RspFoldingRange
        $ Core.makeResponseMessage req
        $ J.List
        $ fmap toFoldingRange
        $ actions

handleSelectionRangeRequest :: J.SelectionRangeRequest -> RIO ()
handleSelectionRangeRequest req = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let positions = req ^. J.params . J.positions
    (tree, _) <- RIO.fetch uri
    let results = map (findSelectionRange tree) positions
        response = (RspSelectionRange
                      (Core.makeResponseMessage req (J.List results)))
    RIO.liftLsp \funs -> do
      Core.sendFunc funs response

handleDocumentSymbolsRequest :: J.DocumentSymbolRequest -> RIO ()
handleDocumentSymbolsRequest req = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    (tree, _) <- RIO.fetch uri
    result <- extractDocumentSymbols (J.fromNormalizedUri uri) tree
    RIO.respondWith req RspDocumentSymbols (J.DSSymbolInformation $ J.List result)

handleHoverRequest :: J.HoverRequest -> RIO ()
handleHoverRequest req = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    (tree, _) <- RIO.fetch uri
    let pos = fromLspPosition $ req ^. J.params . J.position
    RIO.respondWith req RspHover (hoverDecl pos tree)

handleRenameRequest :: J.RenameRequest -> RIO ()
handleRenameRequest req = do
    let uri  = req ^. J.params . J.textDocument . J.uri
    let nuri = J.toNormalizedUri uri
    let pos  = fromLspPosition $ req ^. J.params . J.position
    let newName = req ^. J.params . J.newName

    (tree, _) <- RIO.fetch nuri

    RIO.liftLsp $ \funs -> case renameDeclarationAt pos tree newName of
      NotFound -> do
        Log.debug "Rename" [i|Declaration not found for: #{show req}|]
        Core.sendErrorResponseS (Core.sendFunc funs) (req ^. J.id . to J.responseId)
          J.InvalidRequest "Cannot rename this"
      Ok edits ->
        let
          -- TODO: change this once we set up proper module system integration
          documentChanges = J.List
            [ J.TextDocumentEdit
                { _textDocument = J.VersionedTextDocumentIdentifier uri Nothing
                , _edits = J.List edits
                }
            ]

          response = RspRename $ Core.makeResponseMessage req
            J.WorkspaceEdit
              { _changes = Nothing
              , _documentChanges = Just documentChanges
              }
        in Core.sendFunc funs response

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith (ExitFailure n)
