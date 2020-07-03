
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception as E
import           Control.Lens
import           Control.Monad

import           Data.Default
import qualified Data.Text                             as Text
import           Data.Text                               (Text)
import           Data.Foldable
import           Data.String.Interpolate (i)

import qualified Language.Haskell.LSP.Control          as CTRL
import qualified Language.Haskell.LSP.Core             as Core
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages         as Msg
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import qualified Language.Haskell.LSP.Utility          as U
-- import           Language.Haskell.LSP.VFS

import           System.Exit
import qualified System.Log                            as L

import           Parser
import           Range
import           Product
import           AST hiding (def)
import qualified AST.Find as Find
import           Error
import           Tree

main :: IO ()
main = do
  -- for_ [1.. 100] \_ -> do
  --   print =<< runParser contract example
  errCode <- mainLoop
  exit errCode

mainLoop :: IO Int
mainLoop = do
    chan <- atomically newTChan :: IO (TChan FromClientMessage)

    let
      callbacks = Core.InitializeCallbacks
        { Core.onInitialConfiguration = const $ Right ()
        , Core.onConfigurationChange = const $ Right ()
        , Core.onStartup = \lFuns -> do
            _ <- forkIO $ eventLoop lFuns chan
            return Nothing
        }

    Core.setupLogger (Just "log.txt") [] L.INFO
    CTRL.run callbacks (lspHandlers chan) lspOptions (Just "log.txt")
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
lspHandlers rin = def
  { Core.initializedHandler                       = Just $ passHandler rin NotInitialized
  , Core.renameHandler                            = Just $ passHandler rin ReqRename
  , Core.hoverHandler                             = Just $ passHandler rin ReqHover
  , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
  , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
  , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
  , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
  , Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
  , Core.responseHandler                          = Just $ responseHandlerCb rin
  , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
  , Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
  }

passHandler :: TChan FromClientMessage -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (c notification)

responseHandlerCb :: TChan FromClientMessage -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

send :: Core.LspFuncs () -> FromServerMessage -> IO ()
send = Core.sendFunc

nextID :: Core.LspFuncs () -> IO J.LspId
nextID = Core.getNextReqId

eventLoop :: Core.LspFuncs () -> TChan FromClientMessage -> IO ()
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
        let uri = req^.J.params.J.textDocument.J.uri
        let pos = posToRange $ req^.J.params.J.position
        tree <- loadByURI uri
        case Find.definitionOf pos tree of
          Just defPos -> do
            error "do later"

      _ -> U.logs "unknown msg"

posToRange :: J.Position -> Range
posToRange (J.Position l c) = Range (l, c, 0) (l, c, 0) ""

rangeToJRange :: Range -> J.Range
rangeToJRange (Range (a, b, _) (c, d, _) _) = J.Range (J.Position a b) (J.Position c d)

rangeToLoc :: Range -> J.Range
rangeToLoc (Range (a, b, _) (c, d, _) _) = J.Range (J.Position a b) (J.Position c d)

loadByURI :: J.Uri -> IO (Pascal (Product [[ScopedDecl], Range, [Text]]))
loadByURI uri = do
  case J.uriToFilePath uri of
    Just fin -> do
      (tree, _) <- runParser contract fin
      return $ addLocalScopes tree

collectErrors
  :: Core.LspFuncs ()
  -> J.NormalizedUri
  -> Maybe FilePath
  -> Maybe Int
  -> IO ()
collectErrors funs uri path version = do
  case path of
    Just fin -> do
      (tree, errs) <- runParser contract fin
      Core.publishDiagnosticsFunc funs 100 uri version
        $ partitionBySource
        $ map errorToDiag (errs <> errors tree)

    Nothing -> error "TODO: implement URI file loading"

errorToDiag :: Error ASTInfo -> J.Diagnostic
errorToDiag (Expected what _ (getRange -> (Range (sl, sc, _) (el, ec, _) _))) =
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
