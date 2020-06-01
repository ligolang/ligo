
import Data.Foldable (for_)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception as E
import           Control.Lens
import           Control.Monad

import qualified Data.Text                             as Text
import           Data.String.Interpolate (i)

import qualified Language.Haskell.LSP.Control          as CTRL
import qualified Language.Haskell.LSP.Core             as Core
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages         as Msg
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import qualified Language.Haskell.LSP.Utility          as U
import           Language.Haskell.LSP.VFS

import           System.Environment
import           System.Exit
import qualified System.Log                            as L

import           ParseTree
import           Parser
import           Range
import           AST
import           Pretty

main :: IO ()
main = do
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
    return 0
  `catches`
    [ Handler \(e :: SomeException) -> do
        print e
        return 1
    ]

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

errorToDiag :: Error -> J.Diagnostic
errorToDiag (Expected what instead (Range (sl, sc, _) (el, ec, _))) =
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
