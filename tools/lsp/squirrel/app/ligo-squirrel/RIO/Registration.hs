module RIO.Registration
  ( registerDidChangeConfiguration
  , registerFileWatcher
  ) where

import Control.Monad (void)
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J

import Extension (extGlobs)
import RIO.Types (RIO)

registerDidChangeConfiguration :: RIO ()
registerDidChangeConfiguration = do
  let
    reg = J.Registration "ligoChangeConfiguration" J.SWorkspaceDidChangeConfiguration J.Empty
    params = J.RegistrationParams $ J.List [J.SomeRegistration reg]

  void $ S.sendRequest J.SClientRegisterCapability params (const $ pure ())

registerFileWatcher :: RIO ()
registerFileWatcher = do
  let
    watcher extGlob = J.FileSystemWatcher
      { J._globPattern = extGlob
      , J._kind = Just J.WatchKind
        { J._watchChange = True
        , J._watchCreate = True
        , J._watchDelete = True
        }
      }
    regOpts = J.DidChangeWatchedFilesRegistrationOptions $ J.List $ map watcher extGlobs
    reg = J.Registration "ligoFileWatcher" J.SWorkspaceDidChangeWatchedFiles regOpts
    regParams = J.RegistrationParams $ J.List [J.SomeRegistration reg]

  void $ S.sendRequest J.SClientRegisterCapability regParams (const $ pure ())
