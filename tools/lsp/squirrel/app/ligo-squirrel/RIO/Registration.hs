module RIO.Registration
  ( registerDidChangeConfiguration
  , registerFileWatcher
  ) where

import Control.Monad (void)
import Data.Text qualified as T
import Language.LSP.Server qualified as S
import Language.LSP.Types qualified as J
import System.FilePath ((</>))

import Extension (extGlobs)
import RIO.Indexing (ligoProjectName)
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
    ligoFilesOpts = J.DidChangeWatchedFilesRegistrationOptions $ J.List $ map watcher extGlobs
    ligoFilesReg = J.Registration "ligoFileWatcher" J.SWorkspaceDidChangeWatchedFiles ligoFilesOpts

    projGlob = T.pack $ "**" </> ligoProjectName
    projWatcher = J.FileSystemWatcher
      { J._globPattern = projGlob
      , J._kind = Just J.WatchKind
        { J._watchChange = True
        , J._watchCreate = True
        , J._watchDelete = True
        }
      }
    ligoProjOpts = J.DidChangeWatchedFilesRegistrationOptions $ J.List [projWatcher]
    ligoProjReg = J.Registration "ligoProjectWatcher" J.SWorkspaceDidChangeWatchedFiles ligoProjOpts

    regParams = J.RegistrationParams $ J.List
      [ J.SomeRegistration ligoFilesReg
      , J.SomeRegistration ligoProjReg
      ]

  void $ S.sendRequest J.SClientRegisterCapability regParams (const $ pure ())
