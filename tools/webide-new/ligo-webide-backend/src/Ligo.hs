module Ligo (runLigo) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Servant (err500, errBody)
import System.Exit (ExitCode)
import System.Process (proc, readCreateProcessWithExitCode)

import Common (WebIDEM)
import Config (cDockerizedLigoVersion, cLigoPath)

runLigo :: FilePath -> [String] -> WebIDEM (ExitCode, String, String)
runLigo dirPath commands = do
  dockerizedLigo <- lift (asks cDockerizedLigoVersion)
  case dockerizedLigo of
    Just version ->
      liftIO
      $ flip readCreateProcessWithExitCode ""
      $ proc "docker"
      $ [ "run"
        , "--rm"
        , "-v"
        , dirPath ++ ":" ++ dirPath
        , "-w"
        , dirPath
        , "ligolang/ligo:" ++ version
        ]
        ++ commands
    Nothing -> do
      mLigoPath <- lift (asks cLigoPath)
      case mLigoPath of
        Nothing -> lift $ throwError err500
          {errBody = "server doesn't have access to LIGO binary."}
        Just ligoPath ->
          liftIO $ readCreateProcessWithExitCode (proc ligoPath commands) ""
