module Ligo (runLigo) where

import System.Exit (ExitCode)
import System.Process (proc, readCreateProcessWithExitCode)

import Common (WebIDEM)
import Config (scDockerizedLigoVersion, scLigoPath)

runLigo :: FilePath -> [String] -> WebIDEM (ExitCode, String, String)
runLigo dirPath commands = do
  dockerizedLigo <- lift (asks scDockerizedLigoVersion)
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
      ligoPath <- lift (asks scLigoPath)
      liftIO $ readCreateProcessWithExitCode (proc ligoPath commands) ""
