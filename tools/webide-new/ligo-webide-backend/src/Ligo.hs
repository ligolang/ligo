module Ligo (runLigo) where

import System.Exit (ExitCode)
import System.Process (proc, readCreateProcessWithExitCode)

import Common (WebIDEM)
import Config (cDockerizedLigoVersion, cLigoPath)
import Error (LigoCompilerError(..))

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
        Nothing -> throwM NoLigoBinary
        Just ligoPath ->
          liftIO $ readCreateProcessWithExitCode (proc ligoPath commands) ""
