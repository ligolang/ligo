module Method.Compile (compile) where

import Data.Text qualified as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Common (WebIDEM)
import Ligo (runLigo)
import Schema.CompileRequest (CompileRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Source (withProject)
import Types (DisplayFormat(..))

compile :: CompileRequest -> WebIDEM CompilerResponse
compile request =
  withProject (rProject request) $ \(dirPath, fullMainPath) -> do
    (ec, out, err) <- do
      let commands = case rStorage request of
            Nothing -> ["compile", "contract", fullMainPath]
            Just storage -> ["compile", "storage", fullMainPath, Text.unpack storage]
      let commands1 = (commands ++) $ case rDisplayFormat request of
            Nothing -> []
            Just df -> ("--display-format":) $ case df of
              DFDev -> ["dev"]
              DFHumanReadable -> ["human-readable"]
              DFJson -> ["json"]
      let commands2 = (commands1 ++) $ case rProtocol request of
            Nothing -> []
            Just pr -> ["-p", Text.unpack pr]
       in runLigo dirPath commands2

    case ec of
      ExitSuccess -> pure (CompilerResponse $ Text.pack out)
      ExitFailure _ -> pure (CompilerResponse $ Text.pack err)
