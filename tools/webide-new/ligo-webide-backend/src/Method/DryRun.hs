module Method.DryRun (dryRun) where

import Data.Text qualified as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Common (WebIDEM)
import Ligo (runLigo)
import Schema.CompilerResponse (CompilerResponse(..))
import Schema.DryRunRequest (DryRunRequest(..))
import Source (withProject)
import Types (DisplayFormat(..))

dryRun :: DryRunRequest -> WebIDEM CompilerResponse
dryRun request =
  withProject (drrProject request) $ \(dirPath, fullMainPath) -> do
    (ec, out, err) <- do
      let commands = ["run", "dry-run", fullMainPath, Text.unpack (drrParameters request), Text.unpack (drrStorage request)]
      let commands1 = (commands ++) $ case drrDisplayFormat request of
            Nothing -> []
            Just df -> ("--display-format":) $ case df of
              DFDev -> ["dev"]
              DFHumanReadable -> ["human-readable"]
              DFJson -> ["json"]
      let commands2 = (commands1 ++) $ case drrProtocol request of
            Nothing -> []
            Just pr -> ["-p", Text.unpack pr]
      let commands3 = (commands2 ++) $ case drrEntrypoint request of
            Nothing -> []
            Just e -> ["-e", Text.unpack e]
       in runLigo dirPath commands3

    case ec of
      ExitSuccess -> pure (CompilerResponse $ Text.pack out)
      ExitFailure _ -> pure (CompilerResponse $ Text.pack err)
