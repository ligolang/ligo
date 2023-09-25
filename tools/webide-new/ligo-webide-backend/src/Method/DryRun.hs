module Method.DryRun (dryRun) where

import Data.Text qualified as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Common (WebIDEM)
import Error (LigoCompilerError(..))
import Ligo (runLigo, moduleOption)
import Schema.CompilerResponse (CompilerResponse(..))
import Schema.DryRunRequest (DryRunRequest(..))
import Source (withProject)
import Types (prettyDisplayFormat)

dryRun :: DryRunRequest -> WebIDEM CompilerResponse
dryRun request =
  withProject (drrProject request) $ \(dirPath, fullMainPath, pModule) -> do
    (ec, out, err) <- runLigo dirPath $
      ["run", "dry-run", "--no-color", fullMainPath]
      ++ moduleOption pModule
      ++ [Text.unpack (drrParameters request), Text.unpack (drrStorage request)]
      ++ maybe []
           (\df -> ["--display-format", prettyDisplayFormat df])
           (drrDisplayFormat request)
      ++ maybe [] (\p -> ["-p", Text.unpack p]) (drrProtocol request)
      ++ maybe [] (\e -> ["-e", Text.unpack e]) (drrEntrypoint request)

    case ec of
      ExitSuccess -> pure (CompilerResponse $ Text.pack out)
      ExitFailure _ -> throwM $ LigoCompilerError $ Text.pack err
