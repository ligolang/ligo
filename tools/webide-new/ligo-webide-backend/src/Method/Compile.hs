module Method.Compile (compile) where

import Data.Text qualified as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Common (WebIDEM)
import Error (LigoCompilerError(..))
import Ligo (moduleOption, runLigo)
import Schema.CompileRequest (CompileRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Source (withProject)
import Types (prettyDisplayFormat)

compile :: CompileRequest -> WebIDEM CompilerResponse
compile request =
  withProject (rProject request) $ \(dirPath, fullMainPath, pModule) -> do
    let initial = case rStorage request of
          Nothing ->
            ["compile", "contract", fullMainPath]
            ++ moduleOption pModule
          Just storage ->
            ["compile", "storage", "--no-color",
             fullMainPath, Text.unpack storage]
            ++ moduleOption pModule

    (ec, out, err) <- runLigo dirPath $
      initial
      ++ maybe []
           (\df -> ["--display-format", prettyDisplayFormat df])
           (rDisplayFormat request)
      ++ maybe [] (\p -> ["-p", Text.unpack p]) (rProtocol request)

    case ec of
      ExitSuccess -> pure (CompilerResponse $ Text.pack out)
      ExitFailure _ -> throwM $ LigoCompilerError $ Text.pack err
