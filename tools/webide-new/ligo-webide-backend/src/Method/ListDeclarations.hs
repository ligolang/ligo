module Method.ListDeclarations (listDeclarations) where

import Data.Text qualified as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Common (WebIDEM)
import Error (LigoCompilerError(..))
import Ligo (runLigo)
import Schema.ListDeclarationsRequest (ListDeclarationsRequest(..))
import Schema.ListDeclarationsResponse (ListDeclarationsResponse)
import Source (withProject)

listDeclarations :: ListDeclarationsRequest -> WebIDEM ListDeclarationsResponse
listDeclarations request =
  withProject (ldrProject request) $ \(dirPath, fullMainPath) -> do
    (ec, out, err) <- runLigo dirPath $
      [ "info", "list-declarations", "--no-color" , fullMainPath]
      ++ ["--display-format", "dev"]
      ++ ["--only-ep" | fromMaybe False $ ldrOnlyEndpoint request]

    case ec of
      ExitSuccess -> do
        -- TODO: make this more robust
        case Text.lines . Text.strip . Text.pack $ out of
          _ : xs -> pure xs
          _ -> pure []
      ExitFailure _ -> throwM $ LigoCompilerError $ Text.pack err
