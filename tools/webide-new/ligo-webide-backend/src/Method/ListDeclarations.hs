module Method.ListDeclarations (listDeclarations) where

import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as Text
import Servant (err400, errBody)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Common (WebIDEM)
import Ligo (runLigo)
import Schema.ListDeclarationsRequest (ListDeclarationsRequest(..))
import Schema.ListDeclarationsResponse (ListDeclarationsResponse)
import Source (withProject)

listDeclarations :: ListDeclarationsRequest -> WebIDEM ListDeclarationsResponse
listDeclarations request =
  withProject (ldrProject request) $ \(dirPath, fullMainPath) -> do
    (ec, out, err) <- runLigo dirPath $
      [ "info", "list-declarations" , fullMainPath]
      ++ ["--display-format", "dev"]
      ++ ["--only-ep" | ldrOnlyEndpoint request]

    case ec of
      ExitSuccess -> do
        -- TODO: make this more robust
        pure . tail . Text.lines . Text.strip . Text.pack $ out
      ExitFailure _ -> lift . throwError $ err400
        {errBody = LBS.pack err}
