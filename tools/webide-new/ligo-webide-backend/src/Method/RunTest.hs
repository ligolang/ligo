module Method.RunTest (runTest) where

import Data.Text qualified as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Common (WebIDEM)
import Error (LigoCompilerError(..))
import Ligo (runLigo, moduleOption)
import Schema.RunTestRequest (RunTestRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Source (withProject)
import Types (prettyDisplayFormat)


runTest :: RunTestRequest -> WebIDEM CompilerResponse
runTest request =
  withProject (rProject request) $ \(dirPath, fullMainPath, pModule) -> do
    (ec, out, err) <- runLigo dirPath $
      ["run", "test", dirPath ++ "/" ++ (rTestFilePath request)]
      ++ maybe []
           (\df -> ["--display-format", prettyDisplayFormat df])
           (rDisplayFormat request)

    case ec of
      ExitSuccess -> pure (CompilerResponse $ Text.pack out)
      ExitFailure _ -> throwM $ LigoCompilerError $ Text.pack err
