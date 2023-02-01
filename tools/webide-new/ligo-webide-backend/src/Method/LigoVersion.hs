module Method.LigoVersion (ligoVersion) where
  
import Data.Text qualified as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Directory (getCurrentDirectory)
import System.IO.Temp (withTempDirectory)

import Common (WebIDEM)
import Error (LigoCompilerError(..))
import Ligo (runLigo)

ligoVersion :: WebIDEM Text
ligoVersion = do
  pwd <- liftIO getCurrentDirectory
  withTempDirectory pwd "" $ \dirPath -> do
    (ec, out, err) <- runLigo dirPath [ "--version"]
    case ec of
      ExitSuccess -> pure $ Text.strip $ Text.pack out
      ExitFailure _ -> throwM $ LigoCompilerError $ Text.pack err
