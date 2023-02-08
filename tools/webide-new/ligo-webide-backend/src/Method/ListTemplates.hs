module Method.ListTemplates (listTemplates) where

import Data.Text qualified as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Directory (getCurrentDirectory)
import System.IO.Temp (withTempDirectory)

import Common (WebIDEM)
import Error (LigoCompilerError(..))
import Ligo (runLigo)

listTemplates :: WebIDEM [Text]
listTemplates = do
  pwd <- liftIO getCurrentDirectory
  withTempDirectory pwd "" $ \dirPath -> do
    (ec, out, err) <- runLigo dirPath [ "init", "contract", "--no-color", "--template-list"]
    case ec of
      ExitSuccess -> do
        case Text.lines . Text.strip . Text.pack $ out of
          _ : xs -> pure xs
          _ -> pure []
      ExitFailure _ -> throwM $ LigoCompilerError $ Text.pack err
