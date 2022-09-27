module Method.CompileExpression (compileExpression) where

import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Data.Text qualified as Text
import Servant (err400, errBody)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Common (WebIDEM)
import Ligo (runLigo)
import Schema.CompileExpressionRequest (CompileExpressionRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Source (withProject)
import Types (DisplayFormat(..))

compileExpression :: CompileExpressionRequest -> WebIDEM CompilerResponse
compileExpression request =
  withProject (cerProject request) $ \(dirPath, fullMainPath) -> do
    dialect <- case inferDialect fullMainPath of
      Nothing -> lift . throwError $ err400
        {errBody = "couldn't infer dialect from filetype"}
      Just d -> pure d

    (ec, out, err) <- do
      let commands = ["compile", "expression", prettyDialect dialect, Text.unpack (cerFunction request)]
      let commands1 = (commands ++) $ case cerDisplayFormat request of
            Nothing -> []
            Just df -> ("--display-format":) $ case df of
              DFDev -> ["dev"]
              DFHumanReadable -> ["human-readable"]
              DFJson -> ["json"]
      let commands2 = (commands1 ++) $ case cerProtocol request of
            Nothing -> []
            Just pr -> ["-p", Text.unpack pr]
      let commands3 = commands2 ++ ["--init-file", fullMainPath]

       in runLigo dirPath commands3

    case ec of
      ExitSuccess -> pure (CompilerResponse $ Text.pack out)
      ExitFailure _ -> pure (CompilerResponse $ Text.pack err)

data Dialect = CameLIGO | PascaLIGO | JsLIGO
  deriving stock (Eq, Show, Ord, Enum)

prettyDialect :: Dialect -> String
prettyDialect = \case
  CameLIGO -> "cameligo"
  PascaLIGO -> "pascaligo"
  JsLIGO -> "jsligo"

inferDialect :: FilePath -> Maybe Dialect
inferDialect filepath =
  case Text.takeWhileEnd (/= '.') (Text.pack filepath) of
    "mligo" -> Just CameLIGO
    "ligo" -> Just PascaLIGO
    "jsligo" -> Just JsLIGO
    _ -> Nothing
