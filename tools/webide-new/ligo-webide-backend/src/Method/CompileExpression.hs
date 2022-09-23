module Method.CompileExpression (compileExpression) where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Servant (err400, errBody)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withTempDirectory)

import Common (WebIDEM)
import Ligo (runLigo)
import Schema.CompileExpressionRequest (CompileExpressionRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Types (DisplayFormat(..), Source(..))

compileExpression :: CompileExpressionRequest -> WebIDEM CompilerResponse
compileExpression request = do
  pwd <- liftIO getCurrentDirectory
  let (filepaths, sources) = unzip (cerSources request)
   in withTempDirectory pwd "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> cerMain request

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

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
