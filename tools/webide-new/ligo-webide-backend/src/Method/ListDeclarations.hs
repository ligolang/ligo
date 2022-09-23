module Method.ListDeclarations (listDeclarations) where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Servant (err400, errBody)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withTempDirectory)

import Common (WebIDEM)
import Ligo (runLigo)
import Schema.ListDeclarationsRequest (ListDeclarationsRequest(..))
import Schema.ListDeclarationsResponse (ListDeclarationsResponse)
import Types (Source(..))

listDeclarations :: ListDeclarationsRequest -> WebIDEM ListDeclarationsResponse
listDeclarations request = do
  pwd <- liftIO getCurrentDirectory
  let (filepaths, sources) = unzip (ldrSources request)
   in withTempDirectory pwd "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> ldrMain request

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

        (ec, out, err) <- do
          let commands =
                [ "info"
                , "list-declarations"
                , fullMainPath
                , "--display-format"
                , "dev"]
                 ++["--only-ep" | ldrOnlyEndpoint request]
           in runLigo dirPath commands

        case ec of
          ExitSuccess -> do
            -- TODO: make this more robust
            pure . tail . Text.lines . Text.strip . Text.pack $ out
          ExitFailure _ -> lift . throwError $ err400
           {errBody = LBS.pack err}
