module Method.DryRun (dryRun) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withTempDirectory)

import Common (WebIDEM)
import Ligo (runLigo)
import Schema.CompilerResponse (CompilerResponse(..))
import Schema.DryRunRequest (DryRunRequest(..))
import Types (DisplayFormat(..), Source(..))

dryRun :: DryRunRequest -> WebIDEM CompilerResponse
dryRun request = do
  pwd <- liftIO getCurrentDirectory
  let (filepaths, sources) = unzip (drrSources request)
   in withTempDirectory pwd "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> drrMain request

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

        (ec, out, err) <- do
          let commands = ["run", "dry-run", fullMainPath, Text.unpack (drrParameters request), Text.unpack (drrStorage request)]
          let commands1 = (commands ++) $ case drrDisplayFormat request of
                Nothing -> []
                Just df -> ("--display-format":) $ case df of
                  DFDev -> ["dev"]
                  DFHumanReadable -> ["human-readable"]
                  DFJson -> ["json"]
          let commands2 = (commands1 ++) $ case drrProtocol request of
                Nothing -> []
                Just pr -> ["-p", Text.unpack pr]
          let commands3 = (commands2 ++) $ case drrEntrypoint request of
                Nothing -> []
                Just e -> ["-e", Text.unpack e]
           in runLigo dirPath commands3

        case ec of
          ExitSuccess -> pure (CompilerResponse $ Text.pack out)
          ExitFailure _ -> pure (CompilerResponse $ Text.pack err)
