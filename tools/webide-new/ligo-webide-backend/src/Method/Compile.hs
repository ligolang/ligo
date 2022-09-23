module Method.Compile (compile) where

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
import Schema.CompileRequest (CompileRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Types (DisplayFormat(..), Source(..))

compile :: CompileRequest -> WebIDEM CompilerResponse
compile request = do
  pwd <- liftIO getCurrentDirectory
  let (filepaths, sources) = unzip (rSources request)
   in withTempDirectory pwd "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> rMain request

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

        (ec, out, err) <- do
          let commands = case rStorage request of
                Nothing -> ["compile", "contract", fullMainPath]
                Just storage -> ["compile", "storage", fullMainPath, Text.unpack storage]
          let commands1 = (commands ++) $ case rDisplayFormat request of
                Nothing -> []
                Just df -> ("--display-format":) $ case df of
                  DFDev -> ["dev"]
                  DFHumanReadable -> ["human-readable"]
                  DFJson -> ["json"]
          let commands2 = (commands1 ++) $ case rProtocol request of
                Nothing -> []
                Just pr -> ["-p", Text.unpack pr]
           in runLigo dirPath commands2

        case ec of
          ExitSuccess -> pure (CompilerResponse $ Text.pack out)
          ExitFailure _ -> pure (CompilerResponse $ Text.pack err)
