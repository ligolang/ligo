module Source
  ( Source (..)
  , SourceFile (..)
  , Project (..)
  , withProject
  ) where

import Data.Aeson
  (FromJSON, Options(..), ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON,
  genericToJSON, parseJSON, toJSON)
import Data.Swagger.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema, unwrapUnaryRecords)
import Data.Text.IO qualified as Text
import System.FilePath (takeDirectory, (</>))

import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.IO.Temp (withTempDirectory)
import Util (prepareField)

newtype Source = Source {unSource :: Text}
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Source where
  parseJSON = genericParseJSON defaultOptions
    {unwrapUnaryRecords = True}

instance ToJSON Source where
  toJSON = genericToJSON defaultOptions
    {unwrapUnaryRecords = True}

instance ToSchema Source where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {unwrapUnaryRecords = True}

data SourceFile = SourceFile
  { sfFilePath :: FilePath
  , sfSource :: Source
  } deriving stock (Eq, Ord, Show, Generic)

instance FromJSON SourceFile where
  parseJSON = genericParseJSON defaultOptions
    {fieldLabelModifier = prepareField 2}

instance ToJSON SourceFile where
  toJSON = genericToJSON defaultOptions
    {fieldLabelModifier = prepareField 2}

instance ToSchema SourceFile where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 2}

data Project = Project
  { pSourceFiles :: [SourceFile]
  , pMain :: FilePath
  } deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Project where
  parseJSON = genericParseJSON defaultOptions
    {fieldLabelModifier = prepareField 1}

instance ToJSON Project where
  toJSON = genericToJSON defaultOptions
    {fieldLabelModifier = prepareField 1}

instance ToSchema Project where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 1}

withProject
  :: (MonadIO m, MonadMask m)
  => Project
  -> ((FilePath, FilePath) -> m a)
  -> m a
withProject project f = do
  pwd <- liftIO getCurrentDirectory
  let sourceFiles = pSourceFiles project
      filepaths = fmap sfFilePath sourceFiles
      sources = fmap sfSource sourceFiles
   in withTempDirectory pwd "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> pMain project

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

        f (dirPath, fullMainPath)
