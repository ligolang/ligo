module Source
  ( Source (..)
  , SourceFile (..)
  , Project (..)
  ) where

import Data.Aeson
  (FromJSON, Options(..), ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON,
  genericToJSON, parseJSON, toJSON)
import Data.Swagger.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema, unwrapUnaryRecords)
import Data.Text (Text)
import GHC.Generics (Generic)

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
