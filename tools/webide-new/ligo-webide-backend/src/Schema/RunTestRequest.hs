module Schema.RunTestRequest (RunTestRequest (..)) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.OpenApi.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema)

import Source (Project(..))
import Types (DisplayFormat(..))
import Util (prepareField)

data RunTestRequest = RunTestRequest
  { rProject :: Project
  , rTestFilePath :: FilePath
  , rDisplayFormat :: Maybe DisplayFormat
  } deriving stock (Eq, Show, Ord, Generic)

instance FromJSON RunTestRequest where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 1}

instance ToJSON RunTestRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 1}

instance ToSchema RunTestRequest where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 1}


