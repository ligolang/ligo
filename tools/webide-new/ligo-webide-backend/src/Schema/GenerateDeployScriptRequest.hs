module Schema.GenerateDeployScriptRequest
  ( GenerateDeployScriptRequest (..)
  ) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.Swagger.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

import Types (Source(..))
import Util (prepareField)

data GenerateDeployScriptRequest = GenerateDeployScriptRequest
  { gdsrName :: Text
  , gdsrSources :: [(FilePath, Source)]
  , gdsrMain :: FilePath
  , gdsrStorage :: Text
  , gdsrEntrypoint :: Maybe Text
  , gdsrProtocol :: Maybe Text
  }
  deriving stock (Show, Generic)

instance FromJSON GenerateDeployScriptRequest where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 4}

instance ToJSON GenerateDeployScriptRequest where
  toJSON = genericToJSON
    defaultOptions {fieldLabelModifier = prepareField 4}

instance ToSchema GenerateDeployScriptRequest where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 4}
