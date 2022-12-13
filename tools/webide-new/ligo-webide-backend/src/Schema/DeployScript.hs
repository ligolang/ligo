module Schema.DeployScript (DeployScript (..)) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.OpenApi.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema)

import Schema.CompilerResponse (CompilerResponse(..))
import Util (prepareField)

data DeployScript = DeployScript
  { dsScript :: Text
  , dsBuild :: CompilerResponse
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DeployScript where
  toJSON = genericToJSON
    defaultOptions {fieldLabelModifier = prepareField 2}

instance FromJSON DeployScript where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 2}

instance ToSchema DeployScript where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 2}
