module Schema.CompileExpressionRequest (CompileExpressionRequest (..)) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.Swagger.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

import Source (Project(..))
import Types (DisplayFormat(..))
import Util (prepareField)

data CompileExpressionRequest = CompileExpressionRequest
  { cerProject :: Project
  , cerFunction :: Text
  , cerProtocol :: Maybe Text
  , cerDisplayFormat :: Maybe DisplayFormat
  } deriving stock (Eq, Show, Ord, Generic)

instance FromJSON CompileExpressionRequest where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON CompileExpressionRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToSchema CompileExpressionRequest where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 3}


