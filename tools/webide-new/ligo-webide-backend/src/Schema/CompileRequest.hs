module Schema.CompileRequest (CompileRequest (..)) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.Swagger.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

import Types (DisplayFormat(..), Source(..))
import Util (prepareField)

data CompileRequest = CompileRequest
  { rSources :: [(FilePath, Source)]
  , rMain :: FilePath
  , rEntrypoint :: Maybe Text
  , rProtocol :: Maybe Text
  , rStorage :: Maybe Text
  , rDisplayFormat :: Maybe DisplayFormat
  } deriving stock (Eq, Show, Ord, Generic)

instance FromJSON CompileRequest where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 1}

instance ToJSON CompileRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 1}

instance ToSchema CompileRequest where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 1}


