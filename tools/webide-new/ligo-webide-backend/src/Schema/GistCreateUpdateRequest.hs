module Schema.GistCreateUpdateRequest (GistCreateUpdateRequest(..)) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.OpenApi.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema)

import Source (SourceFile(..))
import Util (prepareField)

data GistCreateUpdateRequest = GistCreateUpdateRequest
  { gcuSourceFiles :: [SourceFile]
  , gcuDescription :: Maybe Text
  , gcuGistId :: Maybe Text
  } deriving stock (Eq, Show, Ord, Generic)

instance FromJSON GistCreateUpdateRequest where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON GistCreateUpdateRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToSchema GistCreateUpdateRequest where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 3}
