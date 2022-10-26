module Schema.CompilerResponse (CompilerResponse (..)) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON,
  unwrapUnaryRecords)
import Data.OpenApi.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, genericDeclareNamedSchema,
  unwrapUnaryRecords)

newtype CompilerResponse = CompilerResponse {unCompilerResponse :: Text}
  deriving stock (Show, Generic, Eq)

instance ToJSON CompilerResponse where
  toJSON = genericToJSON
    defaultOptions {unwrapUnaryRecords = True}

instance FromJSON CompilerResponse where
  parseJSON = genericParseJSON
    defaultOptions {unwrapUnaryRecords = True}

instance ToSchema CompilerResponse where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {unwrapUnaryRecords = True}
