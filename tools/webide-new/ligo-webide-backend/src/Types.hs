module Types
  ( Source (..)
  , DisplayFormat (..)
  ) where

import Data.Aeson
  (FromJSON, Options(..), ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.Swagger.Schema
  (ToSchema, constructorTagModifier, declareNamedSchema, defaultSchemaOptions,
  genericDeclareNamedSchema, unwrapUnaryRecords)
import Data.Text (Text)
import GHC.Generics (Generic)

import Util (prepareField)

newtype Source = Source {unSource :: Text}
  deriving stock (Eq, Show, Ord, Generic)

instance FromJSON Source where
  parseJSON = genericParseJSON defaultOptions
    {unwrapUnaryRecords = True}

instance ToJSON Source where
  toJSON = genericToJSON defaultOptions
    {unwrapUnaryRecords = True}

instance ToSchema Source where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {unwrapUnaryRecords = True}

data DisplayFormat =
  DFDev | DFJson | DFHumanReadable
    deriving stock (Eq, Ord, Show, Enum, Generic)

instance FromJSON DisplayFormat

instance ToJSON DisplayFormat

instance ToSchema DisplayFormat where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {constructorTagModifier = prepareField 2 }
