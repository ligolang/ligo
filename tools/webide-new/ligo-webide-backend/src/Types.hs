module Types
  ( DisplayFormat (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger.Schema
  (ToSchema, constructorTagModifier, declareNamedSchema, defaultSchemaOptions,
  genericDeclareNamedSchema)
import GHC.Generics (Generic)

import Util (prepareField)

data DisplayFormat =
  DFDev | DFJson | DFHumanReadable
    deriving stock (Eq, Ord, Show, Enum, Generic)

instance FromJSON DisplayFormat

instance ToJSON DisplayFormat

instance ToSchema DisplayFormat where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {constructorTagModifier = prepareField 2 }
