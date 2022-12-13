module SwaggerSchema
  ( webIdeOpenApi
  ) where

import Lens.Micro.Platform (zoom, (.=), (?=))
import Servant.OpenApi

import Api (API)
import Data.OpenApi qualified as DO

webIdeOpenApi :: DO.OpenApi
webIdeOpenApi = executingState (toOpenApi (Proxy @API)) $ do
    zoom DO.info $ do
      DO.title .= "WebIde backend API"
      DO.version .= "1.0.0"
      DO.contact ?= mempty `executingState` do
        DO.name ?= "Serokell OÜ"
        DO.email ?= "hi@serokell.io"
        DO.url ?= DO.URL "https://serokell.io"

    DO.externalDocs ?= mempty `executingState` do
      DO.description ?= "Find out more about Swagger"
      DO.url .= DO.URL "http://swagger.io"
