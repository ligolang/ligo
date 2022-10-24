module Main
  ( main
  ) where

import Data.Aeson.Text (encodeToLazyText)

import SwaggerSchema (webIdeOpenApi)

main :: IO ()
main = putText . toText . encodeToLazyText $ webIdeOpenApi