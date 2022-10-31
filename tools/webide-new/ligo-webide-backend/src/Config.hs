module Config (Config (..)) where

data Config = Config
  { cLigoPath :: Maybe FilePath
  , cTezosClientPath :: Maybe FilePath
  , cPort :: Int
  , cVerbose :: Bool
  , cDockerizedLigoVersion :: Maybe String
  }
