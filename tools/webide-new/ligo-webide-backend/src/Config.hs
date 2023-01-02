module Config (Config (..)) where

data Config = Config
  { cLigoPath :: Maybe FilePath
  , cOctezClientPath :: Maybe FilePath
  , cPort :: Int
  , cVerbose :: Bool
  , cDockerizedLigoVersion :: Maybe String
  , cGistToken :: String
  }
