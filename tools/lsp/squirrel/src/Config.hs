{-# LANGUAGE RecordWildCards #-}

-- | This module contains glpbal extension configuration

module Config
  ( Config(..)
  ) where

import Data.Aeson
  (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Default (Default (def))
import Data.Set (Set)
import Language.LSP.Types qualified as J

import Cli (LigoClientEnv (..))

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Global extension configuration
data Config =
  Config
    { _cLigoBinaryPath :: FilePath -- ^ Path to ligo binary
    , _cMaxNumberOfProblems :: Int -- ^ Maximum amount of errors displayed
    , _cDisabledFeatures :: Set J.SomeClientMethod
    -- ^ Features disabled in the language server.
    } deriving stock (Show)

instance Default Config where
  def = Config
    { _cLigoBinaryPath = _lceClientPath def -- Extract ligo binary from $PATH
    , _cMaxNumberOfProblems = 100
    , _cDisabledFeatures = mempty
    }

----------------------------------------------------------------------------
-- JSON
----------------------------------------------------------------------------

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    s <- v .: "ligoLanguageServer"
    flip (withObject "Config.settings") s $ \o -> do
      _cLigoBinaryPath <- o .:? "ligoBinaryPath" .!= _cLigoBinaryPath def
      _cMaxNumberOfProblems <- o .:? "maxNumberOfProblems" .!= _cMaxNumberOfProblems def
      _cDisabledFeatures <- o .:? "disabledFeatures" .!= _cDisabledFeatures def
      pure Config
        { _cLigoBinaryPath = _cLigoBinaryPath
        , _cMaxNumberOfProblems = _cMaxNumberOfProblems
        , _cDisabledFeatures
        }

instance ToJSON Config where
  toJSON Config {..} = object [ "ligoLanguageServer" .= r ]
    where
      r = object
        [ "ligoBinaryPath" .= _cLigoBinaryPath
        , "maxNumberOfProblems" .= _cMaxNumberOfProblems
        , "disabledFeatures" .= _cDisabledFeatures
        ]
