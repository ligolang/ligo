{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LIGO.Debugger.CLI.Exception
  ( LigoException
  , SomeLigoException (..)
  , LigoErrorNodeParseErrorException  (..)
  , LigoClientFailureException (..)
  , LigoDecodedExpectedClientFailureException (..)
  , LigoIOException (..)
  , LigoMalformedJSONException (..)
  , LigoPreprocessFailedException (..)
  , LigoDecodeException (..)
  , ConfigurationException (..)
  , LigoCallException (..)
  , PluginCommunicationException (..)
  ) where

import Data.Default (Default (def))
import Data.Text qualified as Text
import Fmt.Buildable (Buildable, FromDoc, build, pretty)
import GHC.IO.Exception qualified as IOException
import System.Exit (ExitCode)
import Text.Interpolation.Nyan hiding (rmode')
import Util

import Language.LIGO.Debugger.CLI.Types

class Exception a => LigoException a where

-- | Catch ligo failure to be able to restore from it
data LigoClientFailureException = LigoClientFailureException
  { cfeStdout :: Text -- ^ stdout
  , cfeStderr :: Text -- ^ stderr
  , cfeFile   :: Maybe FilePath  -- ^ File that caused the error
  , cfeExit   :: Maybe ExitCode  -- ^ The exit code that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | Expected ligo failure decoded from its JSON output
data LigoDecodedExpectedClientFailureException = LigoDecodedExpectedClientFailureException
  { decfeErrorsDecoded :: NonEmpty LigoError -- ^ Successfully decoded ligo errors
  , decfeWarningsDecoded :: [LigoError]
  , decfeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

data LigoPreprocessFailedException = LigoPreprocessFailedException
  { pfeMessage :: Text -- ^ Successfully decoded ligo error
  , pfeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | Starting @ligo@ executable failed - some system error.
--
-- Likely LIGO isn't installed or was not found.
data LigoIOException = LigoIOException
  { lieType :: IOException.IOErrorType
  , lieDescription :: Text
  } deriving anyclass (LigoException)
    deriving stock (Show)

----------------------------------------------------------------------------
-- Errors that may fail due to changes in ligo compiler
----------------------------------------------------------------------------

-- | Parse error occured during ligo output JSON decoding.
data LigoErrorNodeParseErrorException = LigoErrorNodeParseErrorException
  { lnpeError :: Text -- ^ Error description
  , lnpeOutput :: Text -- ^ The JSON output which we failed to decode
  , lnpeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | Parse error occured during ligo stderr JSON decoding.
data LigoDefinitionParseErrorException = LigoDefinitionParseErrorException
  { ldpeError :: Text -- ^ Error description
  , ldpeOutput :: Text -- ^ The JSON output which we failed to decode
  , ldpeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | LIGO produced a malformed JSON that can't be encoded into a Value. This is
-- a bug in the compiler that must be reported to the LIGO team.
data LigoMalformedJSONException = LigoMalformedJSONException
  { lmjeError :: String -- ^ Error description
  , lmjeOutput :: Text -- ^ The JSON output which we failed to decode
  , lmjeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

data SomeLigoException where
  SomeLigoException :: LigoException a => a -> SomeLigoException

deriving stock instance Show SomeLigoException

instance Exception SomeLigoException where
  displayException (SomeLigoException a) = [int||Error (LIGO): #{displayException a}|]

  fromException e =
    asum
      [ SomeLigoException <$> fromException @LigoClientFailureException                e
      , SomeLigoException <$> fromException @LigoDecodedExpectedClientFailureException e
      , SomeLigoException <$> fromException @LigoErrorNodeParseErrorException          e
      , SomeLigoException <$> fromException @LigoMalformedJSONException                e
      , SomeLigoException <$> fromException @LigoDefinitionParseErrorException         e
      , SomeLigoException <$> fromException @LigoPreprocessFailedException             e
      , SomeLigoException <$> fromException @LigoIOException                           e
      ]

instance Exception LigoClientFailureException where
  displayException LigoClientFailureException {..} =
    [int||LIGO binary failed with
    #{stdOut}
    #{stdErr}
    #{causedBy}|]
    where
      causedBy = maybe ("" :: String) (\file -> [int||Caused by: #{file}|]) cfeFile

      stdOut
        | Text.null cfeStdout = "" :: String
        | otherwise = [int||Stdout: #{cfeStdout}|]

      stdErr
        | Text.null cfeStderr = "" :: String
        | otherwise = [int||Stderr: #{cfeStderr}|]

instance Exception LigoDecodedExpectedClientFailureException where
  displayException LigoDecodedExpectedClientFailureException {..} =
    [int||LIGO binary produced expected error which we successfully decoded as:
    #{toList decfeErrorsDecoded}
    With warnings
    #{toList decfeWarningsDecoded}
    Caused by: #{decfeFile}|]

instance Exception LigoErrorNodeParseErrorException where
  displayException LigoErrorNodeParseErrorException {..} =
    [int||LIGO binary produced an error JSON which we were unable to decode:
    #{lnpeError}
    Caused by: #{lnpeFile}
    JSON output dumped:
    #{lnpeOutput}|]

instance Exception LigoMalformedJSONException where
  displayException LigoMalformedJSONException {..} =
    [int||LIGO binary produced a malformed JSON:
    #{lmjeError}
    Caused by: #{lmjeFile}
    JSON output dumped:
    #{lmjeOutput}|]

instance Exception LigoDefinitionParseErrorException where
  displayException LigoDefinitionParseErrorException {..} =
    [int||LIGO binary produced a definition output which we consider malformed:
    #{ldpeError}
    Caused by: #{ldpeFile}
    JSON output dumped:
    #{ldpeOutput}|]

instance Exception LigoPreprocessFailedException where
  displayException LigoPreprocessFailedException {..} =
    [int||LIGO failed to preprocess contract with: #{pfeMessage}
    Caused by: #{pfeFile}|]

instance Exception LigoIOException where
  displayException LigoIOException {..} =
    [int||LIGO executable run failed: #{lieDescription}|]

----------------------------------------------------------------------------
-- Debugger exceptions
----------------------------------------------------------------------------

-- | Failed to decode LIGO output.
data LigoDecodeException = LigoDecodeException
  { ldeSource :: Text
    -- ^ What we were trying to decode.
  , ldeMessage :: Text
    -- ^ The error message.
  } deriving stock (Show)

-- | Invalid debugger configuration provided.
newtype ConfigurationException = ConfigurationException Text
  deriving newtype (Show, Buildable, FromDoc)

-- | Something got wrong on @ligo@ executable's side.
newtype LigoCallException = LigoCallException { leMessage :: Text }
  deriving newtype (Eq, Show, FromDoc)

-- | Some unexpected error in communication with the plugin.
newtype PluginCommunicationException = PluginCommunicationException Text
  deriving newtype (Show, Buildable, FromDoc)

instance Default LigoCallException where
  def = LigoCallException ""

instance Buildable LigoCallException where
  build LigoCallException{..} = build $ replaceANSI leMessage

instance Exception LigoCallException where
  displayException = pretty

instance Buildable LigoDecodeException where
  build LigoDecodeException{..} =
    [int||Failed to parse LIGO output (#{ldeSource}): #{ldeMessage}|]

instance Exception LigoDecodeException where
  displayException = pretty

instance Exception ConfigurationException where
  displayException (ConfigurationException msg) = pretty msg

instance Exception PluginCommunicationException where
  displayException = pretty
