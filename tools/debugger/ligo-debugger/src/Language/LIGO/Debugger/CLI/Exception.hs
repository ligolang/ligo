{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LIGO.Debugger.CLI.Exception
  ( LigoException
  , SomeLigoException (..)
  , LigoErrorNodeParseErrorException  (..)
  , LigoClientFailureException (..)
  , LigoIOException (..)
  , LigoJSONException (..)
  , LigoJSONExceptionContent (..)
  , LigoMalformedJSONException (..)
  , LigoDecodeException (..)
  , ConfigurationException (..)
  , LigoCallException (..)
  , LigoResolveConfigException (..)
  , PluginCommunicationException (..)
  ) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Default (Default (def))
import Data.Text qualified as Text
import Fmt.Buildable (Buildable, FromDoc, build, pretty)
import GHC.IO.Exception qualified as IOException
import System.Exit (ExitCode)
import Text.Interpolation.Nyan hiding (rmode')
import Util

import Language.LIGO.Debugger.CLI.Types

-- | A subclass for exceptions that come from LIGO binary.
class Exception a => LigoException a where

-- | Catch ligo failure to be able to restore from it
data LigoClientFailureException = LigoClientFailureException
  { cfeStdout :: Text -- ^ stdout
  , cfeStderr :: Text -- ^ stderr
  , cfeFile   :: Maybe FilePath  -- ^ File that caused the error
  , cfeExit   :: Maybe ExitCode  -- ^ The exit code that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | Starting @ligo@ executable failed - some system error.
--
-- Likely LIGO isn't installed or was not found.
data LigoIOException = LigoIOException
  { lieType :: IOException.IOErrorType -- ^ An error type.
  , lieDescription :: Text -- ^ An error message.
  } deriving anyclass (LigoException)
    deriving stock (Show)

----------------------------------------------------------------------------
-- JSON helper exception
----------------------------------------------------------------------------

-- | Arbitrary exception from the @LIGO@ binary
-- in JSON format.
data LigoJSONException = LigoJSONException
  { ljeStatus :: Text -- ^ A status of exception.
  , ljeStage :: Text -- ^ Where LIGO exception occurred.
  , ljeContent :: LigoJSONExceptionContent -- ^ Exception's contents.
  } deriving stock (Show, Generic)

-- | Contents of @LigoJSONException@.
data LigoJSONExceptionContent = LigoJSONExceptionContent
  { ljecMessage :: Text -- ^ An exception message.
  , ljecLocation :: Maybe LigoRange -- ^ A possible location of the exception.
  , ljecChildren :: Maybe LigoJSONException -- ^ Possible children of the exception.
  } deriving stock (Show, Generic)

instance FromJSON LigoJSONException where
  parseJSON = withObject "LigoJSONException" \o -> do
    ljeStatus <- o .: "status"
    ljeStage <- o .: "stage"
    ljeContent <- o .: "content"
    pure LigoJSONException{..}

instance FromJSON LigoJSONExceptionContent where
  parseJSON = withObject "LigoJSONExceptionContent" \o -> do
    ljecMessage <- o .: "message"
    ljecLocation <- o .:? "location"
    ljecChildren <- o .:? "children"
    pure LigoJSONExceptionContent{..}

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

-- | An existential wrapper for @LigoException@s.
data SomeLigoException where
  SomeLigoException :: LigoException a => a -> SomeLigoException

deriving stock instance Show SomeLigoException

instance Exception SomeLigoException where
  displayException (SomeLigoException a) = [int||Error (LIGO): #{displayException a}|]

  fromException e =
    asum
      [ SomeLigoException <$> fromException @LigoClientFailureException                e
      , SomeLigoException <$> fromException @LigoErrorNodeParseErrorException          e
      , SomeLigoException <$> fromException @LigoMalformedJSONException                e
      , SomeLigoException <$> fromException @LigoDefinitionParseErrorException         e
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

-- | Something went wrong in the debugger's configuration
-- written in @LIGO@.
newtype LigoResolveConfigException = LigoResolveConfigException { lrceMessage :: Text }
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

instance Buildable LigoResolveConfigException where
  build LigoResolveConfigException{..} = build lrceMessage

instance Exception LigoResolveConfigException where
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
