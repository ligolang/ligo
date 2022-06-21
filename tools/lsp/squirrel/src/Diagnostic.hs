module Diagnostic
  ( Message (..)
  , MessageDetail (..)
  , Severity (..)
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Duplo.Pretty (Pretty (..))

import Range (Range)

-- | Represents some diagnostic (error, warning, etc) that may be contained
-- together with some node.
--
-- Note that this is different from @Error@, which is a node by itself, and not
-- something extra that is associated with some node.
data Message = Message
  { mMessage :: MessageDetail
  , mSeverity :: Severity
  , mRange :: Range
  } deriving stock (Eq, Ord, Show)

data MessageDetail
  = FromLanguageServer Text
  | FromLIGO Text
  | MissingContract FilePath
  | Unexpected Text
  | Unrecognized Text
  deriving stock (Eq, Ord, Show)

instance Pretty MessageDetail where
  pp (FromLanguageServer msg) = pp msg
  pp (FromLIGO msg) = pp msg
  pp (MissingContract path) = "Missing contract: " <> pp (Text.pack path)
  pp (Unexpected src) = "Unexpected: " <> pp src
  pp (Unrecognized src) = "Unrecognized: " <> pp src

data Severity
  = SeverityError
  | SeverityWarning
  deriving stock (Eq, Ord, Show)
