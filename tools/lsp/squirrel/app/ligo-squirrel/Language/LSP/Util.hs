-- | Utilities for working with LSP.
module Language.LSP.Util
  ( sendError
  , logShowId
  ) where

import Data.Functor (($>))
import Data.Text (Text)
import qualified Language.LSP.Types as J
import qualified Language.LSP.Server as S

import System.IO.Unsafe

import qualified Log

-- | Unsafely log some message to the output.
-- This is made to mimic `Debug.Trace.traceShowId`'s behavior.
-- TODO: See LIGO-187.
logShowId :: Show a => a -> a
logShowId a = unsafePerformIO (Log.debug "DEBUG" (show a) $> a)

-- | Ask the LSP client to display an error to the user.
sendError :: S.MonadLsp config m => Text -> m ()
sendError = S.sendNotification J.SWindowShowMessage . J.ShowMessageParams J.MtError
