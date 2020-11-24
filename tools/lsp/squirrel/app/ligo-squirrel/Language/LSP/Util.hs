-- | Utilities for working with LSP.
module Language.LSP.Util
  ( sendError
  ) where

import Data.Text (Text)
import qualified Language.LSP.Types as J
import qualified Language.LSP.Server as S


-- | Ask the LSP client to display an error to the user.
sendError :: S.MonadLsp config m => Text -> m ()
sendError = S.sendNotification J.SWindowShowMessage . J.ShowMessageParams J.MtError
