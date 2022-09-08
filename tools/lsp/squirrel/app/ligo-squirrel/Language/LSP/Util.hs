-- | Utilities for working with LSP.
module Language.LSP.Util
  ( sendError
  , sendWarning
  , sendInfo
  , reverseUriMap
  , filePathToNormalizedUri
  ) where

import Control.Lens (dimap)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Language.LSP.Types qualified as J
import Language.LSP.Server qualified as S

sendMsg :: S.MonadLsp config m => J.MessageType -> Text -> m ()
sendMsg msg = S.sendNotification J.SWindowShowMessage . J.ShowMessageParams msg

sendError, sendWarning, sendInfo :: S.MonadLsp config m => Text -> m ()
-- | Ask the LSP client to display an error to the user.
sendError = sendMsg J.MtError
-- | Ask the LSP client to display a warning to the user.
sendWarning = sendMsg J.MtWarning
-- | Ask the LSP client to display an information to the user.
sendInfo = sendMsg J.MtInfo

-- | Like 'S.reverseFilePath', but applied to 'J.NormalizedUri'.
reverseUriMap :: S.MonadLsp config m => m (J.NormalizedUri -> J.NormalizedUri)
reverseUriMap = dimap normalizedUriToFilePath filePathToNormalizedUri <$> S.reverseFileMap
  where
    normalizedUriToFilePath = fromJust . J.uriToFilePath . J.fromNormalizedUri

filePathToNormalizedUri :: FilePath -> J.NormalizedUri
filePathToNormalizedUri = J.toNormalizedUri . J.filePathToUri
