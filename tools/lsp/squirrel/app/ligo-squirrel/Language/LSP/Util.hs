-- | Utilities for working with LSP.
module Language.LSP.Util
  ( sendError
  , sendWarning
  , reverseUriMap
  ) where

import Control.Lens (dimap)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Language.LSP.Types qualified as J
import Language.LSP.Server qualified as S

-- | Ask the LSP client to display an error to the user.
sendError :: S.MonadLsp config m => Text -> m ()
sendError = S.sendNotification J.SWindowShowMessage . J.ShowMessageParams J.MtError

-- | Ask the LSP client to display an error to the user.
sendWarning :: S.MonadLsp config m => Text -> m ()
sendWarning = S.sendNotification J.SWindowShowMessage . J.ShowMessageParams J.MtWarning

-- | Like 'S.reverseFilePath', but applied to 'J.NormalizedUri'.
reverseUriMap :: S.MonadLsp config m => m (J.NormalizedUri -> J.NormalizedUri)
reverseUriMap = dimap normalizedUriToFilePath filePathToNormalizedUri <$> S.reverseFileMap
  where
    normalizedUriToFilePath = fromJust . J.uriToFilePath . J.fromNormalizedUri
    filePathToNormalizedUri = J.toNormalizedUri . J.filePathToUri
