module Language.LIGO.Extension
  ( ElimExt (..)
  , Lang (..)
  , UnsupportedExtension (..)
  , getExt
  , isLigoFile
  , onExt
  , supportedExtensions
  ) where

import Control.Monad.Except (MonadError (throwError))
import System.FilePath
import Text.Interpolation.Nyan

import Language.LIGO.AST.Skeleton (Lang (..))

-- | A convenient wrapper that stores actions
-- for each LIGO extension.
data ElimExt a = ElimExt
  { eeCaml :: a -- ^ action on @CameLIGO@.
  , eeJs   :: a -- ^ action on @JsLIGO@.
  }

-- | An exception which is thrown when a given file has
-- unsupported extension.
newtype UnsupportedExtension = UnsupportedExtension String
  deriving stock Show

instance Exception UnsupportedExtension where
  displayException (UnsupportedExtension ext) =
    [int||Unsupported extension has been met: "#{ext}"|]

-- | Gets a LIGO extension from file path.
-- Throws @UnsupportedExtension@ if the extension is unknown.
getExt :: MonadError UnsupportedExtension m => FilePath -> m Lang
getExt path =
  case takeExtension path of
    ".mligo"  -> return Caml
    ".jsligo" -> return Js
    ext       -> throwError $ UnsupportedExtension ext

-- | Determines if the file has a known LIGO extension.
isLigoFile :: FilePath -> Bool
isLigoFile = isRight . getExt

-- | Runs an action on the corresponding extension.
-- Throws @UnsupportedExtension@ if the extension is unknown.
onExt :: MonadError UnsupportedExtension m => ElimExt a -> FilePath -> m a
onExt ee path =
  getExt path <&> \case
    Caml -> eeCaml   ee
    Js   -> eeJs     ee

-- | A list of supported LIGO file extensions.
supportedExtensions :: [FilePath]
supportedExtensions = [".mligo", ".jsligo"]
