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

data ElimExt a = ElimExt
  { eeCaml :: a
  , eeJs   :: a
  }

newtype UnsupportedExtension = UnsupportedExtension String
  deriving stock Show

instance Exception UnsupportedExtension where
  displayException (UnsupportedExtension ext) =
    [int||Unsupported extension has been met: "#{ext}"|]

getExt :: MonadError UnsupportedExtension m => FilePath -> m Lang
getExt path =
  case takeExtension path of
    ".mligo"  -> return Caml
    ".jsligo" -> return Js
    ext       -> throwError $ UnsupportedExtension ext

isLigoFile :: FilePath -> Bool
isLigoFile = isRight . getExt

onExt :: MonadError UnsupportedExtension m => ElimExt a -> FilePath -> m a
onExt ee path =
  getExt path <&> \case
    Caml -> eeCaml   ee
    Js   -> eeJs     ee

supportedExtensions :: [FilePath]
supportedExtensions = [".mligo", ".jsligo"]
