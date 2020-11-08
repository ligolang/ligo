module Extension
  ( ElimExt (..)
  , Lang (..)
  , UnsupportedExtension (..)
  , getExt
  , onExt
  ) where

import Control.Monad.Catch
import System.FilePath

import AST.Skeleton (Lang (..))

data ElimExt a = ElimExt
  { eePascal :: a
  , eeCaml   :: a
  , eeReason ::  a
  }

data UnsupportedExtension = UnsupportedExtension String
  deriving stock Show
  deriving anyclass Exception

getExt :: MonadThrow m => FilePath -> m Lang
getExt path = do
  case takeExtension path of
    ".religo" -> return Reason
    ".ligo"   -> return Pascal
    ".mligo"  -> return Caml
    ext      -> throwM $ UnsupportedExtension ext

onExt :: ElimExt a -> FilePath -> IO a
onExt ee path = do
  getExt path >>= return . \case
    Pascal -> eePascal ee
    Caml   -> eeCaml   ee
    Reason -> eeReason ee
