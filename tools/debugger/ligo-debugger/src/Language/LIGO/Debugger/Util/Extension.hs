module Language.LIGO.Debugger.Util.Extension
  ( ElimExt (..)
  , Lang (..)
  , UnsupportedExtension (..)
  , extGlobs
  , getExt
  , isLigoFile
  , onExt
  , supportedExtensions
  ) where

import Control.Monad.Except (MonadError (throwError))
import Data.String.Interpolate (i)
import System.FilePath

import Language.LIGO.Debugger.Util.AST.Skeleton (Lang (..))

data ElimExt a = ElimExt
  { eeCaml :: a
  , eeJs   :: a
  }

newtype UnsupportedExtension = UnsupportedExtension String
  deriving stock Show

instance Exception UnsupportedExtension where
  displayException (UnsupportedExtension ext) =
    [i|Unsupported extension has been met: "#{ext}"|]

-- TODO: 'lsp' uses the 'Glob' package to deal with globs, but it doesn't
-- support braced globs such as "{,m,re}ligo" even though the LSP spec allows
-- it. Because of this, we return multiple globs instead of one single glob.
extGlobs :: [Text]
extGlobs = toText . (("**" </>) . ("*" <>)) <$> supportedExtensions

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
