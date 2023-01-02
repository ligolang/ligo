{-# LANGUAGE StandaloneKindSignatures #-}

module Error
  ( ErrorKind (..)
  , ServerLikeError (..)
  , LigoCompilerError (..)
  , MorleyError (..)
  , GistError (..)

  , notFoundFormatter
  , convertToServerError
  , jsonError
  , customFormatters
  ) where

import Control.Monad.Catch (Handler(..), catches)
import Control.Monad.Except (MonadError)
import Data.Aeson qualified as A
import Fmt ((+|), (|+))
import Network.Wai.Internal (rawPathInfo)
import Servant
  (ErrorFormatter, ErrorFormatters(..), NotFoundErrorFormatter, ServerError(..),
  defaultErrorFormatters, err400, err404, err500, throwError)

data ErrorKind
  = ServerInternal
    -- ^ Something went wrong on server side.
  | ApplicationInternal
    -- ^ Something went wrong on server-client side,
    -- e.g. client passed invalid data.
  | NormalError
    -- ^ An error that should be propagated to the user.

jsonError :: ServerError -> ErrorKind -> Text -> Text -> ServerError
jsonError baseErr kind errorType reason =
  baseErr
  { errBody = A.encode $ A.object
      [ "type" A..= errorType
      , "reason" A..= reason
      , "showToUser" A..= case kind of NormalError -> True; _ -> False
      ]
  , errHeaders = [("Content-Type", "application/json")]
  }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req = jsonError err404 ApplicationInternal "NotFound" $ "Not found path: " +| decodeUtf8 @Text @ByteString (rawPathInfo req) |+ ""

customFormatter :: ErrorFormatter
customFormatter _ _ err = jsonError err400 ApplicationInternal "WrongRequest" $ "Wrong request: " +| (show err :: String) |+ ""

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { notFoundErrorFormatter = notFoundFormatter
  , bodyParserErrorFormatter = customFormatter
  , urlParseErrorFormatter = customFormatter
  , headerParseErrorFormatter = customFormatter
  }

class Exception e => ServerLikeError e where
  toServerError :: e -> ServerError

type MkHandlers :: [Type] -> Constraint
class MkHandlers es where
  mkHandlers :: MonadError ServerError m => [Handler m a]

instance MkHandlers '[] where
  mkHandlers = []

instance (MkHandlers xs, ServerLikeError x) => MkHandlers (x ': xs) where
  mkHandlers = Handler (throwError . toServerError @x) : mkHandlers @xs

convertToServerError :: forall es m a. (MkHandlers es, MonadCatch m, MonadError ServerError m) => m a -> m a
convertToServerError action = action `catches` mkHandlers @es

data LigoCompilerError
  = LigoCompilerError Text
  | WrongMainFileExtension FilePath
  | NoLigoBinary
  deriving stock Show
  deriving anyclass Exception

instance ServerLikeError LigoCompilerError where
  toServerError = \case
    LigoCompilerError text ->
      jsonError err400 NormalError "LigoCompilerError" $ "Ligo compiler error:" +| text |+ ""
    WrongMainFileExtension fp ->
      jsonError err400 NormalError "WrongMainFileExtension" $ "Couldn't infer dialect from filetype" +| fp |+ ""
    NoLigoBinary ->
      jsonError err500 ServerInternal "NoLigoBinary" "Server doesn't have access to LIGO binary"

data MorleyError
  = MorleyError Text
  | MorleyStorageParsingError Text
  deriving stock Show
  deriving anyclass Exception

instance ServerLikeError MorleyError where
  toServerError = \case
    MorleyError text ->
      jsonError err400 NormalError "MorleyError" $ "Morley error:" +| text |+ ""
    MorleyStorageParsingError text ->
      jsonError err400 NormalError "MorleyStorageParsingError" $ "Morley can't parse storage:" +| text |+ ""

newtype GistError = GistError Text
  deriving stock Show
  deriving anyclass Exception

instance ServerLikeError GistError where
  toServerError = \case
    GistError text -> jsonError err400 NormalError "GistError" $ "Gist error:" +| text |+ ""

instance ServerLikeError SomeException where
  toServerError e = jsonError err500 ServerInternal "Server internal error" (toText $ displayException e)


