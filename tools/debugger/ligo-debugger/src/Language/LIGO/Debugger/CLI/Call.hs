module Language.LIGO.Debugger.CLI.Call
  ( compileLigoContractDebug
  , compileLigoExpression
  , getAvailableEntrypoints
  ) where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (AsPrimitive (_String), key)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as T
import Text.Interpolation.Nyan
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (fromEither, mapExceptionM, throwIO)

import Cli (HasLigoClient, LigoClientFailureException (..), callLigoImplBS)

import Morley.Michelson.Parser qualified as MP
import Morley.Michelson.Untyped qualified as MU

import Language.LIGO.Debugger.CLI.Types

withMapLigoExc :: (MonadUnliftIO m) => m a -> m a
withMapLigoExc = mapExceptionM \(e :: LigoClientFailureException) ->
  case Aeson.decodeStrict $ T.encodeUtf8 $ cfeStderr e of
    Nothing -> [int||#{displayException e}|]
    Just (decoded :: LigoException) -> decoded

-- | This function tries to decode @ligo@ compiler's output.
-- If it fails, then it would throw @LigoException@.
decodeBytes :: (MonadIO m, FromJSON a) => (String -> LigoException) -> LBS.ByteString -> m a
decodeBytes mkExc bytes = case Aeson.decode bytes of
  Just res -> pure res
  Nothing -> case Aeson.eitherDecode @LigoException bytes of
    Right exc -> throwIO exc
    Left err -> throwIO $ mkExc err

-- | Run ligo to compile the contract with all the necessary debug info.
compileLigoContractDebug :: forall m. (HasLigoClient m) => String -> FilePath -> m LigoMapper
compileLigoContractDebug entrypoint file = withMapLigoExc $
  callLigoImplBS Nothing
    [ "compile", "contract"
    , "--no-warn"
    , "--display-format", "json"
    , "--michelson-format", "json"
    , "--michelson-comments", "location"
    , "--michelson-comments", "env"
    , "-e", entrypoint
    , "--experimental-disable-optimizations-for-debugging"
    , "--disable-michelson-typechecking"
    , file
    ] Nothing
    >>= decodeOutput
  where
    decodeOutput :: LBS.ByteString -> m LigoMapper
    decodeOutput bs = do
      body <-
        maybe
          (throwIO @_ @LigoException [int||Ligo compile contract: json-output parse error|])
          pure
          bodyMb
      decodeBytes [int|m|Unexpected output of `ligo` from decoding source mapper: #{id}|] body
      where
        bodyMb = LBS.fromStrict . T.encodeUtf8 <$> bs ^? key "json_code" . _String

-- | Run ligo to compile expression into Michelson in the context of the
-- given file.
compileLigoExpression :: forall m. (HasLigoClient m)
                      => MP.MichelsonSource -> FilePath -> Text -> m MU.Value
compileLigoExpression valueOrigin ctxFile expr = withMapLigoExc $
  callLigoImplBS Nothing
    [ "compile", "expression"
    , "--no-warn"
    , "--display-format", "json"
    , "--init-file", ctxFile
    , "auto"  -- `syntax` argument, we can leave `auto` since context file is specified
    , toString expr
    ] Nothing
    >>= decodeOutput
  where
    decodeOutput :: LBS.ByteString -> m MU.Value
    decodeOutput bs = do
      body <-
        maybe
          (throwIO @_ @LigoException [int||Ligo compile expression: json-output parse error|])
          pure
          bodyMb

      let parsedValue = first
            [int|m|Unexpected output of `ligo` from parsing Michelson value: #{id}|]
            do MP.parseExpandValue valueOrigin body

      fromEither @LigoException parsedValue
      where
        bodyMb = bs ^? key "text_code" . _String

getAvailableEntrypoints :: forall m. (HasLigoClient m)
                        => FilePath -> m EntrypointsList
getAvailableEntrypoints file = withMapLigoExc $
  callLigoImplBS Nothing
    [ "info", "list-declarations"
    , "--display-format", "json"
    , "--only-ep"
    , file
    ] Nothing
    >>= decodeBytes [int|m|Unexpected output of `ligo` from decoding list declarations #{id}|]
