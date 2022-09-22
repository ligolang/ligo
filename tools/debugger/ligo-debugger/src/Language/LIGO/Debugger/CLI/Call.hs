module Language.LIGO.Debugger.CLI.Call
  ( compileLigoContractDebug
  , compileLigoExpression
  , getAvailableEntrypoints
  ) where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Text.Interpolation.Nyan
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (fromEither, mapExceptionM, throwIO)

import Cli (HasLigoClient, LigoClientFailureException (..), callLigoImplBS)

import Morley.Michelson.Parser qualified as MP
import Morley.Michelson.Untyped qualified as MU

import Language.LIGO.Debugger.CLI.Types

withMapLigoExc :: (MonadUnliftIO m) => m a -> m a
withMapLigoExc = mapExceptionM \(e :: LigoClientFailureException) ->
  [int||#{cfeStderr e}|] :: LigoException

-- | This function tries to decode @ligo@ compiler's output.
-- If it fails, then it would throw @LigoException@.
decodeBytes :: (MonadIO m, FromJSON a) => (String -> LigoException) -> LBS.ByteString -> m a
decodeBytes mkExc bytes = case Aeson.decode bytes of
  Just res -> pure res
  Nothing -> throwIO $ mkExc $ decodeUtf8 bytes

{-
  Here and in the next calling @ligo@ binary functions
  we don't use '--format / --display-format json' flags.

  It's because we don't want to support @json@-schemas
  for @ligo@ errors. They look complex and it's
  not obvious how to extract useful info from them.
  Moreover, one day they can change this format
  and it would be painful to resolve it on our side.
-}

-- | Run ligo to compile the contract with all the necessary debug info.
compileLigoContractDebug :: forall m. (HasLigoClient m) => String -> FilePath -> m LigoMapper
compileLigoContractDebug entrypoint file = withMapLigoExc $
  callLigoImplBS Nothing
    [ "compile", "contract"
    , "--no-warn"
    , "--michelson-format", "json"
    , "--michelson-comments", "location"
    , "--michelson-comments", "env"
    , "-e", entrypoint
    , "--experimental-disable-optimizations-for-debugging"
    , "--disable-michelson-typechecking"
    , file
    ] Nothing
    >>= decodeBytes [int|m|Unexpected output of `ligo` from decoding source mapper: #{id}|]

-- | Run ligo to compile expression into Michelson in the context of the
-- given file.
compileLigoExpression :: forall m. (HasLigoClient m)
                      => MP.MichelsonSource -> FilePath -> Text -> m MU.Value
compileLigoExpression valueOrigin ctxFile expr = withMapLigoExc $
  callLigoImplBS Nothing
    [ "compile", "expression"
    , "--no-warn"
    , "--init-file", ctxFile
    , "auto"  -- `syntax` argument, we can leave `auto` since context file is specified
    , toString expr
    ] Nothing
    >>= decodeOutput
  where
    decodeOutput :: LBS.ByteString -> m MU.Value
    decodeOutput bs = do
      let parsedValue = first
            [int|m|Unexpected output of `ligo` from parsing Michelson value: #{id}|]
            do MP.parseExpandValue valueOrigin (decodeUtf8 bs)

      fromEither @LigoException parsedValue

getAvailableEntrypoints :: forall m. (HasLigoClient m)
                        => FilePath -> m EntrypointsList
getAvailableEntrypoints file = withMapLigoExc $
  callLigoImplBS Nothing
    [ "info", "list-declarations"
    , "--only-ep"
    , file
    ] Nothing
    >>= decodeOutput
  where
    decodeOutput :: LBS.ByteString -> m EntrypointsList
    decodeOutput bs = do
      maybe
        do throwIO @_ @LigoException
            [int||Unexpected output of `ligo` from \
            decoding list declarations #{decodeUtf8 @Text bs}|]
        pure
        do parseEntrypointsList $ decodeUtf8 bs
