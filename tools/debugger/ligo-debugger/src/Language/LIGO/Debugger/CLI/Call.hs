module Language.LIGO.Debugger.CLI.Call
  ( compileLigoContractDebug
  , compileLigoExpression
  , getAvailableEntrypoints
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (AsPrimitive (_String), key)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as T
import Data.Typeable (cast)
import Fmt (pretty)
import Text.Interpolation.Nyan
import UnliftIO.Exception (handle)

import Cli
  (HasLigoClient, LigoClientFailureException (..), SomeLigoException (SomeLigoException),
  callLigoImplBS)

import Morley.Michelson.Parser qualified as MP
import Morley.Michelson.Untyped qualified as MU

import Language.LIGO.Debugger.CLI.Types

mapLeftM :: (Monad m) => (e -> e') -> Either e a -> m (Either e' a)
mapLeftM f = pure . first f

withLigoException :: (HasLigoClient m) => m (Either LigoException a) -> m (Either LigoException a)
withLigoException = handle \sle@(SomeLigoException e) -> do
  case cast @_ @LigoClientFailureException e of
    Just LigoClientFailureException{..} -> do
      case Aeson.decodeStrict $ T.encodeUtf8 cfeStderr of
        Just decoded -> pure $ Left decoded
        Nothing -> pure $ Left [int||#{displayException sle}|]
    Nothing -> pure $ Left [int||#{displayException sle}|]

-- | Run ligo to compile the contract with all the necessary debug info.
compileLigoContractDebug :: (HasLigoClient m) => String -> FilePath -> m (Either LigoException LigoMapper)
compileLigoContractDebug entrypoint file = withLigoException $
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
    >>= mapLeftM [int|m|Unexpected output of `ligo` from decoding source mapper: #{toText}|] . decodeOutput
  where
    decodeOutput :: LBS.ByteString -> Either String LigoMapper
    decodeOutput bs =
      maybe (Left "Ligo compile contract: json-output parse error")
            Aeson.eitherDecode
            (LBS.fromStrict . T.encodeUtf8 <$> (bs ^? key "json_code" . _String))

-- | Run ligo to compile expression into Michelson in the context of the
-- given file.
compileLigoExpression :: (HasLigoClient m)
                      => MP.MichelsonSource -> FilePath -> Text -> m (Either LigoException MU.Value)
compileLigoExpression valueOrigin ctxFile expr = withLigoException $
  callLigoImplBS Nothing
    [ "compile", "expression"
    , "--no-warn"
    , "--display-format", "json"
    , "--init-file", ctxFile
    , "auto"  -- `syntax` argument, we can leave `auto` since context file is specified
    , toString expr
    ] Nothing
    >>= mapLeftM [int|m|Unexpected output of `ligo` from parsing Michelson value: #{id}|] . decodeOutput
  where
    decodeOutput :: LBS.ByteString -> Either Text MU.Value
    decodeOutput bs =
      maybe (Left "Ligo compile expression: json-output parse error")
      (first pretty . MP.parseExpandValue valueOrigin . decodeUtf8)
      (LBS.fromStrict . T.encodeUtf8 <$> (bs ^? key "text_code" . _String))

getAvailableEntrypoints :: (HasLigoClient m)
                        => FilePath -> m (Either LigoException EntrypointsList)
getAvailableEntrypoints file = withLigoException $
  callLigoImplBS Nothing
    [ "info", "list-declarations"
    , "--display-format", "json"
    , "--only-ep"
    , file
    ] Nothing
    >>= mapLeftM [int|m|Unexpected output of `ligo` from decoding list declarations #{toText}|] . Aeson.eitherDecode
