module Language.LIGO.Debugger.CLI.Call
  ( compileLigoContractDebug
  , compileLigoExpression
  , getAvailableEntrypoints

    -- * Versions
  , LSP.Version (..)

  , getLigoVersion
  , parseLigoVersion
  , VersionSupport (..)
  , isSupportedVersion
  , minimalSupportedVersion
  , recommendedVersion
  , mentionVersionIssues
  ) where

import Data.Aeson qualified as Aeson
import Data.SemVer qualified as SemVer
import Data.Text qualified as T
import Fmt (Buildable, build, pretty)
import System.FilePath ((</>))
import Text.Interpolation.Nyan
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (fromEither, mapExceptionM, throwIO)

import Cli (HasLigoClient, LigoClientFailureException (..), callLigo, callLigoBS)
import Cli qualified as LSP

import Morley.Michelson.Parser qualified as MP
import Morley.Michelson.Untyped qualified as MU

import Language.LIGO.Debugger.CLI.Types
import Util

withMapLigoExc :: (MonadUnliftIO m) => m a -> m a
withMapLigoExc = mapExceptionM \(e :: LigoClientFailureException) ->
  [int||#{cfeStderr e}|] :: LigoCallException

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
compileLigoContractDebug :: forall m. (HasLigoClient m) => String -> FilePath -> m (LigoMapper 'Unique)
compileLigoContractDebug entrypoint file = withMapLigoExc $
  callLigoBS Nothing
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
    >>= either (throwIO . LigoDecodeException "decoding source mapper" . toText) pure
      . Aeson.eitherDecode

-- | Run ligo to compile expression into Michelson in the context of the
-- given file.
compileLigoExpression :: forall m. (HasLigoClient m)
                      => MP.MichelsonSource -> FilePath -> Text -> m MU.Value
compileLigoExpression valueOrigin ctxFile expr = withMapLigoExc $
  callLigo Nothing
    [ "compile", "expression"
    , "--no-warn"
    , "--init-file", ctxFile
    , "auto"  -- `syntax` argument, we can leave `auto` since context file is specified
    , toString expr
    ] Nothing
    >>= decodeOutput
  where
    decodeOutput :: Text -> m MU.Value
    decodeOutput txt =
      MP.parseExpandValue valueOrigin txt
        & first (LigoDecodeException "parsing Michelson value" .  pretty)
        & fromEither

getAvailableEntrypoints :: forall m. (HasLigoClient m)
                        => FilePath -> m EntrypointsList
getAvailableEntrypoints file = withMapLigoExc $
  callLigo Nothing
    [ "info", "list-declarations"
    , "--only-ep"
    , file
    ] Nothing
    >>= decodeOutput
  where
    decodeOutput :: Text -> m EntrypointsList
    decodeOutput txt =
      maybe
        do throwIO $ LigoDecodeException "decoding list declarations" txt
        pure
        do parseEntrypointsList txt

-- Versions
----------------------------------------------------------------------------

-- | Run ligo to get the version of executable.
getLigoVersion :: (HasLigoClient m) => m LSP.Version
getLigoVersion = withMapLigoExc LSP.getLigoVersionRaw

parseLigoVersion :: LSP.Version -> Maybe SemVer.Version
parseLigoVersion =
  rightToMaybe . SemVer.fromText . T.strip . LSP.getVersion

-- | Whether a particular version of @ligo@ version we treat as supported.
data VersionSupport
  = VersionSupported
    -- ^ We fully support a version with high level of assurance.
    --
    -- If our tests pass for a particular version, we can treat it as supported.
  | VersionPartiallySupported
    -- ^ This version works for us with exception of some minor corner cases;
    -- or we don't know for sure if this version is supported.
  | VersionUnsupported
    -- ^ We do not provide an adequate support for this version.
  deriving stock (Eq, Show, Enum, Bounded)

-- | @x > y@ means that @x@ assumes better support than @y@.
instance Ord VersionSupport where
  -- writing down this instance manually to avoid invalid
  -- addition of new constructors

  compare = compare `on` \case
    VersionUnsupported -> 0 :: Int
    VersionPartiallySupported -> 1
    VersionSupported -> 2

instance Buildable VersionSupport where
  build = \case
    VersionSupported -> "supported"
    VersionPartiallySupported -> "partially supported"
    VersionUnsupported -> "unsupported"

-- | See how much do we support the provided version of @ligo@.
isSupportedVersion :: SemVer.Version -> VersionSupport
isSupportedVersion ver = fromMaybe VersionSupported $ asum
  -- List of rules to detect an unsupported version.
  --
  -- Rules in `docs/ligo-versions.md` make sure that this function
  -- is kept up-to-date.
  --
  -- You can add custom rules, e.g. the following one excludes one version
  -- because it is buggy:
  --
  -- @
  -- ver == [Data.SemVer.QQ.version|0.1.2|]
  --   ?- noSupport
  -- @
  [
    -- Debug information in the necessary format is not available in old versions
    ver < minimalSupportedVersion
      ?- noSupport

    -- Future versions that we didn't check yet
  , ver > recommendedVersion  -- don't hesitate to replace this with a higher constant
      ?- partialSupport

  ]
  where
    infix 0 ?-
    (?-) :: Bool -> a -> Maybe a
    cond ?- res = guard cond $> res

    noSupport = VersionUnsupported
    partialSupport = VersionPartiallySupported

  -- Implementation note: in case in the future we'll want to provide the users
  -- with the full list of supported versions, we can define rules in terms of
  -- 'Data.SemVer.Constraint.Constraint'.

-- | Minimal version which we at least partially support.
--
-- We extract this to a separate variable only because it is needed
-- in tests.
minimalSupportedVersion :: SemVer.Version
minimalSupportedVersion =
  $$(readSemVerQ $ resourcesFolder </> "versions" </> "minimal-supported")

-- | Version that we suggest the user to use with our debugger.
--
-- For now we assume that debugger may break more often and more badly than LSP,
-- so it has its own recommended version. By experience, it quite possible that
-- LIGO introduces breaking changes that we cannot workaround and that may take
-- a while to fix.
-- When the situation stabilizes, we can get rid of this and assume that we
-- are successful at supporting the latest version.
recommendedVersion :: SemVer.Version
recommendedVersion =
  $$(readSemVerQ $ resourcesFolder </> "versions" </> "recommended")

-- | Update an error so that it mentions issues with ligo version being
-- unsupported in case any such issues take place.
mentionVersionIssues :: (HasLigoClient m) => Text -> m Text
mentionVersionIssues exc = do
  mVer <- parseLigoVersion <$> getLigoVersion
  let verNote = case mVer of
        Nothing -> Just [int|n|
          You seem to be using not a stable release of ligo,
          consider trying #semv{recommendedVersion}.
          |]
        Just ver -> case isSupportedVersion ver of
          -- We should never get this as @unsupported@ case is checked
          -- at startup, but putting some message here nevertheless just in case
          VersionUnsupported -> Just [int|n|
            Note that the current version of ligo #semv{ver} is not supported!
            |]
          VersionPartiallySupported -> Just [int|n|
            Note that the current ligo version #semv{ver} is not guaranteed to work
            correctly with debugger.

            If you need debugging capabilities, you might try ligo of
            #semv{recommendedVersion} version until the new version of
            the extension is released.
            |]
          VersionSupported -> Nothing
  return $
    maybe id (\note' e -> e <> "\n" <> note') verNote exc
