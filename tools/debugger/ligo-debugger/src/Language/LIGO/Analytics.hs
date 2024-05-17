module Language.LIGO.Analytics
  ( generateDebuggerLaunchAnalytics
  ) where

import Data.ByteString.Builder (toLazyByteString)
import Data.Text qualified as Text
import Data.Text.IO (readFile, writeFile)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Named ((!), (:?), NamedF (ArgF))
import Network.HTTP.Client
  (Request (..), RequestBody (RequestBodyLBS), httpNoBody, requestFromURI)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType, methodPut)
import Network.URI.Static (uri)
import Network.URI (URI (..), URIAuth, nullURI)
import System.FilePath ((</>), takeDirectory)
import System.Metrics.Prometheus.Encode.Text (encodeMetrics)
import System.Metrics.Prometheus.Registry (RegistrySample, new, registerCounter, sample)
import System.Metrics.Prometheus.Metric.Counter (inc)
import System.Metrics.Prometheus.MetricId (Name, fromList, makeName)
import Text.Interpolation.Nyan (int, rmode')
import UnliftIO (throwIO)
import UnliftIO.Directory
  (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, findFile)
import UnliftIO.Environment (lookupEnv)

import Language.LIGO.Debugger.Error
import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Helpers hiding (Name)
import Language.LIGO.Extension

-- | Recursively looks for the project root from the provided path. For example, if the
-- path is @/home/johndoe/test.mligo@, we'll look in @/home/johndoe/@, @/home/@, and @/@,
-- in this order, returning the first directory containing @ligo.json@, if it exists.
--
-- If the provided path is a directory, it will start the search from the same.
findProjectRoot :: MonadIO m => FilePath -> m (Maybe FilePath)
findProjectRoot startPath = do
  startDir <- bool (takeDirectory startPath) startPath <$> doesDirectoryExist startPath
  takeDirectory <<$>> findFile (generateDirs startDir) ligoproject
  where
    ligoproject :: String
    ligoproject = "ligo.json"

    generateDirs :: FilePath -> [FilePath]
    generateDirs = \case
      "/" -> ["/"]
      path -> path : generateDirs (takeDirectory path)

-- | @dotLigo projectRootMb rest useHomeFolder@ looks for the @.ligo@ directory. If
-- @useHomeFolder@ is set, it will look for the @HOME@ and @USERPROFILE@ environment
-- variables, in this order, picking the first one that is set, or @""@ if neither are
-- set. It will use @home/.ligo/rest@ as the path.
--
-- If it isn't set, however, it will try to use the project root. If it's @Nothing@, it
-- will use @./.ligo/rest@, or @projectRoot/.ligo/rest@, otherwise.
dotLigo :: MonadIO m => Maybe FilePath -> FilePath -> Bool -> m FilePath
dotLigo projectRootMb rest useHomeFolder
  | useHomeFolder = do
    -- We could use @System.Directory.getHomeDirectory@, but we're copying what LIGO does
    -- here for consistency.
    home <- lookupEnv "HOME" >>= maybe (fromMaybe "" <$> lookupEnv "USERPROFILE") pure
    pure $ home </> ".ligo" </> rest
  | otherwise =
    pure case projectRootMb of
      Nothing -> "." </> ".ligo" </> rest
      Just projectRoot -> projectRoot </> ".ligo" </> rest

-- | Checks whether the @LIGO_SKIP_ANALYTICS@ environment variable is set.
isSkipAnalyticsThroughEnvVar :: MonadIO m => m Bool
isSkipAnalyticsThroughEnvVar = isJust <$> lookupEnv "LIGO_SKIP_ANALYTICS"

-- | The URI to which we push counters. Do not use for gauges (not used by the debugger).
aggRegistry :: URI
aggRegistry =
  -- If you change this string, then also change the one in the compiler in
  -- @src/main/analytics/analytics.ml@. Look for the @agg_registry@ variable.
  [uri|https://agg.push.analytics.ligolang.org/metrics|]

-- | Looks for the path in @.ligo@ where @term_acceptance@ is stored. The optional file
-- path is the project root, see @findProjectRoot@ and @dotLigo@.
termAcceptanceFilepath :: MonadIO m => Maybe FilePath -> m FilePath
termAcceptanceFilepath projectRoot = dotLigo projectRoot "term_acceptance" True

-- | Creates a random UUID.
createId :: MonadIO m => m Text
createId = UUID.toText <$> liftIO UUID.nextRandom

-- | Checks whether the provided path exists. Currently just an alias to @doesFileExist@.
checkIdPresence :: MonadIO m => FilePath -> m Bool
checkIdPresence = doesFileExist

-- | @store str filepath@ writes @str@ to the provided @filepath@, overwriting it if
-- already exists. If the parent directory of @filepath@ doesn't exist, it will create it
-- /non-recursively/.
store :: MonadIO m => Text -> FilePath -> m ()
store str filepath = do
  createDirectoryIfMissing False $ takeDirectory filepath
  liftIO $ writeFile filepath str

-- | Gets the UUID at the provided file path. If it doesn't exist, it will create the file
-- with a random UUID generated with @createId@.
getOrCreateId :: MonadIO m => FilePath -> m Text
getOrCreateId idStoreFilepath =
  checkIdPresence idStoreFilepath >>= \case
    False -> do
      id' <- createId
      store id' idStoreFilepath
      pure id'
    True -> liftIO $ readFile idStoreFilepath

-- | Checks whether the term for collecting analytics is accepted. The path to the term
-- acceptance is expected to exist. The optional file path is the project root, see
-- @findProjectRoot@ and @dotLigo@.
isTermAccepted :: MonadIO m => Maybe FilePath -> m Bool
isTermAccepted projectRoot = do
  termAcceptance <- liftIO . readFile =<< termAcceptanceFilepath projectRoot
  pure $ termAcceptance == "accepted"

-- | Checks whether the current version is built off the @dev@ Git branch, i.e., the
-- version is (either @Rolling release@ or @@).
isDevVersion :: Version -> Bool
isDevVersion (Version version) =
  "Rolling release" `Text.isPrefixOf` version || Text.null version

-- | Gets or creates the user UUID. The optional file path is the project root, see
-- @findProjectRoot@.
getUserId :: MonadIO m => Maybe FilePath -> m Text
getUserId projectRoot = getOrCreateId =<< dotLigo projectRoot "user_id" True

-- | Gets or creates the repository UUID. The optional file path is the project root, see
-- @findProjectRoot@.
getRepositoryId :: MonadIO m => Maybe FilePath -> m Text
getRepositoryId projectRoot = getOrCreateId =<< dotLigo projectRoot "repository_id" False

-- | Generates metrics for the launch request, collecting the user ID, repository ID,
-- LIGO version, and launch syntax. Pushes metrics to the server (respecting conditions to
-- push them or not).
generateDebuggerLaunchAnalytics
  :: (MonadThrow m, HasLigoClient m)
  => Bool  -- ^ Whether to skip analytics collection (@True@) or not (@False@).
  -> FilePath  -- ^ The path to the program being launched.
  -> m ()
generateDebuggerLaunchAnalytics skipAnalytics program = unless skipAnalytics do
  projectRoot <- findProjectRoot program
  user <- getUserId projectRoot
  repository <- getRepositoryId projectRoot
  version <- getVersion
  syntax <- either throwIO pure getSyntax

  (debuggerInitializeCounter, registry) <-
    liftIO $ registerCounter
      ((mkName ! #namespace "ligo" ! #subsystem "tooling") "debugger_launch")
      (System.Metrics.Prometheus.MetricId.fromList
        [ ("user", user)
        , ("repository", repository)
        , ("version", unVersion version)
        , ("syntax", syntax)
        ])
      new
  liftIO $ inc debuggerInitializeCounter
  registry' <- liftIO $ sample registry
  pushMetrics version projectRoot skipAnalytics aggRegistry "analytics" registry'
  where
    getSyntax :: Either UnsupportedExtension Text
    getSyntax =
      getExt program <&> \case
        Caml -> "CameLIGO"
        Js -> "JsLIGO"

    formatMaybe :: Maybe Text -> Text
    formatMaybe = maybe Text.empty (flip Text.snoc '_')

    mkName :: "namespace" :? Text -> "subsystem" :? Text -> Text -> Name
    mkName (ArgF namespace) (ArgF subsystem) name =
      makeName [int|t|#{formatMaybe namespace}#{formatMaybe subsystem}#{name}|]

-- Copied from prometheus and adapted to our use case.
-- | Pushes metrics to the server (respecting conditions to push them or not).
pushMetrics
  :: (MonadThrow m, HasLigoClient m)
  => Version  -- ^ LIGO version.
  -> Maybe FilePath  -- ^ Optional path to the project root.
  -> Bool  -- ^ Whether to skip analytics (@True@) or not (@False@).
  -> URI  -- ^ PushGateway URI name, including port number (ex: @parseUri https://myGateway.com:8080@).
  -> Text  -- ^ Job name.
  -> RegistrySample  -- ^ Latest metrics.
  -> m ()
pushMetrics version projectRoot skipAnalytics gatewayURI jobName registrySample = do
  shouldSkipAnalytics <-
    -- We don't check whether we are in CI because the check is essentially "we
    -- are not in Docker and we are not in a TTY". Both conditions are always the case and
    -- the debug adapter itself isn't run by CI (just its test suite), so there is no
    -- reason to check for this.
    orM
      [ pure skipAnalytics
      , not <$> isTermAccepted projectRoot
      , isSkipAnalyticsThroughEnvVar
      , pure $ isDevVersion version
      ]
  unless shouldSkipAnalytics do
    manager <- newTlsManager
    gn <- maybe (throwM $ ImpossibleHappened "Invalid URI Authority") pure gatewayName
    requestUri <- requestFromURI $ buildUri scheme gn jobName
    void $ liftIO $ flip httpNoBody manager requestUri
      { method = methodPut
      , requestBody = RequestBodyLBS $ toLazyByteString $ encodeMetrics registrySample
      , requestHeaders = [(hContentType, "text/plain; version=0.0.4")]
      }
  where
    scheme :: String
    gatewayName :: Maybe URIAuth
    URI scheme gatewayName _ _ _ = gatewayURI

-- Copied from prometheus and adapted to our use case.
-- | @buildUri scheme gatewayName jobName@ builds a URI that can be used to push metrics
-- to the push gateway at @scheme@//@gatewayName@/metrics/job/@jobName@.
buildUri :: String -> URIAuth -> Text -> URI
buildUri scheme gatewayName jobName = nullURI
  { uriScheme = scheme
  , uriAuthority = Just gatewayName
  , uriPath = "/metrics/job/" <> Text.unpack jobName
  }
