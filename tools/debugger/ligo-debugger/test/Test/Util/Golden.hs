-- | Golden tests functionality.
module Test.Util.Golden
  ( goldenTestWithSnapshots
  , dumpCurSnapshot
  , dumpAllSnapshotsWithStep
  ) where

import Data.Algorithm.Diff (PolyDiff (Both), getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Fmt (Doc, FromDoc, fmt, pretty)
import GHC.Stack (srcLocFile, srcLocStartLine)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (pathSeparator)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit.Lang qualified as HUnit
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.Interpolation.Nyan hiding (rmode')
import Text.Show qualified
import UnliftIO.Exception (handle, throwIO)

import Morley.Debugger.Core.Navigate
  (HistoryReplay, HistoryReplayM, MovementResult (..), curSnapshot, frozen)

import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Snapshots

import Test.Util

-- | Wraps 'HUnit.HUnitFailure' and provides a neat 'Show' instance for it.
--
-- This way the 'assertFailure''s exceptions are renderred properly even
-- outside of 'Test.Tasty.HUnit.testCase'.
newtype UnhandledHUnitException = UnhandledHUnitException HUnit.HUnitFailure
  deriving stock (Eq)

instance Show UnhandledHUnitException where
  show = displayException

instance Exception UnhandledHUnitException where
  displayException (UnhandledHUnitException (HUnit.HUnitFailure mloc reason)) =
    prependLocation mloc $ HUnit.formatFailureReason reason
    where
      prependLocation = \case
        Nothing -> id
        Just loc ->
          \s -> [int||
            #{srcLocFile loc}:#s{srcLocStartLine loc}:
            #{s}
            |]

-- | Let's dumping a piece of output, that will be then matched againt
-- the golden file.
data GoldenActionContext = GoldenActionContext
  { gacDumpOutput :: Doc -> IO ()
  , gacLigoTypesVec :: LigoTypesVec
  }

-- | This creates a golden test of the snapshots visited by the specific replay.
--
-- This is similar to 'testwithSnapshots', but it lets printing all the
-- interesting snapshots to a file and check them all in a batch instead of
-- checking each of them manually in the code.
--
-- The supplied action is similar to what you would pass to 'testWithSnapshots',
-- but now instead of checking the interesting snapshots manually, you should use
-- 'dumpCurSnapshot' at all interesting positions.
--
-- Also feel free to arbitrarily use 'dumpComment' if they can help in understanding
-- the test code or the contents of the golden file.
--
-- Tips:
--
-- * Use 'assertFailure' to fail if necessary.
-- * When running tests, use @--test-arguments --accept@ to re-generate the
--   output files. Make sure to thoroughly verify the diff before committing it.
-- * In case there are multiple unrelated factors inducing golden files change
--   (e.g. a dependency bump, pretty printer change, stepping or breakpoints logic
--   changes, try to put those in separate commits).
goldenTestWithSnapshotsImpl
  :: (Text -> IO ())
     -- ^ What to do with logs.
  -> TestName
     -- ^ Name of the test suite, pass it here instead of 'testCase'.
  -> FilePath
     -- ^ Golden file.
  -> ContractRunData
     -- ^ Which contract to run and how.
  -> ReaderT GoldenActionContext (ReaderT TestCtx (HistoryReplayM (InterpretSnapshot 'Unique) IO)) ()
     -- ^ The movement logic within a special monad that has 'HistoryReplay'
     -- constraint and provides a way to construct the expected output.
  -> TestTree
goldenTestWithSnapshotsImpl logger testName goldenFolder runData logicFunc = do
  goldenTest
    testName
    (Text.readFile outputFile)
    action
    (pure ... runCmp)
    updateFile
  where
    outputFilename = testName
      & map Char.toLower
      & filter (not . ((== pathSeparator) || Char.isPunctuation || Char.isSymbol))
      & intercalate "-" . List.words
    outputDir = "test" </> "golden" </> goldenFolder
    outputFile = outputDir </> outputFilename <.> "golden"

    action = handle (throwIO . UnhandledHUnitException) do
      outputVar <- newIORef mempty
      let write = liftIO . modifyIORef outputVar . (:)
      testWithSnapshotsImpl logger Nothing runData do
        ligoTypesVec <- asks tcLigoTypesVec
        usingReaderT (GoldenActionContext write ligoTypesVec) logicFunc
      recordedOutput <- readIORef outputVar
      return $
        fmt $ mconcat $ intersperse "\n" $
        reverse recordedOutput

    runCmp :: Text -> Text -> Maybe String
    runCmp =
      validateDiff
      ...
      (getGroupedDiff `on` (map toString . Text.lines))
      where
        validateDiff diff = do
          guard $ any \case{ Both{} -> False; _ -> True } diff
          return [int|n|
            Output mismatch, in-file (<) vs produced (>):

            #{ppDiff diff}


            If you intend the new outputs to be considered valid,
            pass `--accept` option to the test executable to update
            the golden files and then verify the diff.
            |]

    updateFile txt = do
      createDirectoryIfMissing True outputDir
      -- We intentionally let several golden tests run against the same file,
      -- this way we can avoid duplication.
      -- For this to work, we have to ensure that no two writes to the same file
      -- occur in parallel. And here is one of the ways to guarantee this:
      writeFileNoMoreThanOnce outputFile txt

    writeFileNoMoreThanOnce targetFile txt = do
      alreadyWritten <- gonnaWriteGoldenFile targetFile
      unless alreadyWritten $ Text.writeFile targetFile txt

-- | All golden files that have been written.
writtenGoldenFiles :: IORef (Set FilePath)
writtenGoldenFiles = unsafePerformIO $ newIORef mempty
{-# NOINLINE writtenGoldenFiles #-}

-- | Records that you are planning to write the given file, and returns
-- whether a previous write to the same file has taken place.
gonnaWriteGoldenFile :: FilePath -> IO Bool
gonnaWriteGoldenFile file = do
  atomicModifyIORef writtenGoldenFiles \files ->
    (Set.insert file files, Set.member file files)

goldenTestWithSnapshots, _goldenTestWithSnapshotsLogging
  :: TestName
  -> FilePath
  -> ContractRunData
  -> ReaderT GoldenActionContext (ReaderT TestCtx (HistoryReplayM (InterpretSnapshot 'Unique) IO)) ()
  -> TestTree
goldenTestWithSnapshots = goldenTestWithSnapshotsImpl dummyLoggingFunction
_goldenTestWithSnapshotsLogging = goldenTestWithSnapshotsImpl putStrLn
{-# WARNING _goldenTestWithSnapshotsLogging "'goldenTestWithSnapshotsLogging' remains in code" #-}

-- | A little fun, here it seems justified.
instance (a ~ (), MonadIO m) => FromDoc (ReaderT GoldenActionContext m a) where
  fmt = dumpComment

-- | Dump some text, it will be put to the overall compared text without changes
-- (except for some surrounding newlines).
dumpComment
  :: (MonadReader GoldenActionContext m, MonadIO m)
  => Doc -> m ()
dumpComment msg = ask >>= \GoldenActionContext{..} -> liftIO $ gacDumpOutput msg

-- | Dump the current snapshot to the overall compared text.
dumpCurSnapshot
  :: ( HistoryReplay (InterpretSnapshot 'Unique) m
     , MonadReader GoldenActionContext m, MonadIO m
     )
  => m ()
dumpCurSnapshot =
  asks gacLigoTypesVec >>= \vec -> frozen curSnapshot >>= dumpComment . pretty . makeConciseSnapshots vec

-- | Perform the given step many times until the end and record the snapshots
-- at stops.
dumpAllSnapshotsWithStep
  :: ( HistoryReplay (InterpretSnapshot 'Unique) m
     , MonadReader GoldenActionContext m, MonadIO m
     , HasCallStack
     )
  => m (MovementResult a)
  -> m ()
dumpAllSnapshotsWithStep step = go (1000 :: Int)
  where
  go 0 = error "Looks like stepping fell into infinite loop"
  go n = do
    dumpCurSnapshot
    step >>= \case{ HitBoundary -> pass; _ -> go (n - 1) }
