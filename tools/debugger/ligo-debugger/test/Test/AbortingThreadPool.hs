module Test.AbortingThreadPool
  ( test_Execution
  ) where

import Control.Concurrent (threadDelay)
import Test.Tasty (TestTree, testGroup)
import Test.Util
import Text.Interpolation.Nyan
import UnliftIO.Async (waitEither, withAsync)

import Control.AbortingThreadPool

-- | Functions for splitting the execution into stages.
-- This helps to enforce a particular order of execution.
data Stager m = Stager
  { -- | Block until the given or later stage comes in.
    waitTill :: Word -> m ()

    -- | Jump to the given stage.
  , jumpTo   :: Word -> m ()

    -- | Block until the given stage, and then jump to the next one.
    --
    -- This allows for multiple calls for the same stage, but all
    -- except one can get interleaved with the actions at next stages.
  , complete :: Word -> m ()
  }

{- | Creates a function that forces the test scenario to execute in stages.

Stages should grow strictly sequentially and start from 1.
Passing a stage @N@ (@N > 1@) when @N - 1@ does not exist will cause deadlock.

Note: logs are not guaranteed to be printed in order.

-}
mkStager :: (String -> IO ()) -> IO (Stager IO)
mkStager log = do
  curStageVar <- newTVarIO 1
  log [int||Declaring stage 1|]
  return Stager
    { waitTill = \neededStage -> atomically do
        curStage <- readTVar curStageVar
        guard (curStage >= neededStage)
    , jumpTo = \stage -> do
        log [int||Jumped to stage #{stage}|]
        atomically $ writeTVar curStageVar stage
    , complete = \neededStage -> do
        atomically do
          curStage <- readTVar curStageVar
          guard (curStage >= neededStage)
          writeTVar curStageVar (max (neededStage + 1) curStage)
        log [int||Completed stage #{neededStage}|]
    }

timeLimited :: IO a -> IO a
timeLimited action =
  withAsync action \actionAsync ->
  withAsync (threadDelay 10e6) \timerAsync -> do
    waitEither timerAsync actionAsync >>= \case
      Left () -> assertFailure "Test didn't complete in time"
      Right a -> return a

test_Execution :: TestTree
test_Execution = testGroup "Execution"
  [ testCaseSteps "Can execute jobs" \doStep -> timeLimited do
      Stager{..} <- mkStager doStep
      pool <- newPool 5
      runAbortableAsync pool $ complete 1
      runAbortableAsync pool $ complete 2
      waitTill 3

  , testCaseSteps "On overflow the old jobs are cancelled" \doStep -> timeLimited do
      Stager{..} <- mkStager doStep
      pool <- newPool 2

      runAbortableAsync pool do
        (complete 1 >> complete 999)
          `onException` complete 4
      complete 2
      runAbortableAsync pool $ complete 6
      complete 3
      runAbortableAsync pool $ complete 7
      complete 5
      complete 8

  , testCaseSteps "Closing pool works" \doStep -> timeLimited do
      Stager{..} <- mkStager doStep
      pool <- newPool 5

      runAbortableAsync pool do
        (complete 1 >> complete 999)
          `onException` complete 4
      runAbortableAsync pool do
        (complete 2 >> complete 999)
          `onException` complete 5
      complete 3
      close pool
      complete 6
  ]
