module Control.DelayedValues
  ( Manager (..)
  , newManager
  , putComputed
  , compute
  , computeSTM
  , runPendingComputations
  ) where

import Control.Concurrent.STM.TVar qualified as STM
import Control.Monad.STM.Class (MonadSTM (..))
import StmContainers.Map qualified as STM.Map

{- | Provides capabilities for batched values computation and caching.

The use case is the following: you have a heavy IO function that is
nevertheless deterministic and can be batched. You need to periodically perform
this computation, preferrably optimizing out extra calls, and get notified once
the computation is done.
-}
data Manager i o = (Eq i, Hashable i) => Manager
  { mComputation :: [i] -> IO [o]
    -- ^ The main computation we run.
  , mCache :: STM.Map.Map i o
    -- ^ Values that have been computed.
  , mPending :: TVar [i]
    -- ^ Values that were requested and wait for computation.
  }

-- | Instantiate a new empty manager for computations.
newManager
  :: (Eq i, Hashable i, MonadIO m)
  => ([i] -> IO [o])
  -- ^ The batched computation.
  -> m (Manager i o)
newManager mComputation = do
  mCache <- liftIO STM.Map.newIO
  mPending <- newTVarIO mempty
  return Manager{..}

-- | If some value could be computed fast you can use this function
-- to put its result instantly.
putComputed :: (MonadSTM m) => Manager i o -> i -> o -> m ()
putComputed Manager{..} i o = liftSTM $ STM.Map.insert o i mCache

-- | Version of 'compute' that can be run within an STM transaction.
computeSTM :: MonadSTM m => Manager i o -> i -> m (Maybe o)
computeSTM Manager{..} inp = liftSTM do
  mOut <- STM.Map.lookup inp mCache
  when (isNothing mOut) $ modifyTVar' mPending (inp :)
  return mOut

-- | Instantly get the computation result.
--
-- In case it has been evaluated for this input before, the cached output
-- is returned. Otherwise this function returns 'Nothing' and the input
-- is remembered for the later computation, which should be then manually
-- invoked with 'runPendingComputations'.
compute :: MonadIO m => Manager i o -> i -> m (Maybe o)
compute = atomically ... computeSTM

-- | Run all the pending computations at the moment.
--
-- This will call the computation supplied to 'newManager' at most once.
--
-- Returns whether the computation has been performed, or there were
-- no pending values to compute.
--
-- This call is synchronous and will block until the computation returns.
-- In case the computation throws an exception, it will be propagated;
-- the previously pending computation will not be preserved.
--
-- You should make sure to limit the number of concurrent calls to this function,
-- as this may result to consumed memory growth.
-- If running this asynchronously, use 'AbortingThreadPool'.
runPendingComputations :: MonadIO m => Manager i o -> m Bool
runPendingComputations Manager{..} = do
  pending <- atomically $ STM.swapTVar mPending []

  if null pending
  then return False
  else True <$ do
    results <- liftIO $ mComputation pending
    atomically $
      forM_ (zip pending results) \(i, o) ->
        STM.Map.insert o i mCache
