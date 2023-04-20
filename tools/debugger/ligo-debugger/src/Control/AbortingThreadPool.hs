module Control.AbortingThreadPool
  ( Pool
  , newPool
  , close
  , runAbortableAsync
  ) where

import Prelude hiding (onException)

import Data.Sequence qualified as Seq
import UnliftIO (MonadUnliftIO, mask_)
import UnliftIO.Async qualified as Async
import UnliftIO.MVar (modifyMVar_)

-- | Thread pool that aborts old computations when the limit on the number of
-- threads is hit.
--
-- Use this pool type for actions that are auxiliary and you are fine with them
-- being killed if necessary. You will get such liveness guarantee:
-- if no new actions are added, then the last @N@ actions will not be killed,
-- where @N@ is the limit specified in 'newPool'.
newtype Pool = Pool (MVar PoolState)

data PoolState = Working WorkingPool | Closed

data WorkingPool = WorkingPool
  { wpAsyncs :: Seq (Async.Async ())
    -- ^ Queue with asyncs that have been run.
  , wpLimit :: Int
    -- ^ How many entries we yet can put to the queue to stay within the limit.
    --
    -- Invariant: > 0
  }

-- | Make sure there is a slot for a new async.
--
-- This returns 1. a function for getting new state as soon as you get
-- an async to insert to the queue; 2. some old async that got removed from the
-- queue to avoid overflow.
wpAllocateSlot :: WorkingPool -> ((Async.Async () -> WorkingPool), Maybe (Async.Async ()))
wpAllocateSlot WorkingPool{..}
  | Seq.length wpAsyncs >= wpLimit =
      let (oldAsync, remAsyncs) = case Seq.viewl wpAsyncs of
            Seq.EmptyL -> error "Unexpectedly empty queue"
            h Seq.:< tl -> (h, tl)
      in
      (\newAsync -> WorkingPool
        { wpAsyncs = remAsyncs Seq.:|> newAsync
        , wpLimit
        }
      , Just oldAsync
      )
  | otherwise =
      (\newAsync -> WorkingPool
        { wpAsyncs = wpAsyncs Seq.:|> newAsync
        , wpLimit
        }
      , Nothing
      )

-- | Create a pool, accepts the limit on the threads number as an argument.
newPool :: (MonadIO m, HasCallStack) => Int -> m Pool
newPool limit = liftIO do
  when (limit <= 0) $ error "Limit must be positive"
  poolState <- newMVar $ Working WorkingPool
    { wpAsyncs = mempty
    , wpLimit = limit
    }
  return $ Pool poolState

-- | Close the pool, aborting all the running actions and stopping accepting
-- new actions.
close :: MonadIO m => Pool -> m ()
close (Pool poolState) = liftIO $ modifyMVar_ poolState \case
  Working WorkingPool{..} ->
    mapM_ Async.cancel wpAsyncs $> Closed
  Closed -> pure Closed

-- | Run an async action and record it.
--
-- If there are too many other async actions running at the same moment,
-- the oldest one will get aborted.
--
-- This call is blocking if some previous async action needs to be aborted
-- and it is uninteruptible at the moment; see also 'Async.cancel'.
--
-- If the pool has been closed, nothing happens.
runAbortableAsync :: MonadUnliftIO m => Pool -> m () -> m ()
runAbortableAsync (Pool poolState) action = modifyMVar_ poolState \case
  Closed -> pure Closed
  Working wState -> mask_ do
    let (mkNewWState, mOldAsyncToCancel) = wpAllocateSlot wState

    -- Abort the old computation.
    --
    -- Note that it does nothing for the completed asyncs.
    --
    -- This is synchronous intentionally, we cannot start the new async action
    -- before the old one terminates, to stay under the limit.
    whenJust mOldAsyncToCancel \oldAsync -> do
      Async.cancel oldAsync
      void $ Async.waitCatch oldAsync

    async <- Async.async action

    return $ Working $ mkNewWState async
