
module Debouncer (debounced) where

import Control.Monad.Catch
import Control.Monad
import Control.Concurrent

-- | Ensure the function is run in single thread, w/o overlapping.
--
--   If called concurently, everyone will get results of the winner.
--
--   If called, waits for next result to arrive.
--
--   If function throws an error, will rethrow it in caller thread.
--
debounced :: forall s r. (s -> IO r) -> IO (s -> IO r)
debounced act = do
  i <- newEmptyMVar
  o <- newEmptyMVar

  mask_ do
    forkIO do
      forever do
        _  <- tryTakeMVar o
        i' <- takeMVar i
        o' <- try $ act i'
        putMVar o (o' :: Either SomeException r)

  return $ \i' -> do
    _ <- tryTakeMVar i
    putMVar i i'
    readMVar o >>= either throwM return

test :: IO ([Int] -> IO Int)
test = debounced \s -> do
  threadDelay 2000000
  unless (odd (length s)) do
    error "even"
  return (length s)
