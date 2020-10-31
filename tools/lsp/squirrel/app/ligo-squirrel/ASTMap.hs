module ASTMap
  ( ASTMap

  , empty
  , fetch
  , reload
  ) where

import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map
import Data.Hashable (Hashable)


data ASTMap k v m = ASTMap
  { amStore :: Map k v
  , amLoad  :: k -> m v
  }

empty :: (k -> m v) -> IO (ASTMap k v m)
empty amLoad = do
  amStore <- atomically Map.new
  return ASTMap { amStore, amLoad }


reload
  :: ( Eq k, Hashable k
     , MonadIO m
     )
  => k -> ASTMap k v m -> m v
reload k tmap = do
  v <- amLoad tmap k
  liftIO $ atomically $ Map.insert v k $ amStore tmap
  return v


fetch
  :: ( Eq k, Hashable k
     , MonadIO m
     )
  => k -> ASTMap k v m -> m v
fetch k tmap = do
  mv <- liftIO $ atomically $ Map.lookup k $ amStore tmap
  maybe (reload k tmap) return mv
