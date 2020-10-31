
module ASTMap where

import Control.Concurrent.STM (atomically)
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map
import Control.Monad.Reader
import Data.Hashable (Hashable)

import Product (Contains, Product, getElem)

data T k v m = ASTMap
  { amStore :: Map k v
  , amLoad  :: k -> m v
  }

reload
  :: ( MonadReader (Product xs) m
     , Hashable k
     , Eq k
     , MonadIO m
     , Contains (T k v m) xs
     )
  => k -> m v
reload k = do
  tmap <- asks getElem
  v <- amLoad tmap k
  liftIO $ atomically $ Map.insert v k $ amStore tmap
  return v

fetch
  :: forall k v m xs
  .  ( MonadReader (Product xs) m
     , Hashable k
     , Eq k
     , MonadIO m
     , Contains (T k v m) xs
     )
  => k -> m v
fetch k = do
  tmap <- asks getElem
  mv <- liftIO $ atomically $ Map.lookup k $ amStore (tmap :: T k v m)
  maybe (reload k) return mv

empty :: forall k v m. (k -> m v) -> IO (T k v m)
empty amLoad = do
  amStore <- atomically Map.new
  return ASTMap { amStore, amLoad }
