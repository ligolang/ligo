
{- | Utils for updating the @Tree@ type.
-}

module Update
  ( -- * Interfaces
    UpdateOver(..)

    -- * Default implementation
  , skip
  )
  where

-- | Update callbacks for a @f a@ while working inside monad @m@.
class Monad m => UpdateOver m f a where
  before :: f a -> m ()
  after  :: f a -> m ()

  before _ = skip
  after  _ = skip

-- | Do nothing.
skip :: Monad m => m ()
skip = return ()