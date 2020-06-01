
module Update
  ( HasMethods(..)
  , UpdateOver(..)
  , skip
  )
  where

-- | Abstraction over monad capabilities.
class Monad m => HasMethods m where
  data Methods m :: *
  method :: Methods m

-- | Update callbacks for a @f a@ while working inside monad @m@.
class HasMethods m => UpdateOver m f a where
  before :: f a -> m ()
  after  :: f a -> m ()

  before _ = skip
  after  _ = skip

-- | Do nothing.
skip :: Monad m => m ()
skip = return ()