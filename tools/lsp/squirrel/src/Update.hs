
module Update where

{-
  Abstraction over monad capabilities.
-}
class Monad m => HasMethods m where
  data Methods m :: *
  method :: Methods m

{-
  Given some AST structure, do some stuff before & after it is traversed.
-}
class HasMethods m => UpdateOver m f a where
  before :: f a -> m ()
  after  :: f a -> m ()

  before _ = skip
  after  _ = skip

skip :: Monad m => m ()
skip = return ()