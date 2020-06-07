
{- | The carrier type for AST.

     "Untypedness" of the tree is a payoff to ablity to stop and navigate
     anywhere, not just inside the expression context.

     Is a `Functor` and `Foldable` over its @info@ parameter.
     Is not `Traversable`, because this will definitely not preserve scope.
     Use `updateTree` instead of `traverse`/`for`.
-}

module Tree
  ( -- * Tree type
    Tree
  , spineTo
  , traverseTree
  , mk
  , infoOf

    -- * Callbacks on update
  , UpdateOver (..)
  , skip
  )
  where

import Union
import Lattice
import Comment
import Pretty
import Error

-- | A tree, where each layer is one of @layers@ `Functor`s.
--
--   Is equipped with @info@.
--
--   Can contain `Error` instead of all the above.
--
newtype Tree layers info = Tree
  { unTree :: Either (Error info) (info, Union layers (Tree layers info))
  }

instance (Functor (Union layers)) => Functor (Tree layers) where
  fmap f = go
    where
      go (Tree (Left   err))      = Tree $ Left $ fmap f err
      go (Tree (Right (a, rest))) = Tree $ Right (f a, fmap go rest)

instance (Functor (Union layers), Foldable (Union layers)) => Foldable (Tree layers) where
  foldMap f = go
    where
      go (Tree (Left   err))      = foldMap f err
      go (Tree (Right (a, rest))) = f a <> foldMap go rest

instance
    ( Functor    (Union layers)
    , HasComments info
    , Pretty1    (Union layers)
    , Pretty      info
    )
  =>
    Show (Tree layers info)
  where
    show = show . pp

instance {-# OVERLAPS #-}
    ( HasComments info
    , Functor    (Union fs)
    , Pretty1    (Union fs)
    , Pretty      info
    )
  =>
    Pretty (Tree fs info)
  where
    pp = go
      where
        go (Tree (Left  err))           = pp err
        go (Tree (Right (info, fTree))) = c info $ pp fTree

-- | Return all subtrees that cover the range, ascending in size.
spineTo
  :: ( Lattice   info
     , Foldable (Union fs)
     )
  => info
  -> Tree fs info
  -> [Tree fs info]
spineTo info = reverse . go
  where
    go tree@(Tree (Right (info', fres))) =
      if   info <? info'
      then tree : foldMap go fres
      else []

    go _ = []

-- | Traverse the tree over some monad that exports its methods.
--
--   For each tree piece, will call `before` and `after` callbacks.
--
traverseTree
  :: ( UpdateOver m (Union fs) (Tree fs a)
     , Traversable  (Union fs)
     )
  => (a -> m b) -> Tree fs a -> m (Tree fs b)
traverseTree act = go
  where
    go (Tree (Right (a, union))) = do
      b <- act a
      before union
      union' <- traverse go union
      after union
      return (Tree (Right (b, union')))

    go (Tree (Left err)) = do
      err' <- traverse act err
      return (Tree (Left err'))

-- | Make a tree out of a layer and an info.
mk :: (Functor f, Member f fs) => info -> f (Tree fs info) -> Tree fs info
mk i fx = Tree $ Right (i, inj fx)

-- | Get info from the tree.
infoOf :: Tree fs info -> info
infoOf = either eInfo fst . unTree

instance Stubbed (Tree fs info) info where
  stub = Tree . Left

instance Foldable (Union fs) => HasErrors (Tree fs info) info where
  errors = go
    where
      go (Tree (Left   err))      = pure err
      go (Tree (Right (_, rest))) = foldMap go rest

-- | Update callbacks for a @f a@ while working inside monad @m@.
class Monad m => UpdateOver m f a where
  before :: f a -> m ()
  after  :: f a -> m ()

  before _ = skip
  after  _ = skip

-- | Do nothing.
skip :: Monad m => m ()
skip = return ()

instance Monad m => UpdateOver m (Union '[]) a where
  before = error "Union.empty"
  after  = error "Union.empty"

instance (UpdateOver m f a, UpdateOver m (Union fs) a) => UpdateOver m (Union (f : fs)) a where
  before = eliminate before before
  after  = eliminate after  after
