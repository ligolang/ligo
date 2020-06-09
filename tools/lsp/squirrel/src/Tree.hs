
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
  , lookupTree
  , traverseTree
  , mk
  , infoOf

    -- * Callbacks on update
  , UpdateOver (..)
  , skip
  )
  where

import Data.Foldable
import Data.List
import Data.Sum
import Data.Monoid (First(..), getFirst)

import Lattice
import Comment
import Pretty
import Error

import Debug.Trace

-- | A tree, where each layer is one of @layers@ `Functor`s.
--
--   Is equipped with @info@.
--
--   Can contain `Error` instead of all the above.
--
newtype Tree layers info = Tree
  { unTree :: Either (Error info) (info, Sum layers (Tree layers info))
  }

dumpTree
  :: (Apply Functor layers, Apply Foldable layers, HasComments info, Pretty1 (Sum layers), Pretty info)
  => Tree layers info
  -> Doc
dumpTree (Tree tree) =
  case tree of
    Left e -> "ERR"
    Right (i, ls) ->
      pp (Tree tree) `indent` block (dumpTree <$> toList ls)

instance Apply Functor layers => Functor (Tree layers) where
  fmap f = go
    where
      go (Tree (Left   err))      = Tree $ Left $ fmap f err
      go (Tree (Right (a, rest))) = Tree $ Right (f a, fmap go rest)

instance Apply Foldable layers => Foldable (Tree layers) where
  foldMap f = go
    where
      go (Tree (Left   err))      = foldMap f err
      go (Tree (Right (a, rest))) = f a <> foldMap go rest

instance
    ( Apply Functor layers
    , HasComments info
    , Pretty1    (Sum layers)
    , Pretty      info
    )
  =>
    Show (Tree layers info)
  where
    show = show . pp

instance {-# OVERLAPS #-}
    ( HasComments info
    , Apply Functor fs
    , Pretty1    (Sum fs)
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
lookupTree
  :: forall fs info
  .  ( Apply Foldable fs
     , Apply Functor fs
     )
  => (info -> Bool)
  -> Tree fs info
  -> Maybe (Tree fs info)
lookupTree rightInfo = go
  where
    go :: Tree fs info -> Maybe (Tree fs info)
    go tree = do
      if rightInfo (infoOf tree)
      then getFirst $ foldMap (First . go) (layers tree) <> First (Just tree)
      else Nothing

    layers :: (Apply Foldable fs) => Tree fs info -> [Tree fs info]
    layers (Tree (Right (_, ls))) = toList ls
-- | Traverse the tree over some monad that exports its methods.
--
--   For each tree piece, will call `before` and `after` callbacks.
--
traverseTree
  :: ( UpdateOver m (Sum fs) (Tree fs a)
     , Apply Foldable fs
     , Apply Functor fs
     , Apply Traversable fs
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
mk :: (Functor f, Element f fs) => info -> f (Tree fs info) -> Tree fs info
mk i fx = Tree $ Right (i, inject fx)

-- | Get info from the tree.
infoOf :: Tree fs info -> info
infoOf = either eInfo fst . unTree

instance Stubbed (Tree fs info) info where
  stub = Tree . Left

instance Apply Foldable fs => HasErrors (Tree fs info) info where
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

instance Monad m => UpdateOver m (Sum '[]) a where
  before = error "Sum.empty"
  after  = error "Sum.empty"

instance (UpdateOver m f a, UpdateOver m (Sum fs) a) => UpdateOver m (Sum (f : fs)) a where
  before = either before before . decompose
  after  = either after  after  . decompose
