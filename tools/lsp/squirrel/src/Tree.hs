
{- | The carrier type for AST.

     "Untypedness" of the tree is a payoff to ablity to stop and navigate
     anywhere, not just inside the expression context.

     Is a `Functor` and `Foldable` over its @info@ parameter.
     Is not `Traversable`, because this will definitely not preserve scope.
     Use `updateTree` instead of `traverse`/`for`.
-}

module Tree
  -- ( -- * Tree type
  --   Tree
  -- , lookupTree
  -- , traverseTree
  -- , mk
  -- , infoOf

  --   -- * Callbacks on update
  -- , UpdateOver (..)
  -- , skip
  -- )
  where

import Data.Foldable
import Data.List
import Data.Sum
import Data.Monoid (First(..), getFirst)

import Lattice
import Comment
import Pretty
import Error
import Range

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
    ( Apply Traversable layers
    , Apply Foldable    layers
    , Apply Functor     layers
    )
  =>
    Traversable (Tree layers)
  where
    traverse f = go
      where
        go (Tree (Left   err))      = (Tree . Left) <$> traverse f err
        go (Tree (Right (a, rest))) = do
          a'    <- f a
          rest' <- (traverse.traverse) f rest
          return $ Tree $ Right (a', rest')

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
     , HasRange info
     )
  => Range
  -> Tree fs info
  -> Maybe (Tree fs info)
lookupTree target = go
  where
    go :: Tree fs info -> Maybe (Tree fs info)
    go tree = do
      if target <? getRange (infoOf tree)
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
     , HasRange a
     )
  => (a -> m b) -> Tree fs a -> m (Tree fs b)
traverseTree act = go
  where
    go (Tree (Right (a, union))) = do
      b <- act a
      before (getRange a) union
      union' <- traverse go union
      after (getRange a) union
      return (Tree (Right (b, union')))

    go (Tree (Left err)) = do
      err' <- traverse act err
      return (Tree (Left err'))

traverseOnly
  :: forall f a b fs m
  .  ( Monad m
     , Monad m
     , Element f fs
     , Apply Foldable fs
     , Apply Functor fs
     , Apply Traversable fs
     , Traversable f
     , HasRange a
     )
  => (a -> f (Tree fs a) -> m (f (Tree fs a)))
  ->          Tree fs a  -> m    (Tree fs a)
traverseOnly act = go
  where
    go (match -> Just (r, fa)) = do
      fb <- act r fa
      fc <- traverse go fb
      pure $ mk r fc

    go tree@(Tree (Right (r, union))) = do
      union' <- traverse go union
      pure $ Tree $ Right (r, union')

    go tree = pure tree

data Visit fs a b m where
  Visit
    :: (Element f fs, Traversable f)
    => (a -> f (Tree fs a) -> m (b, f (Tree fs a)))
    -> Visit fs a b m

traverseMany
  :: ( Apply Functor fs
     , Apply Foldable fs
     , Apply Traversable fs
     , Monad m
     )
  => [Visit fs a b m]
  -> (a -> b)
  -> Tree fs a
  -> m (Tree fs b)
traverseMany visitors orElse = go
  where
    go tree = aux visitors
      where
        aux (Visit visitor : rest) = do
          case match tree of
            Just (r, fa) -> do
              (r', fa')  <- visitor r fa
              fa'' <- traverse go fa'
              return $ mk r' fa''
            Nothing -> do
              aux rest
        aux [] = do
          case tree of
            Tree (Right (r, union)) -> do
              union' <- traverse go union
              return $ Tree $ Right (orElse r, union')
            Tree (Left err) -> do
              return $ Tree $ Left $ fmap orElse err

-- | Make a tree out of a layer and an info.
mk :: (Functor f, Element f fs) => info -> f (Tree fs info) -> Tree fs info
mk i fx = Tree $ Right (i, inject fx)

match
  :: (Functor f, Element f fs)
  => Tree fs info
  -> Maybe (info, f (Tree fs info))
match (Tree (Left   _))      = Nothing
match (Tree (Right (r, it))) = do
  f <- project it
  return (r, f)

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
  before :: Range -> f a -> m ()
  after  :: Range -> f a -> m ()

  before _ _ = skip
  after  _ _ = skip

-- | Do nothing.
skip :: Monad m => m ()
skip = return ()

instance Monad m => UpdateOver m (Sum '[]) a where
  before = error "Sum.empty"
  after  = error "Sum.empty"

instance (UpdateOver m f a, UpdateOver m (Sum fs) a) => UpdateOver m (Sum (f : fs)) a where
  before r = either (before r) (before r) . decompose
  after  r = either (after  r) (after  r) . decompose
