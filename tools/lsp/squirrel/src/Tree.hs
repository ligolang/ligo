
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
  , updateTree
  , mk
  , infoOf

    -- * Callbacks on update
  , UpdateOver (..)
  , skip
  )
  where

import Data.Fix
import Data.Functor.Compose
import Data.Foldable

import Union
import Lattice
import HasComments
import Pretty
import Error

-- | A tree, where each layer is one of @layers@ `Functor`s.
--
--   Is equipped with @info@.
--
--   Can contain `Error` instead of all the above.
--
newtype Tree layers info = Tree
  { unTree :: Fix (Either (Error info) `Compose` (,) info `Compose` Union layers)
  }

instance (Functor (Union layers)) => Functor (Tree layers) where
  fmap f (Tree fixpoint) = Tree $ cata (Fix . go) fixpoint
    where
      go (Compose (Left err)) = Compose $ Left $ fmap f err
      go (Compose (Right (Compose (a, rest)))) =
        Compose $ Right $ Compose (f a, rest)

instance (Functor (Union layers), Foldable (Union layers)) => Foldable (Tree layers) where
  foldMap f (Tree fixpoint) = cata go fixpoint
    where
      go (Compose (Left err))                  = foldMap f err
      go (Compose (Right (Compose (a, rest)))) = f a <> fold rest

instance
    ( Functor    (Union layers)
    , HasComments info
    , Pretty     (Union layers Doc)
    , Pretty      info
    )
  =>
    Show (Tree layers info)
  where
    show = show . pp

instance {-# OVERLAPS #-}
    ( HasComments info
    , Functor    (Union fs)
    , Pretty     (Union fs Doc)
    , Pretty      info
    )
  =>
    Pretty (Tree fs info)
  where
    pp (Tree it) = cata aux it
      where
        aux (Compose (Left err)) = pp err
        aux (Compose (Right (Compose (info, fTree)))) = c info $ pp fTree

-- | Return all subtrees that cover the range, ascending in size.
spineTo
  :: ( Lattice   info
     , Foldable (Union fs)
     )
  => info
  -> Tree fs info
  -> [Tree fs info]
spineTo info = reverse . go . unTree
  where
    go tree@(Fix (Compose (Right (Compose (info', fres))))) =
      if   info <? info'
      then Tree tree : foldMap go fres
      else []

    go _ = []

-- | Traverse the tree over some monad that exports its methods.
--
--   For each tree piece, will call `before` and `after` callbacks.
--
updateTree
  :: ( UpdateOver m (Union fs) (Tree fs a)
     , Traversable  (Union fs)
     )
  => (a -> m b) -> Tree fs a -> m (Tree fs b)
updateTree act = fmap Tree . go . unTree
  where
    go (Fix (Compose (Right (Compose (a, union))))) = do
      b <- act a
      before (Tree <$> union)
      union' <- traverse go union
      after (Tree <$> union)
      return (Fix (Compose (Right (Compose (b, union')))))

    go (Fix (Compose (Left err))) = do
      err' <- traverse act err
      return (Fix (Compose (Left err')))

-- | Make a tree out of a layer and an info.
mk :: (Functor f, Member f fs) => info -> f (Tree fs info) -> Tree fs info
mk i fx = Tree $ Fix $ Compose $ Right $ Compose (i, inj $ fmap unTree fx)

-- | Get info from the tree.
infoOf :: Tree fs info -> info
infoOf = either eInfo (fst . getCompose) . getCompose . unFix . unTree

instance Stubbed (Tree fs info) info where
  stub = Tree . Fix . Compose . Left

instance Foldable (Union fs) => HasErrors (Tree fs info) info where
  errors = go . unTree
    where
      go (Fix (Compose (Left err))) = pure err
      go (Fix rest)                 = foldMap go rest

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
