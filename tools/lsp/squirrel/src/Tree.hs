
module Tree where

import Data.Fix
import Data.Functor.Compose

import Union
import Update
import Lattice
import HasComments
import HasErrors
import Pretty
import Error
import Stubbed

-- | Tree is a fixpoint of `Union` @layers@, each equipped with an @info@.
newtype Tree layers info = Tree
  { unTree :: Fix (Either Error `Compose` (,) info `Compose` Union layers)
  }

instance (Functor (Union layers)) => Functor (Tree layers) where
  fmap f (Tree fixpoint) = Tree $ cata (Fix . go) fixpoint
    where
      go (Compose (Left err)) = Compose $ Left err
      go (Compose (Right (Compose (a, rest)))) =
        Compose $ Right $ Compose (f a, rest)

instance
    ( Functor (Union layers)
    , HasComments info
    , Pretty  (Union layers Doc)
    )
  =>
    Show (Tree layers info)
  where
    show = show . pp

instance {-# OVERLAPS #-}
    ( HasComments info
    , Functor (Union fs)
    , Pretty  (Union fs Doc)
    )
  =>
    Pretty (Tree fs info)
  where
    pp (Tree it) = cata aux it
      where
        aux (Compose (Left err)) = pp err
        aux (Compose (Right (Compose (info, fTree)))) = c info $ pp fTree

-- Return all subtrees that cover the range, ascending in side.
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
      -- traceShow (info <? info', info, info') $
      if   info <? info'
      then Tree tree : foldMap go fres
      else []

    go _ = []

-- | Update the tree in the monad that exports its methods.
updateTree
  :: ( UpdateOver m (Union fs) (Tree fs a)
     , Traversable  (Union fs)
     )
  => (a -> m b)
  -> Tree fs a -> m (Tree fs b)
updateTree act = fmap Tree . go . unTree
  where
    go (Fix (Compose (Right (Compose (a, union))))) = do
      b <- act a
      before (Tree <$> union)
      union' <- traverse go union
      after (Tree <$> union)
      return (Fix (Compose (Right (Compose (b, union')))))

    go (Fix (Compose (Left err))) = do
      return (Fix (Compose (Left err)))

-- | Make a tree out of a layer and an info.
mk :: (Functor f, Member f fs) => info -> f (Tree fs info) -> Tree fs info
mk i fx = Tree $ Fix $ Compose $ Right $ Compose (i, inj $ fmap unTree fx)

infoOf :: Tree fs info -> Maybe info
infoOf (Tree (Fix (Compose it))) =
  either
    (const Nothing)
    (Just . fst . getCompose) it

instance Stubbed (Tree fs info) where
  stubbing = Tree . Fix . Compose . Left

instance Foldable (Union fs) => HasErrors (Tree fs info) where
  errors = go . unTree
    where
      go (Fix (Compose (Left err))) = pure err
      go (Fix rest)                 = foldMap go rest