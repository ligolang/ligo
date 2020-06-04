
{- | The union of functors and utilities.

-}

module Union
  ( -- * Union type
    Union(..)
  , eliminate

    -- * Interface
  , Member
  , proj
  , inj
  )
  where

import Update
import Pretty

-- | The "one of" datatype.
--
--   Each @Union fs a@ is a @f a@, where @f@ is one of @fs@`.
data Union fs x where
  Here  ::       f  x -> Union (f : fs) x
  There :: Union fs x -> Union (f : fs) x

instance Eq          (Union '[] a) where (==)     = error "Union.empty"
instance Show        (Union '[] a) where show     = error "Union.empty"
instance Functor     (Union '[])   where fmap     = error "Union.empty"
instance Foldable    (Union '[])   where foldMap  = error "Union.empty"
instance Traversable (Union '[])   where traverse = error "Union.empty"

instance (Eq (f a), Eq (Union fs a)) => Eq (Union (f : fs) a) where
  a == b = case (a, b) of
    (Here  a', Here  b') -> a' == b'
    (There a', There b') -> a' == b'
    _                    -> False

instance (Show (f a), Show (Union fs a)) => Show (Union (f : fs) a) where
  show = eliminate show show

deriving stock instance (Functor     f, Functor     (Union fs)) => Functor     (Union (f : fs))
deriving stock instance (Foldable    f, Foldable    (Union fs)) => Foldable    (Union (f : fs))
deriving stock instance (Traversable f, Traversable (Union fs)) => Traversable (Union (f : fs))

-- | A case over `Union`.
eliminate
  :: (f x -> a)
  -> (Union fs x -> a)
  -> (Union (f : fs) x -> a)
eliminate here there = \case
  Here  fx   -> here fx
  There rest -> there rest

-- | The `f` functior is in the `fs` list.
class Member f fs where
  inj  :: f x -> Union fs x          -- ^ embed @f@ into some `Union`
  proj :: Union fs x -> Maybe (f x)  -- ^ check if a `Union` is actually @f@

instance {-# OVERLAPS #-} Member f (f : fs) where
  inj  = Here
  proj = eliminate Just (const Nothing)

instance Member f fs => Member f (g : fs) where
  inj  = There . inj
  proj = eliminate (const Nothing) proj

instance HasMethods m => UpdateOver m (Union '[]) a where
  before = error "Union.empty"
  after  = error "Union.empty"

instance (HasMethods m, UpdateOver m f a, UpdateOver m (Union fs) a) => UpdateOver m (Union (f : fs)) a where
  before = eliminate before before
  after  = eliminate after  after

instance Pretty1 (Union '[]) where
  pp1 = error "Union.empty"

instance (Pretty1 f, Pretty1 (Union fs)) => Pretty1 (Union (f : fs)) where
  pp1 = eliminate pp1 pp1