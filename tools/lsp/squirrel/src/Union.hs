
{- | The union of functors and utilities.

-}

module Union
  ( -- * Union type
    Union
  , eliminate

    -- * Interface
  , Member
  , proj
  , inj
  )
  where

import Data.Kind
import Data.Function (on)

import GHC.Exts
import GHC.TypeLits

import Unsafe.Coerce

import Pretty

-- | The "one of" datatype.
--
--   Each @Union fs a@ is a @f a@, where @f@ is one of @fs@`.
data Union (fs :: [* -> *]) x where
  MkUnion :: Integer -> Any fs x -> Union fs x

type family Find (f :: * -> *) fs :: Nat where
  Find f  (f : _)  = 0
  Find f  (_ : fs) = 1 + Find f fs

type family Len (fs :: [* -> *]) :: Nat where
  Len '[]  = 0
  Len  (_ : fs) = 1 + Len fs

type Member f fs = KnownNat (Find f fs)
type KnownList fs = KnownNat (Len fs)

val :: forall n. KnownNat n => Integer
val = natVal' (proxy# :: Proxy# n)

inj
  :: forall f fs n x
  .  ( n ~ Find f fs
     , KnownNat n
     )
  => f x
  -> Union fs x
inj fx = MkUnion (val @n) (unsafeCoerce fx)

raise
  :: Union fs x
  -> Union (f : fs) x
raise (MkUnion i b) = MkUnion (i + 1) (unsafeCoerce b)

proj
  :: forall f fs n x
  .  ( n ~ Find f fs
     , KnownNat n
     )
  => Union fs x
  -> Maybe (f x)
proj (MkUnion i body)
  | i == val @n = Just $ unsafeCoerce body
  | otherwise   = Nothing

split
  :: Union (f : fs) x
  -> Either (f x) (Union fs x)
split = eliminate Left Right

vacuum :: Union '[] a -> b
vacuum = error "Empty union"

-- | A case-split over `Union`.
eliminate
  :: (f x -> a)
  -> (Union fs x -> a)
  -> (Union (f : fs) x -> a)
eliminate here there (MkUnion i body)
  | i == 0    = here  $ unsafeCoerce body
  | otherwise = there $ MkUnion (i - 1) (unsafeCoerce body)

instance Eq          (Union '[] a) where (==)       = vacuum
instance Show        (Union '[] a) where show       = vacuum
instance Functor     (Union '[])   where fmap     _ = vacuum
instance Foldable    (Union '[])   where foldMap  _ = vacuum
instance Traversable (Union '[])   where traverse _ = vacuum

instance (Eq (f a), Eq (Union fs a)) => Eq (Union (f : fs) a) where
  (==) = (==) `on` split

instance (Show (f a), Show (Union fs a)) => Show (Union (f : fs) a) where
  show = eliminate show show

instance (Functor f, Functor (Union fs)) => Functor (Union (f : fs)) where
  fmap f = eliminate (inj . fmap f) (raise . fmap f)

instance (Foldable f, Foldable (Union fs)) => Foldable (Union (f : fs)) where
  foldMap f = eliminate (foldMap f) (foldMap f)

instance (Traversable f, Traversable (Union fs)) => Traversable (Union (f : fs)) where
  traverse f = eliminate (fmap inj . traverse f) (fmap raise . traverse f)

instance Pretty1 (Union '[]) where
  pp1 = vacuum

instance (Pretty1 f, Pretty1 (Union fs)) => Pretty1 (Union (f : fs)) where
  pp1 = eliminate pp1 pp1