{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs, UndecidableInstances #-}

module Util
  ( groupByKey
  , ForInternalUse
  , itIsForInternalUse
  , allowedForInternalUseOnly
  , rmode'ansi
  , rmode'
  , everywhereM'
  , safeIndex
  , unionOrd
  , findKey
  , mapJsonText
  , traverseJsonText
  , lazyBytesToText
  , textToLazyBytes
  , (<<&>>)
  , TextualNumber (..)

  -- * Debugging utilities
  , validate
  ) where

import Control.Monad.Validate (MonadValidate, refute)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Data.Bitraversable (bitraverse)
import Data.List (groupBy, singleton)
import Data.Map.Internal qualified as MI
import Data.MessagePack (Config, DecodeError, MessagePack, Object (..), decodeError)
import Data.MessagePack.Types (MessagePack (fromObjectWith, toObject))
import Data.Reflection (Given, give, given)
import Data.Scientific qualified as Sci
import Data.Text.Lazy.Encoding qualified as TL
import Fmt.Buildable (Buildable, FromDoc, pretty)
import Fmt.Internal.Core (FromBuilder, fromBuilder)
import Generics.SYB (Data (gmapM), GenericM)
import System.Console.ANSI (SGR, setSGRCode)
import Text.Interpolation.Nyan.Core (RMode (..))

import Duplo (Cofree ((:<)), Lattice (leq))

-- | Forms an associative list from list of values and to key/value mappers.
groupByKey
  :: forall a k v
   . Ord k
  => (a -> k) -- ^ to key mapper
  -> (a -> v) -- ^ to value mapper
  -> [a] -- ^ a list of values
  -> [(k, [v])]
groupByKey f g =
  extractGroup
  . groupBy ((==) `on` f)
  . sortOn f
  where
    extractGroup :: [[a]] -> [(k, [v])]
    extractGroup [] = []
    extractGroup ([] : xs) = extractGroup xs
    extractGroup (ys@(y : _) : xs) = (f y, g <$> ys) : extractGroup xs

-- | This constraint indicates that the given value/function must be used
-- only where it will affect us, developers, and would be invisible for the users
-- (it is fine if users can find it if they try hard though).
--
-- It serves as a safety measure.
--
-- Examples:
--
-- * A function that is to be used in tests only;
-- * A function that prints too verbose information.
type ForInternalUse = Given IsForInternalUse
data IsForInternalUse = IsForInternalUse

-- | Allow using values that require 'ForInternalUse'.
itIsForInternalUse :: (ForInternalUse => a) -> a
itIsForInternalUse = give IsForInternalUse

-- | Mark a value as @ForInternalUse@.
allowedForInternalUseOnly :: ForInternalUse => a -> a
allowedForInternalUseOnly =
  case given @IsForInternalUse of IsForInternalUse -> id

-- | Rendering mode for a list of SGR (Select Graphic Rendition).
rmode'ansi :: RMode [SGR]
rmode'ansi = RMode (pretty . concatMap (setSGRCode . singleton))

-- | Monadic variation on everywhere'
everywhereM' :: forall m. Monad m => GenericM m -> GenericM m
everywhereM' f = go
  where
    -- Up-bottom order is also reflected in order of do-actions
    go :: GenericM m
    go x = do
      x' <- f x
      gmapM go x'

-- | Picks n-th element in the list.
-- Returns @Nothing@ if the index is out of bounds.
safeIndex :: (Eq t, Num t) => [a] -> t -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x : _) 0 = Just x
safeIndex (_ : xs) n = safeIndex xs (n - 1)

-- | Takes the union of two lists, leaving no duplicates using the provided 'Ord'
-- instance.  O((m + n) log (m + n)) complexity.
unionOrd :: Ord a => [a] -> [a] -> [a]
unionOrd as bs = ordNub (as <> bs)

-- | Find a key in a map by testing for equality using some projection.
-- O(log n) assuming the projection function is O(1).
--
-- N.B.: It is required that the projection function preserves the order of the
-- Map, that is, for all keys a < b < c < ... in the map, it is required that
-- f a < f b < f c < ....
findKey :: (Ord k, Ord k') => (k -> k') -> k' -> MI.Map k v -> Maybe (k, v)
findKey _ _ MI.Tip = Nothing
findKey f x (MI.Bin _ k v l r) = case compare x (f k) of
  LT -> findKey f x l
  EQ -> Just (k, v)
  GT -> findKey f x r

-- | Apply a pure transformation to every string in some JSON value.
mapJsonText :: (Text -> Text) -> Aeson.Value -> Aeson.Value
mapJsonText f = runIdentity . traverseJsonText (Identity . f)

-- | Apply an applicative transformation to every string in some JSON value.
traverseJsonText :: Applicative f => (Text -> f Text) -> Aeson.Value -> f Aeson.Value
traverseJsonText f = \case
  Aeson.Object obj ->
    let toKeyFunction g x = Key.fromText <$> g (Key.toText x) in
    Aeson.Object . KM.fromList <$> traverse (bitraverse (toKeyFunction f) (traverseJsonText f)) (KM.toList obj)
  Aeson.Array arr -> Aeson.Array <$> traverse (traverseJsonText f) arr
  Aeson.String str -> Aeson.String <$> f str
  n@(Aeson.Number _) -> pure n
  b@(Aeson.Bool _) -> pure b
  Aeson.Null -> pure Aeson.Null

-- | A flipped version of @(<\<$\>>)@.
(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
a <<&>> f = fmap (fmap f) a
{-# INLINE (<<&>>) #-}

-- | Throws an error if the tree contains any subtrees such that the ranges are
-- not smaller than its parent nodes, or returns the tree unmodified, otherwise.
--
-- The error might be useful for debugging, as it will include the offending
-- ranges.
--
-- Warning: Use only for debugging.
validate :: (Functor f, Lattice a, Show a, PrettyShow a) => Cofree f a -> Cofree f a
validate (info :< tree) = info :< fmap (go info) tree
  where
    go info' (info'' :< tree')
      | info'' `leq` info' = info'' :< fmap (go info'') tree'
      | otherwise = error $ show info'' <> " ≰ " <> show info'

-- | Decodes lazy @ByteString@ to strict @Text@.
lazyBytesToText :: LByteString -> Text
lazyBytesToText = toText . TL.decodeUtf8

-- | Encodes strict @Text@ to lazy @ByteString@.
textToLazyBytes :: Text -> LByteString
textToLazyBytes = TL.encodeUtf8 . fromStrict

-- | A render mode for @Buildable@ values.
rmode' :: (Buildable a) => RMode a
rmode' = RMode pretty

-- | Sometimes numbers are carried as strings in order to fit into
-- common limits for sure.
newtype TextualNumber a = TextualNumber { unTextualNumber :: a }
  deriving stock Functor
  deriving newtype NFData

instance Integral a => FromJSON (TextualNumber a) where
  parseJSON = \case
    Aeson.String t -> do
      i <- readMaybe @Integer (toString t)
        & maybe (fail "expected a number") pure
      fromIntegralNoOverflow i
        & either (fail . displayException) (pure . TextualNumber)
    Aeson.Number n -> do
      unless (Sci.isInteger n) $
        fail "Expected an integer number"
      let i :: Integer = round n
      fromIntegralNoOverflow i
        & either (fail . displayException) (pure . TextualNumber)
    other -> Aeson.unexpected other

instance Integral a => MessagePack (TextualNumber a) where
  toObject _ = const ObjectNil
  fromObjectWith
    :: forall m
     . (MonadValidate DecodeError m)
    => Config
    -> Object
    -> m (TextualNumber a)
  fromObjectWith _ = \case
    ObjectStr t -> do
      i <- readMaybe @Integer (toString t)
        & maybe (refute "expected a number") pure
      castNumber i
    ObjectInt n -> castNumber n
    ObjectWord n -> castNumber n
    _ -> refute "Unexpected type"
    where
      castNumber :: (Integral b) => b -> m (TextualNumber a)
      castNumber n = fromIntegralNoOverflow n
        & either (refute . decodeError . displayException) (pure . TextualNumber)

instance {-# OVERLAPPABLE #-} (FromDoc a) => FromBuilder a where
  fromBuilder = pretty
