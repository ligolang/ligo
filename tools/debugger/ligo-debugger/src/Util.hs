module Util
  ( groupByKey
  , ForInternalUse
  , itIsForInternalUse
  , allowedForInternalUseOnly
  , readSemVerQ
  , resourcesFolder
  , rmode'semv
  , rmode'ansi
  , everywhereM'
  , foldMapM
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

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Data.Bitraversable (bitraverse)
import Data.Char qualified as Char
import Data.List (groupBy, singleton)
import Data.Map.Internal qualified as MI
import Data.Reflection (Given, give, given)
import Data.Scientific qualified as Sci
import Data.SemVer qualified as SemVer
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as TL
import Debug qualified
import Fmt (build)
import Generics.SYB (Data (gmapM), GenericM)
import Language.Haskell.TH.Syntax (Code (..), Q, liftTyped)
import System.Console.ANSI (SGR, setSGRCode)
import System.FilePath ((</>))
import TH.RelativePaths (qReadFileText)
import Text.Interpolation.Nyan.Core (RMode (..))

import Duplo (Cofree ((:<)), Lattice (leq))

groupByKey :: Ord k => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupByKey f g =
  extractGroup f g
  . groupBy ((==) `on` f)
  . sortOn f

extractGroup :: (a -> k) -> (a -> v) -> [[a]] -> [(k, [v])]
extractGroup _ _ [] = []
extractGroup f g ([] : xs) = extractGroup f g xs
extractGroup f g (ys@(y : _) : xs) = (f y, g <$> ys) : extractGroup f g xs

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

allowedForInternalUseOnly :: ForInternalUse => a -> a
allowedForInternalUseOnly =
  case given @IsForInternalUse of IsForInternalUse -> id

-- | Read a 'SemVer.Version' from a file at compile time.
--
-- This allows comments in form of lines starting from @#@ sign.
readSemVerQ :: FilePath -> Code Q SemVer.Version
readSemVerQ path = Code do
  content <- qReadFileText path
  let clearContent = content
        & lines
        & filter ((/= "#") . take 1 . toString . Text.dropWhile Char.isSpace)
        & unlines
        & Text.strip

  version <-
    SemVer.fromText clearContent
    & either (\_ -> fail $ "Invalid version constant in the file: " <> Debug.show clearContent) pure

  examineCode . liftTyped $ version

resourcesFolder :: FilePath
resourcesFolder = "src" </> "resources"

rmode'semv :: RMode SemVer.Version
rmode'semv = RMode (build . SemVer.toText)

rmode'ansi :: RMode [SGR]
rmode'ansi = RMode (build . concatMap (setSGRCode . singleton))

-- | Monadic variation on everywhere'
everywhereM' :: forall m. Monad m => GenericM m -> GenericM m
everywhereM' f = go
  where
    -- Up-bottom order is also reflected in order of do-actions
    go :: GenericM m
    go x = do
      x' <- f x
      gmapM go x'

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM folder mempty
  where
    folder !acc new = (acc <>) <$> f new

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

mapJsonText :: (Text -> Text) -> Aeson.Value -> Aeson.Value
mapJsonText f = runIdentity . traverseJsonText (Identity . f)

-- | Apply a transformation to every string in some JSON value.
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


-- | Sometimes numbers are carried as strings in order to fit into
-- common limits for sure.
newtype TextualNumber a = TextualNumber a
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
