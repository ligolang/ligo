{- | Continuous location inside the source and utilities.
-}
{-# LANGUAGE DeriveDataTypeable, UndecidableInstances #-}

module Language.LIGO.Range
  ( HasRange(..)
  , LigoPosition (..)
  , Range(..)
  , PreprocessedRange(..)
  , cutOut
  , excluding
  , intersects
  , interval
  , merged
  , point
  , isLigoStdLib

    -- * Lenses
  , rStart
  , rFinish
  , rFile
  , lpLine
  , lpCol
  , rangeLines
  , startLine
  , finishLine
  )
  where

import Unsafe qualified

import Control.Lens (makeLenses)
import Control.MessagePack (withMsgMap, (.:))
import Control.Monad.Validate (MonadValidate (..))
import Data.ByteString qualified as BS
import Data.Data (Data)
import Data.MessagePack (MessagePack, Object (..))
import Data.MessagePack.Types (MessagePack (fromObjectWith, toObject))
import Debug qualified
import Fmt.Buildable (Buildable, build)

import Duplo.Lattice
import Duplo.Pretty

import Morley.Debugger.Core.Common (IsSourceLoc (..), SrcLoc (..))

import Util

point :: Int -> Int -> Range
point l c = Range (LigoPosition l c) (LigoPosition l c) ""

-- | Construct a range spanning a single line `line` from a column
-- `colSt` (inclusively) to `colFin` (exclusively).
interval :: Int -> Int -> Int -> Range
interval line colSt colFin = Range (LigoPosition line colSt) (LigoPosition line colFin) ""

-- | LIGO debug output, when optimizations are disabled, may mention locations
-- referring to LIGO's standard library that defines bindings to every single
-- Michelson instruction.
-- LIGO teams says that we should just ignore such locations.
isLigoStdLib :: FilePath -> Bool
isLigoStdLib path =
  path == ""

-- | Position in a file.
data LigoPosition = LigoPosition
  { _lpLine :: Int
    -- ^ 1-indexed line number
  , _lpCol :: Int
    -- ^ 1-indexed column number
  } deriving stock (Eq, Ord, Generic, Data)
    deriving anyclass (NFData, Hashable)

instance IsSourceLoc LigoPosition where
  toCanonicalLoc (LigoPosition l c) =
    SrcLoc (Unsafe.fromIntegral $ l - 1) (Unsafe.fromIntegral $ c - 1)
  fromCanonicalLoc (SrcLoc l c) =
    LigoPosition (Unsafe.fromIntegral $ l + 1) (Unsafe.fromIntegral $ c + 1)

-- | A continuous location in text. This includes information to the file as
-- seen by the user (i.e.: before preprocessing).
data Range = Range
  { _rStart  :: LigoPosition  -- ^ [Start: line, col, byte-offset...
  , _rFinish :: LigoPosition  -- ^ ... End: line, col, byte-offset).
  , _rFile   :: FilePath
  }
  deriving stock (Generic, Data)
  deriving (Show) via PP Range
  deriving anyclass (NFData, Hashable)

rangeLines :: Traversal' Range Int
rangeLines f (Range (LigoPosition sl sc) (LigoPosition fl fc) file) =
  Range
    <$> (LigoPosition <$> f sl <*> pure sc)
    <*> (LigoPosition <$> f fl <*> pure fc)
    <*> pure file

instance Pretty Range where
  pp (Range (LigoPosition ll lc) (LigoPosition rl rc) f) =
    text f <.> "@"
    <.> int ll <.> ":"
    <.> int lc <.> "-"
    <.> int rl <.> ":"
    <.> int rc

-- TODO: replace @Pretty@ instance with @Buildable@
instance Buildable Range where
  build = Debug.show . pp

instance MessagePack Range where
  toObject _ = const ObjectNil
  fromObjectWith _ = withMsgMap "LIGO location range" \o -> do
    (file, startPos) <- parseLoc =<< o .: "start"
    (file', endPos) <- parseLoc =<< o .: "stop"
    when (file /= file') $
      refute "Can't handle a range spanning through multiple files"
    return $ Range startPos endPos file
    where
      parseLoc = withMsgMap "location" \o -> do
        file <- o .: "file"
        TextualNumber line <- o .: "line"
        TextualNumber col  <- o .: "col"
        when (line < 1 && (not . isLigoStdLib) file) $ refute "Line number is zero"
        return (file, LigoPosition line (col + 1))

-- | Like 'Range', but includes information on the preprocessed range of the
-- file.
newtype PreprocessedRange
  = PreprocessedRange Range
  deriving newtype (Eq, Lattice, Ord, Pretty, Show)

-- | Ability to get range out of something.
class HasRange a where
  getRange :: a -> Range

instance HasRange Range where
  getRange = id

-- | Extract textual representation of given range.
cutOut :: Int -> Int -> ByteString -> Text
cutOut startOffset finishOffset bs =
  decodeUtf8
    $ BS.take (finishOffset - startOffset)
    $ BS.drop startOffset
      bs

excluding :: Range -> Range -> Range
excluding (Range _ s _) (Range _ f t) = Range s f t

merged :: Range -> Range -> Range
merged (Range s _ _) (Range _ f t) = Range s f t

-- | Returns whether the two ranges have some common intersection. Ranges in
-- different files are always considered to not have intersections.
--
-- N.B.: If R1 = [(l1, c1) ... (l2, c2)] and R2 = [(l2, c2) ... (l3, c3)], this
-- function will return 'False'. That is, a single point of intersection is not
-- enough for both of them to be considered equal, since ranges are exclusive in
-- their end points.
intersects :: Range -> Range -> Bool
intersects
  (Range (LigoPosition ll1 lc1) (LigoPosition ll2 lc2) lf)
  (Range (LigoPosition rl1 rc1) (LigoPosition rl2 rc2) rf)
  -- Different files never intersect.
  | lf /= rf = False
  -- If l's start is before or at r's start, it intersects iff its end is after
  -- r's start (but not at, see function's N.B. part).
  | ll1 < rl1 || ll1 == rl1 && lc1 <= rc1 = ll2 > rl1 || ll2 == rl1 && lc2 > rc1
  -- Same as before, but with l and r swapped.
  | rl1 < ll1 || rl1 == ll1 && rc1 <= lc1 = rl2 > ll1 || rl2 == ll1 && rc2 > lc1
  -- Otherwise, the ranges are disjoint.
  | otherwise = False

-- | Inclusion order
instance Lattice Range where
  Range (LigoPosition ll1 lc1) (LigoPosition ll2 lc2) _
    `leq` Range (LigoPosition rl1 rc1) (LigoPosition rl2 rc2) _ =
    (rl1 < ll1 || rl1 == ll1 && rc1 <= lc1) &&
    (rl2 > ll2 || rl2 == ll2 && rc2 >= lc2)

instance Eq Range where
  Range (LigoPosition l c) (LigoPosition r d) f
    == Range (LigoPosition l1 c1) (LigoPosition r1 d1) f1 =
    (l, c, r, d, f) == (l1, c1, r1, d1, f1)

-- | Lexicographic order
instance Ord Range where
  Range (LigoPosition l c) (LigoPosition r d) f
    `compare` Range (LigoPosition l1 c1) (LigoPosition r1 d1) f1 =
    compare l l1 <> compare c c1 <> compare r r1 <> compare d d1 <> compare f f1

makeLenses ''LigoPosition
makeLenses ''Range

startLine :: Lens' Range Int
startLine = rStart . lpLine
{-# INLINE startLine #-}

finishLine :: Lens' Range Int
finishLine = rFinish . lpLine
{-# INLINE finishLine #-}
