module Language.LIGO.Diagnostic
  ( Message (..)
  , MessageDetail (..)
  , Severity (..)
  , filterDiagnostics
  ) where

import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Duplo.Pretty (Pretty (..))

import Language.LIGO.Range (HasRange (..), Range (..), merged)

-- | Represents some diagnostic (error, warning, etc) that may be contained
-- together with some node.
--
-- Note that this is different from @Error@, which is a node by itself, and not
-- something extra that is associated with some node.
data Message = Message
  { mMessage :: MessageDetail
  , mSeverity :: Severity
  , mRange :: Range
  } deriving stock (Eq, Ord, Show)

type instance PrettyShow Message = ()

instance HasRange Message where
  getRange = mRange

data MessageDetail
  = FromLanguageServer Text
  | FromLIGO Text
  | MissingContract FilePath
  | Missing Text
  | Unexpected Text
  | Unrecognized Text
  deriving stock (Eq, Ord, Show)

instance Pretty MessageDetail where
  pp (FromLanguageServer msg) = pp msg
  pp (FromLIGO msg) = pp msg
  pp (MissingContract path) = "Missing contract: " <> pp (toText path)
  pp (Missing src) = "Missing: " <> pp src
  pp (Unexpected src) = "Unexpected: " <> pp src
  pp (Unrecognized src) = "Unrecognized: " <> pp src

data Severity
  = SeverityError
  | SeverityWarning
  deriving stock (Eq, Ord, Show)

groupByIntersections :: HasRange a => [a] -> [NonEmpty a]
groupByIntersections = elems . foldr go Map.empty
  where
    go a regions = case pivotM of
      Nothing -> Map.insert (merged glb lub) conj disj
      Just pivot -> Map.insert r (a <| pivot) regions
      where
        r = getRange a
        (smaller, pivotM, greater) = Map.splitLookup r regions
        -- Find all elements that have intersections to the left and to the
        -- right of the pivot, grouping them into the same bin. Return all
        -- disjoint elements unchanged.
        (left,  leftDisj)  = Map.partitionWithKey (\k _ -> _rFinish k >  _rStart  r) smaller
        (right, rightDisj) = Map.partitionWithKey (\k _ -> _rStart  k <= _rFinish r) greater
        conj = sconcat ((a :| []) :| (elems left <> elems right))
        disj = Map.union leftDisj rightDisj
        -- Find the greatest lower bound (GLB) and least upper bound (LUB).
        glb = maybe r fst $ Map.lookupMax left
        lub = maybe r fst $ Map.lookupMin right

-- | Filters messages according to the following: if there are no `Unexpected`
-- or `Missing` errors, then just returns the list as it is. Otherwise, filters
-- out all `Unrecognized` errors, to avoid excessive cluttering of errors.
--
-- To make the filtering smarter, this is done only if unrecognized
-- errors appear together with unexpected or missing ones.
--
-- Normally, the presence of `Unrecognized` errors without `Unexpected` errors
-- indicate a bug, however, at the time of this writing, `tree-sitter` itself
-- may sometimes successfully parse a contract and indicate no errors. See
-- `test/contracts/bad/unfinished_code09.{,re}ligo` for examples where our
-- recognizers indicate an unrecognized error without an unexpected error.
filterDiagnostics :: [Message] -> [Message]
filterDiagnostics msgs = go =<< groupByIntersections msgs
  where
    go msgs' = case find findMatcher msgs' of
      Nothing -> NE.toList msgs'
      Just _  -> NE.filter (\case Message (Unrecognized _) _ _ -> False; _ -> True) msgs'

    findMatcher = \case
      Message (Unexpected _) _ _ -> True
      Message (Missing _) _ _ -> True
      _ -> False
