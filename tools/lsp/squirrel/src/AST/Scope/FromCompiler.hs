{-# LANGUAGE RecordWildCards #-}

module AST.Scope.FromCompiler where

import Control.Category ((>>>))
import Data.Function (on)
import Data.HashMap.Strict ((!))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Duplo.Lattice
import Duplo.Tree (make, only)

import AST.Scope.Common
import AST.Scope.ScopedDecl (DeclarationSpecifics (..), ScopedDecl (..), ValueDeclSpecifics (..))
import AST.Scope.ScopedDecl.Parser (parseTypeDeclSpecifics)
import AST.Skeleton (Lang, SomeLIGO (..))
import Cli
import Product
import Range
import Util (removeDots)

data FromCompiler

-- FIXME: Two things need to be fixed here:
-- 1. If one contract throws an exception, the entire thing will fail. Standard
-- scopes will use Fallback.
-- 2. Performance should be improved. This is O(n²) and ideally we should be
-- able to do something smarter. Maybe unjoining scopes for decls in different
-- files for children or simply calling each contract asynchronously.
instance HasLigoClient m => HasScopeForest FromCompiler m where
  scopeForest = traverseAM \(FindContract ast (SomeLIGO dialect _) msg) -> do
    (defs, _) <- getLigoDefinitions ast
    pure $ FindContract ast (fromCompiler dialect defs) msg

-- | Extract `ScopeForest` from LIGO scope dump.
--
fromCompiler :: Lang -> LigoDefinitions -> ScopeForest
fromCompiler dialect (LigoDefinitions decls scopes) =
    foldr (buildTree decls) (ScopeForest [] Map.empty) scopes
  where
    -- For a new scope to be injected, grab its range and decl and start
    -- injection process.
    --
    buildTree :: LigoDefinitionsInner -> LigoScope -> ScopeForest -> ScopeForest
    buildTree (LigoDefinitionsInner decls') (LigoScope r es _) = do
      let ds = Map.fromList $ map (fromLigoDecl . (decls' !)) es
      let rs = Map.keysSet ds
      let r' = normalizeRange $ fromLigoRangeOrDef r
      injectScope (make (rs :> r' :> Nil, []), ds)

    normalizeRange :: Range -> Range
    normalizeRange r = r { rFile = removeDots (rFile r) }

    -- LIGO compiler provides nor comment neither refs, so they left [].
    --
    fromLigoDecl :: LigoDefinitionScope -> (DeclRef, ScopedDecl)
    fromLigoDecl (LigoDefinitionScope n orig bodyR ty _) = do
      let r = normalizeRange $ fromLigoRangeOrDef orig
      ( DeclRef n r
       , ScopedDecl n r [] [] dialect (ValueSpec vspec) -- TODO LIGO-90
       )
      where
        _vdsInitRange = mbFromLigoRange bodyR
        _vdsParams = Nothing
        _vdsTspec = parseTypeDeclSpecifics . fromLigoTypeFull <$> ty
        vspec = ValueDeclSpecifics{ .. }

    -- Find a place for a scope inside a ScopeForest.
    --
    injectScope :: (ScopeTree, Map DeclRef ScopedDecl) -> ScopeForest -> ScopeForest
    injectScope (subject, ds') (ScopeForest forest ds) =
        ScopeForest (loop forest) (ds <> ds')
      where
        loop
          = withListZipper
          $ find (subject `isCoveredBy`) >>> atLocus maybeLoop

        isCoveredBy = leq `on` getRange

        -- If there are no trees above subject here, just put it in.
        -- Otherwise, put it in a tree that covers it.
        --
        maybeLoop :: Maybe ScopeTree -> Maybe ScopeTree
        maybeLoop = Just . maybe subject restart

        -- Take a forest out of tree, loop, put it back.
        --
        restart (only -> (r, trees)) = make (r, loop trees)

data ListZipper a = ListZipper
  { before :: [a]
  , after  :: [a]
  }

withListZipper :: (ListZipper a -> ListZipper b) -> [a] -> [b]
withListZipper f = close . f . open
  where
    open :: [a] -> ListZipper a
    open = ListZipper []

    close :: ListZipper a -> [a]
    close (ListZipper b a) = reverse b ++ a

next :: ListZipper a -> ListZipper a
next (ListZipper b a) = case a of
  locus : after -> ListZipper (locus : b) after
  _             -> ListZipper b a

here :: ListZipper a -> Maybe a
here (ListZipper _ a) = listToMaybe a

-- | Navigate to next point that succeeds (or to the end).
--
find :: (a -> Bool) -> ListZipper a -> ListZipper a
find prop = go
  where
    go lz = case here lz of
      Nothing -> lz
      Just (prop -> True) -> lz
      _ -> go (next lz)

-- | Like `Data.Map.alter`, but for lists.
--
atLocus :: (Maybe a -> Maybe a) -> ListZipper a -> ListZipper a
atLocus f (ListZipper b a) = case a of
  locus : after -> ListZipper b (maybeToList (f (Just locus)) ++ after)
  _             -> ListZipper b (maybeToList (f Nothing))
