module Language.LIGO.Scope
  ( Scope (..)
  , getClosestInnerScope
  ) where

import Fmt.Buildable (Buildable)

import Duplo (leq)

import Language.LIGO.Range

data Scope = Scope
  { sRange :: Range
    -- ^ A range that scope covers.
  , sVariables :: [Text]
    -- ^ Variables that are present in the current scope.
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Buildable)

-- | Returns a scope that satisfies the next rules:
-- 1. Its range is inclusive to the given.
-- 2. There're no scopes in the given list
--    that cover the resulting one.
getClosestInnerScope :: Range -> [Scope] -> Maybe Scope
getClosestInnerScope r scopes = nonEmpty innerScopes
  <&> maximumBy inclusiveComp
  where
    innerScopes = scopes
      & filter ((`leq` r) . sRange)

    inclusiveComp :: Scope -> Scope -> Ordering
    inclusiveComp Scope{ sRange = lhs } Scope{ sRange = rhs }
      | lhs == rhs = EQ
      | lhs `leq` rhs = GT
      | otherwise = LT
