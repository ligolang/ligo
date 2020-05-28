
module AST.ScopeHelpers where

import Control.Monad.State

data Define n e ty a = Define
  { recursive :: Bool
  , name :: n a
  , body :: Maybe (e a)
  , ty   :: Maybe (ty a)
  }
  deriving stock (Functor, Foldable, Traversable)

data Block f a = Block
  { body      :: f a
  }
  deriving stock (Functor, Foldable, Traversable)

class UpdatableScopes n e ty f where
  updateScopes :: (a -> ScopeM n e ty (f b)) -> f a -> ScopeM n e ty (f b)

type ScopeM n e ty = State [Env n e ty]

newtype Env n e ty = Env
  { _eDecls :: [ScopedDecl n e ty]
  }
  deriving newtype (Semigroup, Monoid)

data ScopedDecl n e ty = ScopedDecl
  { _sdName   :: n ()
  , _sdBody   :: Maybe (e ())
  , _sdType   :: Maybe (ty ())
  }

data Kind = Star

define :: (Functor n, Functor b, Functor ty) => Define n b ty a -> ScopeM n b ty ()
define (Define _ n e ty) = do
  modify \(Env top : rest) ->
    Env (sd : top) : rest
  where
    sd = ScopedDecl
      (void n)
      (void <$> e)
      (void <$> ty)

instance
  ( UpdatableScopes n e ty n
  , UpdatableScopes n e ty e
  , UpdatableScopes n e ty ty
  )
    =>
  UpdatableScopes n e ty (Define n e ty)
    where
      updateScopes action def@(Define recur name body ty) = do
        define def
        name' <- updateScopes action name
        _
