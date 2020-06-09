
{- | /The/ scope resolution system.
-}

module AST.Scope
  ( -- * Monad
    ScopeM
  , evalScopeM
  , pinEnv

    -- * Scope
  , Env(..)
  , ScopedDecl(..)
  , Kind(..)
  , HasEnv(..)
  , lookupEnv

    -- * Methods
  , enter
  , leave
  , define
  , defType
  , def
  )
  where

import Control.Monad.State

import Data.Text (Text)

import Range
import AST.Types
import Tree
import Comment
import Pretty
import Product

-- | Scope-holding monad.
type ScopeM = State [Env]

-- | Run the computation with scope starting from empty scope.
evalScopeM :: ScopeM a -> a
evalScopeM action = evalState action [Env []]

-- | The environment.
newtype Env = Env
  { _eDecls :: [ScopedDecl]
  }
  deriving newtype (Semigroup, Monoid)
  deriving Show via PP Env

instance Pretty Env where
  pp = vcat . map pp . _eDecls

-- | The type/value declaration.
data ScopedDecl = ScopedDecl
  { _sdName   :: (Pascal ())
  , _sdOrigin :: Range
  , _sdBody   :: Maybe Range
  , _sdType   :: Maybe (Either (Pascal ()) Kind)
  }

instance Pretty ScopedDecl where
  pp (ScopedDecl n o b t) = pp o <+> "-" <+> (pp n <> ":") <+> maybe "?" (either pp pp) t <+> "=" <+> pp o

-- | The kind.
data Kind = Star
  deriving Show via PP Kind

instance Pretty Kind where
  pp _ = "*"

lookupEnv :: Pascal () -> Env -> Maybe ScopedDecl
lookupEnv name = go . _eDecls
  where
    go (sd@(ScopedDecl {_sdName}) : rest)
      | ppToText _sdName == ppToText name = Just sd
      | otherwise       = go rest
    go _ = Nothing

-- | Make a new scope out of enclosing parent one.
enter  :: ScopeM ()
enter = modify \(a : b) -> a : a : b

-- | Leave current scope, return to parent one.
leave  :: ScopeM ()
leave = modify tail

-- | Add a declaration to the current scope.
define :: ScopedDecl -> ScopeM ()
define d = modify \(Env a : b) -> Env (d : a) : b

-- | Add a type declaration to the current scope.
defType :: HasRange a => Pascal a -> Kind -> Pascal a -> ScopeM ()
defType name kind body = do
  define $ ScopedDecl
    (void name)
    (getRange $ infoOf name)
    (Just $ getRange $ infoOf body)
    (Just (Right kind))

-- | Add a value declaration to the current scope.
def
  :: HasRange a
  => Pascal a
  -> Maybe (Pascal a)
  -> Maybe (Pascal a)
  -> ScopeM ()
def name ty body = do
  define $ ScopedDecl
    (void name)
    (getRange $ infoOf name)
    ((getRange . infoOf) <$> body)
    ((Left . void) <$> ty)

instance UpdateOver ScopeM Contract (Pascal a)

instance HasRange a => UpdateOver ScopeM Declaration (Pascal a) where
  before = \case
    TypeDecl ty body -> defType ty Star body
    _ -> skip

instance HasRange a => UpdateOver ScopeM Binding (Pascal a) where
  before = \case
    Function recur name _args ty body -> do
      when recur do
        def name (Just ty) (Just body)
      enter

    _ -> enter

  after = \case
    Irrefutable name    body -> do leave; def name  Nothing  (Just body)
    Var         name ty body -> do leave; def name (Just ty) (Just body)
    Const       name ty body -> do leave; def name (Just ty) (Just body)
    Function recur name _args ty body -> do
      leave
      unless recur do
        def name (Just ty) (Just body)

instance HasRange a => UpdateOver ScopeM VarDecl (Pascal a) where
  after  (Decl _ name ty) = def name (Just ty) Nothing

instance UpdateOver ScopeM Mutable (Pascal a)
instance UpdateOver ScopeM Type    (Pascal a)
instance UpdateOver ScopeM Variant (Pascal a)
instance UpdateOver ScopeM TField  (Pascal a)

instance HasRange a => UpdateOver ScopeM Expr (Pascal a) where
  before = \case
    Let    {} -> enter
    Lambda {} -> enter
    ForLoop k _ _ _ -> do
      enter
      def k Nothing Nothing

    ForBox k mv _ _ _ -> do
      enter
      def k Nothing Nothing
      maybe skip (\v -> def v Nothing Nothing) mv

    _ -> skip

  after = \case
    Let     {} -> leave
    Lambda  {} -> leave
    ForLoop {} -> leave
    ForBox  {} -> leave
    _ -> skip

instance HasRange a => UpdateOver ScopeM Alt (Pascal a) where
  before _ = enter
  after  _ = leave

instance UpdateOver ScopeM LHS             (Pascal a)
instance UpdateOver ScopeM MapBinding      (Pascal a)
instance UpdateOver ScopeM Assignment      (Pascal a)
instance UpdateOver ScopeM FieldAssignment (Pascal a)
instance UpdateOver ScopeM Constant        (Pascal a)

instance HasRange a => UpdateOver ScopeM Pattern (Pascal a) where
  before = \case
    IsVar n -> def n Nothing Nothing
    _       -> skip

instance UpdateOver ScopeM QualifiedName (Pascal a)
instance UpdateOver ScopeM Path          (Pascal a)
instance UpdateOver ScopeM Name          (Pascal a)

class HasEnv a where
  getEnv :: a -> Env

instance HasEnv Env where
  getEnv = id

instance Contains Env xs => HasEnv (Product xs) where
  getEnv = getElem

data Scope = Scope { unScope :: [Text] }

instance HasComments Scope where
  getComments = unScope

pinEnv :: Product xs -> ScopeM (Product (Env : xs))
pinEnv xs = (`Cons` xs) <$> gets head