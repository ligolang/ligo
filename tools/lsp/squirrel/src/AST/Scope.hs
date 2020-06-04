
{- | /The/ scope resolution system.
-}

module AST.Scope
  ( -- * Monad
    ScopeM
  , evalScopeM

    -- * Scope
  , Env(..)
  , ScopedDecl(..)
  , Kind(..)

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

import Parser
import Range
import AST.Types
import Tree
import HasComments
import Pretty

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

-- | The type/value declaration.
data ScopedDecl = ScopedDecl
  { _sdName   :: (Pascal ())
  , _sdOrigin :: Range
  , _sdBody   :: Maybe Range
  , _sdType   :: Maybe (Either (Pascal ()) Kind)
  }

-- | The kind.
data Kind = Star

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

-- data Contract it
--   = Contract      [it]
--   deriving (Show) via PP (Contract it)
--   deriving stock (Functor, Foldable, Traversable)

instance HasRange a => UpdateOver ScopeM Declaration (Pascal a) where
  before = \case
    TypeDecl ty body -> defType ty Star body
    _ -> skip

-- data Declaration it
--   = ValueDecl it     -- Binding
--   | TypeDecl  it it  -- Name Type
--   | Action    it     -- Expr
--   | Include   Text
--   deriving (Show) via PP (Declaration it)
--   deriving stock (Functor, Foldable, Traversable)

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

-- data Binding it
--   = Irrefutable  it it -- (Pattern) (Expr)
--   | Function     Bool it [it] it it -- (Name) [VarDecl] (Type) (Expr)
--   | Var          it it it -- (Name) (Type) (Expr)
--   | Const        it it it -- (Name) (Type) (Expr)
--   deriving (Show) via PP (Binding it)
--   deriving stock (Functor, Foldable, Traversable)

instance HasRange a => UpdateOver ScopeM VarDecl (Pascal a) where
  after  (Decl _ name ty) = def name (Just ty) Nothing

-- data VarDecl it
--   = Decl         it it it -- (Mutable) (Name) (Type)
--   deriving (Show) via PP (VarDecl it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM Mutable (Pascal a)

-- data Mutable it
--   = Mutable
--   | Immutable
--   deriving (Show) via PP (Mutable it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM Type (Pascal a)

-- data Type it
--   = TArrow    it it -- (Type) (Type)
--   | TRecord   [it] -- [TField]
--   | TVar      it -- (Name)
--   | TSum      [it] -- [Variant]
--   | TProduct  [it] -- [Type]
--   | TApply    it [it] -- (Name) [Type]
--   deriving (Show) via PP (Type it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM Variant (Pascal a)

-- data Variant it
--   = Variant it (Maybe it) -- (Name) (Maybe (Type))
--   deriving (Show) via PP (Variant it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM TField (Pascal a)

-- data TField it
--   = TField it it -- (Name) (Type)
--   deriving (Show) via PP (TField it)
--   deriving stock (Functor, Foldable, Traversable)

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

-- -- | TODO: break onto smaller types? Literals -> Constannt; mapOps; mmove Annots to Decls.
-- data Expr it
--   = Let       [it] it -- [Declaration] (Expr)
--   | Apply     it [it] -- (Expr) [Expr]
--   | Constant  it -- (Constant)
--   | Ident     it -- (QualifiedName)
--   | BinOp     it Text it -- (Expr) Text (Expr)
--   | UnOp      Text it -- (Expr)
--   | Record    [it] -- [Assignment]
--   | If        it it it -- (Expr) (Expr) (Expr)
--   | Assign    it it -- (LHS) (Expr)
--   | List      [it] -- [Expr]
--   | Set       [it] -- [Expr]
--   | Tuple     [it] -- [Expr]
--   | Annot     it it -- (Expr) (Type)
--   | Attrs     [Text]
--   | BigMap    [it] -- [MapBinding]
--   | Map       [it] -- [MapBinding]
--   | MapRemove it it -- (Expr) (QualifiedName)
--   | SetRemove it it -- (Expr) (QualifiedName)
--   | Indexing  it it -- (QualifiedName) (Expr)
--   | Case      it [it] -- (Expr) [Alt]
--   | Skip
--   | ForLoop   it it it it -- (Name) (Expr) (Expr) (Expr)
--   | WhileLoop it it -- (Expr) (Expr)
--   | Seq       [it] -- [Declaration]
--   | Lambda    [it] it it -- [VarDecl] (Type) (Expr)
--   | ForBox    it (Maybe it) Text it it -- (Name) (Maybe (Name)) Text (Expr) (Expr)
--   | MapPatch  it [it] -- (QualifiedName) [MapBinding]
--   | SetPatch  it [it] -- (QualifiedName) [Expr]
--   | RecordUpd it [it] -- (QualifiedName) [FieldAssignment]
--   deriving (Show) via PP (Expr it)
--   deriving stock (Functor, Foldable, Traversable)

instance HasRange a => UpdateOver ScopeM Alt (Pascal a) where
  before _ = enter
  after  _ = leave

-- data Alt it
--   = Alt it it -- (Pattern) (Expr)
--   deriving (Show) via PP (Alt it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM LHS (Pascal a)

-- data LHS it
--   = LHS it (Maybe it) -- (QualifiedName) (Maybe (Expr))
--   deriving (Show) via PP (LHS it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM MapBinding (Pascal a)

-- data MapBinding it
--   = MapBinding it it -- (Expr) (Expr)
--   deriving (Show) via PP (MapBinding it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM Assignment (Pascal a)

-- data Assignment it
--   = Assignment it it -- (Name) (Expr)
--   deriving (Show) via PP (Assignment it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM FieldAssignment (Pascal a)

-- data FieldAssignment it
--   = FieldAssignment it it -- (QualifiedName) (Expr)
--   deriving (Show) via PP (FieldAssignment it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM Constant (Pascal a)

-- data Constant it
--   = Int     Text
--   | Nat     Text
--   | String  Text
--   | Float   Text
--   | Bytes   Text
--   | Tez     Text
--   deriving (Show) via PP (Constant it)
--   deriving stock (Functor, Foldable, Traversable)

instance HasRange a => UpdateOver ScopeM Pattern (Pascal a) where
  before = \case
    IsVar n -> def n Nothing Nothing
    _       -> skip

-- data Pattern it
--   = IsConstr     it (Maybe it) -- (Name) (Maybe (Pattern))
--   | IsConstant   it -- (Constant)
--   | IsVar        it -- (Name)
--   | IsCons       it it -- (Pattern) (Pattern)
--   | IsWildcard
--   | IsList       [it] -- [Pattern]
--   | IsTuple      [it] -- [Pattern]
--   deriving (Show) via PP (Pattern it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM QualifiedName (Pascal a)

-- data QualifiedName it
--   = QualifiedName
--     { qnSource :: it -- Name
--     , qnPath   :: [it] -- [Path]
--     }
--   deriving (Show) via PP (QualifiedName it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM Path (Pascal a)

-- data Path it
--   = At it -- (Name)
--   | Ix Text
--   deriving (Show) via PP (Path it)
--   deriving stock (Functor, Foldable, Traversable)

instance UpdateOver ScopeM Name (Pascal a)

-- data Name it = Name
--   { _raw     :: Text
--   }
--   deriving (Show) via PP (Name it)
--   deriving stock (Functor, Foldable, Traversable)

data Scope = Scope { unScope :: Text }

instance HasComments Scope where
  getComments = pure . ("(* " <>) . (<> " *)") . unScope

_testUpdate :: Pascal ASTInfo -> ScopeM (Pascal Scope)
_testUpdate = updateTree \_ -> do
  Env topmost <- gets head
  let names = _sdName <$> topmost
  let res   = ppToText $ fsep $ map pp names
  return $ Scope res