
{- | The AST and auxillary types along with their pretty-printers.

     The comments for fields in types are the type before it was made untyped.
-}

module AST.Skeleton where

import Data.Text (Text)

import Duplo.Tree

import Product

-- | The AST for Pascali... wait. It is, em, universal one.
--
--   TODO: Rename; add stuff if CamlLIGO/ReasonLIGO needs something.
--
-- type LIGO        = Tree RawLigoList
type LIGO     xs = Tree RawLigoList (Product xs)
type Tree' fs xs = Tree fs (Product xs)

type RawLigoList =
  [ Name, Path, QualifiedName, Pattern, Constant, FieldAssignment
  , MapBinding, Alt, Expr, TField, Variant, Type, Binding
  , RawContract, TypeName, FieldName
  , Error, Parameters, Ctor, Contract, NameDecl
  ]

data Undefined it
  = Undefined Text
  deriving stock (Functor, Foldable, Traversable)

data Lang
  = Pascal
  | Caml
  | Reason

data Contract it
  = ContractEnd
  | ContractCons it it -- ^ Declaration
  deriving stock (Functor, Foldable, Traversable)

data RawContract it
  = RawContract [it] -- ^ Declaration
  deriving stock (Functor, Foldable, Traversable)

data Binding it
  = Function     Bool it [it] (Maybe it) it    -- ^ (Name) (Parameters) (Type) (Expr)
  | Parameter    it it               -- ^ (Name)
  | Var          it (Maybe it) (Maybe it)            -- ^ (Name) (Type) (Expr)
  | Const        it (Maybe it) (Maybe it)            -- ^ (Name) (Type) (Expr)
  | TypeDecl     it it               -- ^ (Name) (Type)
  | Attribute    it                  -- ^ (Name)
  | Include      it
  deriving stock (Functor, Foldable, Traversable)

data Parameters it
  = Parameters [it]
  deriving stock (Functor, Foldable, Traversable)

data Type it
  = TArrow    it it    -- ^ (Type) (Type)
  | TRecord   [it]     -- ^ [TField]
  | TVar      it       -- ^ (Name)
  | TSum      [it]     -- ^ [Variant]
  | TProduct  [it]     -- ^ [Type]
  | TApply    it it  -- ^ (Name) (Type)
  | TString   Text     -- ^ (TString)
  | TArgs     [it]     -- ^ [Type]
  | TOr       it it it it
  | TAnd      it it it it
  deriving stock (Functor, Foldable, Traversable)

data Variant it
  = Variant it (Maybe it)  -- (Name) (Maybe (Type))
  deriving stock (Functor, Foldable, Traversable)

data TField it
  = TField it it  -- (Name) (Type)
  deriving stock (Functor, Foldable, Traversable)

-- | TODO: break onto smaller types? Literals -> Constant; mapOps; mmove Annots to Decls.
data Expr it
  = Let       it it   -- Declaration Expr
  | Apply     it [it] -- (Expr) [Expr]
  | Constant  it -- (Constant)
  | Ident     it -- (QualifiedName)
  | BinOp     it it it -- (Expr) Text (Expr)
  | UnOp      it it -- (Expr)
  | Op        Text
  | Record    [it] -- [Assignment]
  | If        it it (Maybe it) -- (Expr) (Expr) (Expr)
  | Assign    it it -- (LHS) (Expr)
  | List      [it] -- [Expr]
  | ListAccess it [it] -- (Name) [Indexes]
  | Set       [it] -- [Expr]
  | Tuple     [it] -- [Expr]
  | Annot     it it -- (Expr) (Type)
  | Attrs     [it]
  | BigMap    [it] -- [MapBinding]
  | Map       [it] -- [MapBinding]
  | MapRemove it it -- (Expr) (QualifiedName)
  | SetRemove it it -- (Expr) (QualifiedName)
  | Case      it [it]                  -- (Expr) [Alt]
  | Skip
  | ForLoop   it it it (Maybe it) it              -- (Name) (Expr) (Expr) (Expr)
  | WhileLoop it it                    -- (Expr) (Expr)
  | Seq       [it]                     -- [Declaration]
  | Block     [it]                     -- [Declaration]
  | Lambda    [it] (Maybe it) it               -- [VarDecl] (Maybe (Type)) (Expr)
  | ForBox    it (Maybe it) it it it -- (Name) (Maybe (Name)) Text (Expr) (Expr)
  | MapPatch  it [it] -- (QualifiedName) [MapBinding]
  | SetPatch  it [it] -- (QualifiedName) [Expr]
  | RecordUpd it [it] -- (QualifiedName) [FieldAssignment]
  deriving stock (Functor, Foldable, Traversable)

data Alt it
  = Alt it it -- (Pattern) (Expr)
  deriving stock (Functor, Foldable, Traversable)

data MapBinding it
  = MapBinding it it -- (Expr) (Expr)
  deriving stock (Functor, Foldable, Traversable)

data FieldAssignment it
  = FieldAssignment it it -- (QualifiedName) (Expr)
  | Spread it -- (Name)
  deriving stock (Functor, Foldable, Traversable)

data Constant it
  = Int     Text
  | Nat     Text
  | String  Text
  | Float   Text
  | Bytes   Text
  | Tez     Text
  deriving stock (Functor, Foldable, Traversable)

data Pattern it
  = IsConstr     it (Maybe it) -- (Name) (Maybe (Pattern))
  | IsConstant   it -- (Constant)
  | IsVar        it -- (Name)
  | IsCons       it it -- (Pattern) (Pattern)
  | IsAnnot      it it -- (Pattern) (Type) -- Semantically `Var`
  | IsWildcard
  | IsSpread     it   -- (Name)
  | IsList       [it] -- [Pattern]
  | IsTuple      [it] -- [Pattern]
  deriving stock (Functor, Foldable, Traversable)

data QualifiedName it
  = QualifiedName
    { qnSource ::  it -- Name
    , qnPath   :: [it] -- [Path]
    }
  deriving stock (Functor, Foldable, Traversable)

data Path it
  = At it -- (Name)
  | Ix Text
  deriving stock (Functor, Foldable, Traversable)

newtype Name it = Name
  { _raw     :: Text
  }
  deriving stock (Functor, Foldable, Traversable)

newtype NameDecl it = NameDecl
  { _raw     :: Text
  }
  deriving stock (Functor, Foldable, Traversable)

newtype TypeName it = TypeName Text
  deriving stock (Functor, Foldable, Traversable)

newtype Ctor it = Ctor Text
  deriving stock (Functor, Foldable, Traversable)

newtype FieldName it = FieldName Text
  deriving stock (Functor, Foldable, Traversable)

data Error it = Error Text [it]
  deriving stock (Functor, Foldable, Traversable)
