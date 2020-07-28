
{- | The AST and auxillary types along with their pretty-printers.

     The comments for fields in types are the type before it was made untyped.
-}

module AST.Types where

import Data.Text (Text)
import qualified Data.Text as Text

import Duplo.Pretty
import Duplo.Tree
import Duplo.Error

-- import Debug.Trace

-- | The AST for Pascali... wait. It is, em, universal one.
--
--   TODO: Rename; add stuff if CamlLIGO/ReasonLIGO needs something.
--
type LIGO = Tree RawLigoList

type RawLigoList =
  [ Name, Path, QualifiedName, Pattern, Constant, FieldAssignment, Assignment
  , MapBinding, LHS, Alt, Expr, TField, Variant, Type, Mutable, VarDecl, Binding
  , RawContract, TypeName, FieldName, Language
  , Err Text, Parameters, Ctor
  ]

data Undefined it
  = Undefined Text
  deriving (Show) via PP (Undefined it)
  deriving stock (Functor, Foldable, Traversable)

data Language it
  = Language Lang it
  deriving (Show) via PP (Language it)
  deriving stock (Functor, Foldable, Traversable)

data Lang
  = Pascal
  | Caml
  | Reason
  -- deriving (Show) via PP Lang

data Contract it
  = ContractEnd
  | ContractCons it it -- ^ Declaration
  deriving (Show) via PP (Contract it)
  deriving stock (Functor, Foldable, Traversable)

data RawContract it
  = RawContract [it] -- ^ Declaration
  deriving (Show) via PP (RawContract it)
  deriving stock (Functor, Foldable, Traversable)

data Binding it
  = Irrefutable  it it               -- ^ (Pattern) (Expr)
  | Function     Bool it it it it    -- ^ (Name) Parameters (Type) (Expr)
  | Var          it it it            -- ^ (Name) (Type) (Expr)
  | Const        it it it            -- ^ (Name) (Type) (Expr)
  | TypeDecl     it it               -- ^ Name Type
  | Include      it
  deriving (Show) via PP (Binding it)
  deriving stock (Functor, Foldable, Traversable)

data Parameters it
  = Parameters [it]
  deriving (Show) via PP (Parameters it)
  deriving stock (Functor, Foldable, Traversable)

data VarDecl it
  = Decl         it it it  -- ^ (Mutable) (Name) (Type)
  deriving (Show) via PP (VarDecl it)
  deriving stock (Functor, Foldable, Traversable)

data Mutable it
  = Mutable
  | Immutable
  deriving (Show) via PP (Mutable it)
  deriving stock (Functor, Foldable, Traversable)


data Type it
  = TArrow    it it    -- ^ (Type) (Type)
  | TRecord   [it]     -- ^ [TField]
  | TVar      it       -- ^ (Name)
  | TSum      [it]     -- ^ [Variant]
  | TProduct  [it]     -- ^ [Type]
  | TApply    it it  -- (Name) [Type]
  | TTuple    [it]
  | TOr       it it it it
  | TAnd      it it it it
  deriving (Show) via PP (Type it)
  deriving stock (Functor, Foldable, Traversable)

data Variant it
  = Variant it (Maybe it)  -- (Name) (Maybe (Type))
  deriving (Show) via PP (Variant it)
  deriving stock (Functor, Foldable, Traversable)

data TField it
  = TField it it  -- (Name) (Type)
  deriving (Show) via PP (TField it)
  deriving stock (Functor, Foldable, Traversable)

-- | TODO: break onto smaller types? Literals -> Constant; mapOps; mmove Annots to Decls.
data Expr it
  = Let       it it   -- Declaration Expr
  | Apply     it it -- (Expr) [Expr]
  | Constant  it -- (Constant)
  | Ident     it -- (QualifiedName)
  | BinOp     it it it -- (Expr) Text (Expr)
  | UnOp      it it -- (Expr)
  | Op        Text
  | Record    [it] -- [Assignment]
  | If        it it it -- (Expr) (Expr) (Expr)
  | Assign    it it -- (LHS) (Expr)
  | List      [it] -- [Expr]
  | Set       [it] -- [Expr]
  | Tuple     [it] -- [Expr]
  | Annot     it it -- (Expr) (Type)
  | Attrs     [it]
  | BigMap    [it] -- [MapBinding]
  | Map       [it] -- [MapBinding]
  | MapRemove it it -- (Expr) (QualifiedName)
  | SetRemove it it -- (Expr) (QualifiedName)
  | Indexing  it it -- (QualifiedName) (Expr)
  | Case      it [it]                  -- (Expr) [Alt]
  | Skip
  | ForLoop   it it it (Maybe it) it              -- (Name) (Expr) (Expr) (Expr)
  | WhileLoop it it                    -- (Expr) (Expr)
  | Seq       [it]                     -- [Declaration]
  | Lambda    it it it               -- [VarDecl] (Type) (Expr)
  | ForBox    it (Maybe it) it it it -- (Name) (Maybe (Name)) Text (Expr) (Expr)
  | MapPatch  it [it] -- (QualifiedName) [MapBinding]
  | SetPatch  it [it] -- (QualifiedName) [Expr]
  | RecordUpd it [it] -- (QualifiedName) [FieldAssignment]
  deriving (Show) via PP (Expr it)
  deriving stock (Functor, Foldable, Traversable)

data Alt it
  = Alt it it -- (Pattern) (Expr)
  deriving (Show) via PP (Alt it)
  deriving stock (Functor, Foldable, Traversable)

data LHS it
  = LHS it (Maybe it) -- (QualifiedName) (Maybe (Expr))
  deriving (Show) via PP (LHS it)
  deriving stock (Functor, Foldable, Traversable)

data MapBinding it
  = MapBinding it it -- (Expr) (Expr)
  deriving (Show) via PP (MapBinding it)
  deriving stock (Functor, Foldable, Traversable)

data Assignment it
  = Assignment it it -- (Name) (Expr)
  deriving (Show) via PP (Assignment it)
  deriving stock (Functor, Foldable, Traversable)

data FieldAssignment it
  = FieldAssignment it it -- (QualifiedName) (Expr)
  deriving (Show) via PP (FieldAssignment it)
  deriving stock (Functor, Foldable, Traversable)

data Constant it
  = Int     Text
  | Nat     Text
  | String  Text
  | Float   Text
  | Bytes   Text
  | Tez     Text
  deriving (Show) via PP (Constant it)
  deriving stock (Functor, Foldable, Traversable)

data Pattern it
  = IsConstr     it (Maybe it) -- (Name) (Maybe (Pattern))
  | IsConstant   it -- (Constant)
  | IsVar        it -- (Name)
  | IsCons       it it -- (Pattern) (Pattern)
  | IsWildcard
  | IsList       [it] -- [Pattern]
  | IsTuple      [it] -- [Pattern]
  deriving (Show) via PP (Pattern it)
  deriving stock (Functor, Foldable, Traversable)

data QualifiedName it
  = QualifiedName
    { qnSource ::  it -- Name
    , qnPath   :: [it] -- [Path]
    }
  deriving (Show) via PP (QualifiedName it)
  deriving stock (Functor, Foldable, Traversable)

data Path it
  = At it -- (Name)
  | Ix Text
  deriving (Show) via PP (Path it)
  deriving stock (Functor, Foldable, Traversable)

newtype Name it = Name
  { _raw     :: Text
  }
  deriving (Show) via PP (Name it)
  deriving stock (Functor, Foldable, Traversable)

newtype TypeName it = TypeName Text
  deriving (Show) via PP (TypeName it)
  deriving stock   (Functor, Foldable, Traversable)

newtype Ctor it = Ctor Text
  deriving (Show) via PP (Ctor it)
  deriving stock   (Functor, Foldable, Traversable)

newtype FieldName it = FieldName Text
  deriving (Show) via PP (TypeName it)
  deriving stock   (Functor, Foldable, Traversable)

instance Pretty1 Language where
  pp1 = \case
    Language _ p -> p

instance Pretty1 Undefined where
  pp1 = \case
    Undefined mess -> "{{{" <.> pp (Text.take 20 mess) <.> "}}}"

instance Pretty1 Contract where
  pp1 = \case
    ContractEnd -> "(* end *)"
    ContractCons x xs -> x $$ " " $$ xs

instance Pretty1 RawContract where
  pp1 = \case
    RawContract xs -> "(* begin *)" `indent` sparseBlock xs `above` "(* end *)"

instance Pretty1 Binding where
  pp1 = \case
    Irrefutable  pat  expr     -> "irref" <+> pat  <+> "=" `indent` expr
    TypeDecl     n    ty       -> "type"  <+> n    <+> "=" `indent` ty
    Var          name ty value -> "var"   <+> name <+> ":" <+> ty <+> ":=" `indent` value
    Const        name ty body  -> "const" <+> name <+> ":" <+> ty <+>  "=" `indent` body
    Include      fname         -> "#include" <+> fname

    Function isRec name params ty body ->
      (
        (
          (   (if isRec then "recursive" else empty)
          <+> "function"
          <+> name
          )
          `indent` params
        )
        `indent` (":" <+> ty `above` "is")
      )
      `indent` body

instance Pretty1 Parameters where
  pp1 = \case
    Parameters them -> tuple them

instance Pretty1 VarDecl where
  pp1 = \case
    Decl mutability name ty -> mutability <+> name <+> ":" `indent` ty

instance Pretty1 Mutable where
  pp1 = \case
    Mutable          -> "var"
    Immutable        -> "const"

instance Pretty1 Type where
  pp1 = \case
    TArrow    dom codom -> parens (dom `indent` "->" <+> codom)
    TRecord   fields    -> "record [" `indent` block fields `above` "]"
    TVar      name      -> name
    TSum      variants  -> block variants
    TProduct  elements  -> train " *" elements
    TApply    f xs      -> f <+> xs
    TTuple    xs        -> tuple xs
    TOr       l n r m   -> "michelson_or"   <+> tuple [l, n, r, m]
    TAnd      l n r m   -> "michelson_pair" <+> tuple [l, n, r, m]

instance Pretty1 Variant where
  pp1 = \case
    Variant ctor (Just ty) -> "|" <+> ctor <+> "of" `indent` ty
    Variant ctor  _        -> "|" <+> ctor

instance Pretty1 Expr where
  pp1 = \case
    Let       decl body  -> "let" <+> decl `above` body
    Apply     f xs       -> f <+> xs
    Constant  constant   -> constant
    Ident     qname      -> qname
    BinOp     l o r      -> parens (l <+> pp o <+> r)
    UnOp        o r      -> parens (pp o <+> r)
    Op          o        -> pp o
    Record    az         -> "record" <+> list az
    If        b t e      -> fsep ["if" `indent` b, "then" `indent` t, "else" `indent` e]
    Assign    l r        -> l <+> ":=" `indent` r
    List      l          -> "list" <+> list l
    Set       l          -> "set"  <+> list l
    Tuple     l          -> tuple l
    Annot     n t        -> parens (n <+> ":" `indent` t)
    Attrs     ts         -> "attributes" <+> list ts
    BigMap    bs         -> "big_map"    <+> list bs
    Map       bs         ->  "map"       <+> list bs
    MapRemove k m        -> "remove" `indent` k `above` "from" <+> "map" `indent` m
    SetRemove k s        -> "remove" `indent` k `above` "from" <+> "set" `indent` s
    Indexing  a j        -> a <.> list [j]
    Case      s az       -> "case" <+> s <+> "of" `indent` block az
    Skip                 -> "skip"
    ForLoop   j s f d b  -> "for" <+> j <+> ":=" <+> s <+> "to" <+> f <+> mb ("step" <+>) d `indent` b
    ForBox    k mv t z b -> "for" <+> k <+> mb ("->" <+>) mv <+> "in" <+> pp t <+> z `indent` b
    WhileLoop f b        -> "while" <+> f `indent` b
    Seq       es         -> "block {" `indent` block es `above` "}"
    Lambda    ps ty b    -> (("function" `indent` ps) `indent` (":" <+> ty)) `indent` b
    MapPatch  z bs       -> "patch" `indent` z `above` "with" <+> "map" `indent` list bs
    SetPatch  z bs       -> "patch" `indent` z `above` "with" <+> "set" `indent` list bs
    RecordUpd r up       -> r `indent` "with" <+> "record" `indent` list up

instance Pretty1 Alt where
  pp1 = \case
    Alt p b -> "|" <+> p <+> "->" `indent` b

instance Pretty1 MapBinding where
  pp1 = \case
    MapBinding k v -> k <+> "->" `indent` v

instance Pretty1 Assignment where
  pp1 = \case
    Assignment      n e -> n <+> "=" `indent` e

instance Pretty1 FieldAssignment where
  pp1 = \case
    FieldAssignment      n e -> n <+> "=" `indent` e

instance Pretty1 Constant where
  pp1 = \case
    Int           z   -> pp z
    Nat           z   -> pp z
    String        z   -> pp z
    Float         z   -> pp z
    Bytes         z   -> pp z
    Tez           z   -> pp z

instance Pretty1 QualifiedName where
  pp1 = \case
    QualifiedName src path -> src <.> sepByDot path

instance Pretty1 Pattern where
  pp1 = \case
    IsConstr     ctor arg  -> ctor <+> maybe empty id arg
    IsConstant   z         -> z
    IsVar        name      -> name
    IsCons       h t       -> h <+> ("#" <+> t)
    IsWildcard             -> "_"
    IsList       l         -> list l
    IsTuple      t         -> tuple t


instance Pretty1 Name where
  pp1 = \case
    Name         raw -> pp raw

instance Pretty1 TypeName where
  pp1 = \case
    TypeName     raw -> pp raw

instance Pretty1 FieldName where
  pp1 = \case
    FieldName    raw -> pp raw

instance Pretty1 Ctor where
  pp1 = \case
    Ctor         raw -> pp raw

instance Pretty1 Path where
  pp1 = \case
    At n -> n
    Ix j -> pp j

instance Pretty1 TField where
  pp1 = \case
    TField      n t -> n <.> ":" `indent` t

instance Pretty1 LHS where
  pp1 = \case
    LHS    qn mi -> qn <.> foldMap brackets mi
