
{- | The AST and auxillary types along with their pretty-printers.

     The comments for fields in types are the type before it was made untyped.
-}

module AST.Skeleton where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import Duplo.Pretty
import Duplo.Tree
import Duplo.Error

-- | The AST for Pascali... wait. It is, em, universal one.
--
--   TODO: Rename; add stuff if CamlLIGO/ReasonLIGO needs something.
--
type LIGO = Tree RawLigoList

type RawLigoList =
  [ Name, Path, QualifiedName, Pattern, Constant, FieldAssignment, Assignment
  , MapBinding, LHS, Alt, Expr, TField, Variant, Type, Mutable, VarDecl, Binding
  , RawContract, TypeName, FieldName, Language
  , Err Text, Parameters, Ctor, Contract, ReasonExpr
  ]

-- | ReasonLigo specific expressions
data ReasonExpr it
  -- TODO: Block may not need Maybe since last expr may be always `return`
  = Block [it] (Maybe it) -- [Declaration] (Return)
  deriving (Show) via PP (ReasonExpr it)
  deriving stock (Functor, Foldable, Traversable)

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
  | Function     Bool it [it] (Maybe it) it    -- ^ (Name) (Parameters) (Type) (Expr)
  | Var          it (Maybe it) it            -- ^ (Name) (Type) (Expr)
  | Const        it (Maybe it) it            -- ^ (Name) (Type) (Expr)
  | TypeDecl     it it               -- ^ (Name) (Type)
  | Attribute    it                  -- ^ (Name)
  | Include      it
  deriving (Show) via PP (Binding it)
  deriving stock (Functor, Foldable, Traversable)

data Parameters it
  = Parameters [it]
  deriving (Show) via PP (Parameters it)
  deriving stock (Functor, Foldable, Traversable)

data VarDecl it
  = Decl         it it (Maybe it)  -- ^ (Mutable) (Name) (Type)
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
  | TApply    it [it]  -- ^ (Name) [Type]
  | TString   Text     -- ^ (TString)
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
  | Indexing  it it -- (QualifiedName) (Expr)
  | Case      it [it]                  -- (Expr) [Alt]
  | Skip
  | ForLoop   it it it (Maybe it) it              -- (Name) (Expr) (Expr) (Expr)
  | WhileLoop it it                    -- (Expr) (Expr)
  | Seq       [it]                     -- [Declaration]
  | Lambda    [it] (Maybe it) it               -- [VarDecl] (Maybe (Type)) (Expr)
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
  | Spread it -- (Name)
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
  | IsAnnot      it it -- (Pattern) (Type) -- Semantically `Var`
  | IsWildcard
  | IsSpread     it   -- (Name)
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

sexpr :: Text -> [Doc] -> Doc
sexpr header items = "(" <.> pp header `indent` foldr above empty items <.> ")"

sop :: Doc -> Text -> [Doc] -> Doc
sop a op b = "(" <.> a `indent` pp op `indent` foldr above empty b <.> ")"

instance Pretty1 Language where
  pp1 = \case
    Language _ p -> p

instance Pretty1 Undefined where
  pp1 = \case
    Undefined mess -> "{{{" <.> pp (Text.take 20 mess) <.> "}}}"

instance Pretty1 Contract where
  pp1 = \case
    ContractEnd -> "(endtract)"
    ContractCons x xs -> sexpr "constract" [x, xs]

instance Pretty1 RawContract where
  pp1 = \case
    RawContract xs -> sexpr "contract" xs

instance Pretty1 Binding where
  pp1 = \case
    Irrefutable  pat  expr     -> sexpr "irref" [pat, expr]
    TypeDecl     n    ty       -> sexpr "type"  [n, ty]
    Var          name ty value -> sexpr "var"   [name, pp ty, value]
    Const        name ty body  -> sexpr "const" [name, pp ty, body]
    Attribute    name          -> sexpr "attr"  [name]
    Include      fname         -> sexpr "#include" [fname]

    Function isRec name params ty body ->
      sexpr "fun" $ concat
        [ ["rec" | isRec]
        , [name]
        , params
        , [":", pp ty]
        , ["=", body]
        ]

instance Pretty1 Parameters where
  pp1 = \case
    Parameters them -> sexpr "params" them

instance Pretty1 VarDecl where
  pp1 = \case
    Decl mutability name ty -> sexpr "decl" [mutability, name, pp ty]

instance Pretty1 Mutable where
  pp1 = \case
    Mutable          -> "var"
    Immutable        -> "const"

instance Pretty1 Type where
  pp1 = \case
    TArrow    dom codom -> sop dom "->" [codom]
    TRecord   fields    -> sexpr "RECORD" fields
    TVar      name      -> name
    TSum      variants  -> sexpr "SUM" variants
    TProduct  elements  -> sexpr "PROD" elements
    TApply    f xs      -> sop f "$" xs
    TString   t         -> pp t
    TTuple    xs        -> sexpr "TUPLE" xs
    TOr       l n r m   -> sexpr "OR"   [l, n, r, m]
    TAnd      l n r m   -> sexpr "AND" [l, n, r, m]

instance Pretty1 Variant where
  pp1 = \case
    Variant ctor ty -> sexpr "ctor" [ctor, pp ty]

instance Pretty1 ReasonExpr where
  pp1 = \case
    Block decls ret -> sexpr "block" $ decls ++ [pp ret]

instance Pretty1 Expr where
  pp1 = \case
    Let       decl body  -> sexpr "let" [decl, body]
    Apply     f xs       -> sexpr "apply" (f : xs)
    Constant  constant   -> constant
    Ident     qname      -> qname
    BinOp     l o r      -> sop l (ppToText o) [r]
    UnOp        o r      -> sexpr (ppToText o) [r]
    Op          o        -> pp o
    Record    az         -> sexpr "record" az
    If        b t e      -> sexpr "if" [b, t, pp e]
    Assign    l r        -> sop l ":=" [r]
    List      l          -> sexpr "list" l
    ListAccess l ids     -> sexpr "get" (l : ids)
    Set       l          -> sexpr "set" l
    Tuple     l          -> sexpr "tuple" l
    Annot     n t        -> sop n ":" [t]
    Attrs     ts         -> sexpr "attrs" ts
    BigMap    bs         -> sexpr "big_map" bs
    Map       bs         -> sexpr "map" bs
    MapRemove k m        -> sexpr "remove_map" [k, m]
    SetRemove k s        -> sexpr "remove_set" [k, s]
    Indexing  a j        -> sexpr "index" [a, j]
    Case      s az       -> sexpr "case" (s : az)
    Skip                 -> "skip"
    ForLoop   j s f d b  -> sexpr "for" [j, s, f, pp d, b]
    ForBox    k mv t z b -> sexpr "for_box" [k, pp mv, pp t, z, b]
    WhileLoop f b        -> sexpr "while" [f, b]
    Seq       es         -> sexpr "seq" es
    Lambda    ps ty b    -> sexpr "lam" $ concat [ps, [":", pp ty], ["=>", b]]
    MapPatch  z bs       -> sexpr "patch" (z : bs)
    SetPatch  z bs       -> sexpr "patch_set" (z : bs)
    RecordUpd r up       -> sexpr "update" (r : up)

instance Pretty1 Alt where
  pp1 = \case
    Alt p b -> sexpr "alt" [p, b]

instance Pretty1 MapBinding where
  pp1 = \case
    MapBinding k v -> sexpr "bind" [k, v]

instance Pretty1 Assignment where
  pp1 = \case
    Assignment n e -> sexpr "assign" [n, e]

instance Pretty1 FieldAssignment where
  pp1 = \case
    FieldAssignment n e -> sexpr ".=" [n, e]
    Spread n -> sexpr "..." [n]

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
    QualifiedName src path -> sexpr "." (src : path)

instance Pretty1 Pattern where
  pp1 = \case
    IsConstr     ctor arg  -> sexpr "ctor?" [ctor, pp arg]
    IsConstant   z         -> sexpr "is?" [z]
    IsVar        name      -> sexpr "?" [name]
    IsCons       h t       -> sop h "::?" [t]
    IsAnnot      s t       -> sexpr "type?" [s, t]
    IsWildcard             -> "_?"
    IsSpread     n         -> "..." <.> pp n
    IsList       l         -> sexpr "list?" l
    IsTuple      t         -> sexpr "tuple?" t

instance Pretty1 Name where
  pp1 = \case
    Name         raw -> pp raw

instance Pretty1 TypeName where
  pp1 = \case
    TypeName     raw -> pp raw

instance Pretty1 FieldName where
  pp1 = \case
    FieldName    raw -> color 4 $ pp raw

instance Pretty1 Ctor where
  pp1 = \case
    Ctor         raw -> color 5 $ pp raw

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
