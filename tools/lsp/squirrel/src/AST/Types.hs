
{-
  The AST and auxillary types along with their pretty-printers.

  TODO: Untangle pretty-printing mess into combinators.
  TODO: Store offending text verbatim in Wrong*.
-}

module AST.Types where

import Control.Monad.State

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void

import Parser
import ParseTree
import Pretty

import Debug.Trace

data Contract info
  = Contract      info [Declaration info]
  | WrongContract      Error
  deriving (Show) via PP (Contract info)

instance Stubbed (Contract info) where stub = WrongContract

data Declaration info
  = ValueDecl info (Binding info)
  | TypeDecl  info (Name info) (Type info)
  | Action    info (Expr info)
  | Include   info Text
  | WrongDecl      Error
  deriving (Show) via PP (Declaration info)

instance Stubbed (Declaration info) where stub = WrongDecl

data Binding info
  = Irrefutable  info (Pattern info) (Expr info)
  | Function     info Bool (Name info) [VarDecl info] (Type info) (Expr info)
  | Var          info (Name info) (Type info) (Expr info)
  | Const        info (Name info) (Type info) (Expr info)
  | WrongBinding      Error
  deriving (Show) via PP (Binding info)

instance Stubbed (Binding info) where stub = WrongBinding

data VarDecl info
  = Decl         info (Mutable info) (Name info) (Type info)
  | WrongVarDecl      Error
  deriving (Show) via PP (VarDecl info)

instance Stubbed (VarDecl info) where stub = WrongVarDecl

data Mutable info
  = Mutable      info
  | Immutable    info
  | WrongMutable      Error
  deriving (Show) via PP (Mutable info)


instance Stubbed (Mutable info) where stub = WrongMutable

data Type info
  = TArrow    info  (Type info) (Type info)
  | TRecord   info [TField info]
  | TVar      info  (Name info)
  | TSum      info [Variant info]
  | TProduct  info  [Type info]
  | TApply    info  (Name info) [Type info]
  | WrongType      Error
  deriving (Show) via PP (Type info)

instance Stubbed (Type info) where stub = WrongType

data Variant info
  = Variant info (Name info) (Maybe (Type info))
  | WrongVariant Error
  deriving (Show) via PP (Variant info)

instance Stubbed (Variant info) where stub = WrongVariant

data TField info
  = TField info (Name info) (Type info)
  | WrongTField Error
  deriving (Show) via PP (TField info)

instance Stubbed (TField info) where stub = WrongTField

-- | TODO: break onto smaller types? Literals -> Constannt; mapOps; mmove Annots to Decls.
data Expr info
  = Let       info [Declaration info] (Expr info)
  | Apply     info (Expr info) [Expr info]
  | Constant  info (Constant info)
  | Ident     info (QualifiedName info)
  | BinOp     info (Expr info) Text (Expr info)
  | UnOp      info Text (Expr info)
  | Record    info [Assignment info]
  | If        info (Expr info) (Expr info) (Expr info)
  | Assign    info (LHS info) (Expr info)
  | List      info [Expr info]
  | Set       info [Expr info]
  | Tuple     info [Expr info]
  | Annot     info (Expr info) (Type info)
  | Attrs     info [Text]
  | BigMap    info [MapBinding info]
  | Map       info [MapBinding info]
  | MapRemove info (Expr info) (QualifiedName info)
  | SetRemove info (Expr info) (QualifiedName info)
  | Indexing  info (QualifiedName info) (Expr info)
  | Case      info (Expr info) [Alt info]
  | Skip      info
  | ForLoop   info (Name info) (Expr info) (Expr info) (Expr info)
  | WhileLoop info (Expr info) (Expr info)
  | Seq       info [Declaration info]
  | Lambda    info [VarDecl info] (Type info) (Expr info)
  | ForBox    info (Name info) (Maybe (Name info)) Text (Expr info) (Expr info)
  | MapPatch  info (QualifiedName info) [MapBinding info]
  | SetPatch  info (QualifiedName info) [Expr info]
  | RecordUpd info (QualifiedName info) [FieldAssignment info]
  | WrongExpr      Error
  deriving (Show) via PP (Expr info)

instance Stubbed (Expr info) where stub = WrongExpr

data Alt info
  = Alt info (Pattern info) (Expr info)
  | WrongAlt Error
  deriving (Show) via PP (Alt info)

instance Stubbed (Alt info) where stub = WrongAlt

data LHS info
  = LHS info (QualifiedName info) (Maybe (Expr info))
  | WrongLHS Error
  deriving (Show) via PP (LHS info)

instance Stubbed (LHS info) where stub = WrongLHS

data MapBinding info
  = MapBinding info (Expr info) (Expr info)
  | WrongMapBinding Error
  deriving (Show) via PP (MapBinding info)

instance Stubbed (MapBinding info) where stub = WrongMapBinding

data Assignment info
  = Assignment info (Name info) (Expr info)
  | WrongAssignment Error
  deriving (Show) via PP (Assignment info)

instance Stubbed (Assignment info) where stub = WrongAssignment

data FieldAssignment info
  = FieldAssignment info (QualifiedName info) (Expr info)
  | WrongFieldAssignment Error
  deriving (Show) via PP (FieldAssignment info)

instance Stubbed (FieldAssignment info) where stub = WrongFieldAssignment

data Constant info
  = Int     info Text
  | Nat     info Text
  | String  info Text
  | Float   info Text
  | Bytes   info Text
  | Tez     info Text
  | WrongConstant Error
  deriving (Show) via PP (Constant info)

instance Stubbed (Constant info) where stub = WrongConstant

data Pattern info
  = IsConstr     info (Name info) (Maybe (Pattern info))
  | IsConstant   info (Constant info)
  | IsVar        info (Name info)
  | IsCons       info (Pattern info) (Pattern info)
  | IsWildcard   info
  | IsList       info [Pattern info]
  | IsTuple      info [Pattern info]
  | WrongPattern      Error
  deriving (Show) via PP (Pattern info)

instance Stubbed (Pattern info) where stub = WrongPattern

data QualifiedName info
  = QualifiedName
    { qnInfo   :: info
    , qnSource :: Name info
    , qnPath   :: [Path info]
    }
  | WrongQualifiedName Error
  deriving (Show) via PP (QualifiedName info)

instance Stubbed (QualifiedName info) where stub = WrongQualifiedName

data Path info
  = At info (Name info)
  | Ix info Text
  | WrongPath Error
  deriving (Show) via PP (Path info)

instance Stubbed (Path info) where stub = WrongPath

data Name info = Name
  { info    :: info
  , raw     :: Text
  }
  | WrongName Error
  deriving (Show) via PP (Name info)

instance Stubbed (Name info) where stub = WrongName

c :: HasComments i => i -> Doc -> Doc
c i d =
  case getComments i of
    [] -> d
    cc -> block (map Text.init cc) $$ d

instance HasComments i => Pretty (Contract i) where
  pp = \case
    Contract i decls -> c i $
      sparseBlock decls

    WrongContract err ->
      pp err

instance HasComments i => Pretty (Declaration i) where
  pp = \case
    ValueDecl i binding -> c i $ pp binding
    TypeDecl  i n ty    -> c i $ "type" <+> pp n <+> "=" `indent` pp ty
    Action    i e       -> c i $ pp e
    Include   i f       -> c i $ "#include" <+> pp f
    WrongDecl err       ->       pp err

instance HasComments i => Pretty (Binding i) where
  pp = \case
    Irrefutable  i pat expr -> error "irrefs in pascaligo?"
    Function     i isRec name params ty body ->
      c i $
        (
          (
            (   (if isRec then "recursive" else empty)
            <+> "function"
            <+> pp name
            )
            `indent` tuple params
          )
         `indent` (":" <+> pp ty <+> "is")
        )
        `indent` pp body
    Var   i name ty value -> c i $ "var"   <+> pp name <+> ":" <+> pp ty <+> ":=" `indent` pp value
    Const i name ty body  -> c i $ "const" <+> pp name <+> ":" <+> pp ty <+>  "=" `indent` pp body
    WrongBinding err ->
      pp err

instance HasComments i => Pretty (VarDecl i) where
  pp = \case
    Decl i mutability name ty -> c i $
      pp mutability <+> pp name <+> ":" `indent` pp ty
    WrongVarDecl err ->
      pp err

instance HasComments i => Pretty (Mutable i) where
  pp = \case
    Mutable      i   -> c i $ "var"
    Immutable    i   -> c i $ "const"
    WrongMutable err -> pp err

instance HasComments i => Pretty (Type i) where
  pp = \case
    TArrow    i dom codom -> c i $ parens (pp dom `indent` "->" <+> pp codom)
    TRecord   i fields    -> c i $ "record" <+> list fields
    TVar      i name      -> c i $ pp name
    TSum      i variants  -> c i $ block variants
    TProduct  i elements  -> c i $ train " *" elements
    TApply    i f xs      -> c i $ pp f <> tuple xs
    WrongType   err       ->       pp err
    where
      ppField (name, ty) = pp name <> ": " <> pp ty <> ";"

instance HasComments i => Pretty (Variant i) where
  pp = \case
    Variant i ctor (Just ty) -> c i $ "|" <+> pp ctor <+> "of" `indent` pp ty
    Variant i ctor  _        -> c i $ "|" <+> pp ctor
    WrongVariant err -> pp err

-- My eyes.
instance HasComments i => Pretty (Expr i) where
  pp = \case
    Let       i decls body -> c i $ "block {" `indent` sparseBlock decls `above` "}" <+> "with" `indent` pp body
    Apply     i f xs       -> c i $ pp f <+> tuple xs
    Constant  i constant   -> c i $ pp constant
    Ident     i qname      -> c i $ pp qname
    BinOp     i l o r      -> c i $ parens (pp l <+> pp o <+> pp r)
    UnOp      i   o r      -> c i $ parens (pp o <+> pp r)
    Record    i az         -> c i $ "record" <+> list az
    If        i b t e      -> c i $ fsep ["if" `indent` pp b, "then" `indent` pp t, "else" `indent` pp e]
    Assign    i l r        -> c i $ pp l <+> ":=" `indent` pp r
    List      i l          -> c i $ "list" <+> list l
    Set       i l          -> c i $ "set"  <+> list l
    Tuple     i l          -> c i $ tuple l
    Annot     i n t        -> c i $ parens (pp n <+> ":" `indent` pp t)
    Attrs     i ts         -> c i $ "attributes" <+> list ts
    BigMap    i bs         -> c i $ "big_map"    <+> list bs
    Map       i bs         -> c i $  "map"       <+> list bs
    MapRemove i k m        -> c i $ "remove" `indent` pp k `above` "from" <+> "map" `indent` pp m
    SetRemove i k s        -> c i $ "remove" `indent` pp k `above` "from" <+> "set" `indent` pp s
    Indexing  i a j        -> c i $ pp a <> list [j]
    Case      i s az       -> c i $ "case" <+> pp s <+> "of" `indent` block az
    Skip      i            -> c i $ "skip"
    ForLoop   i j s f b    -> c i $ "for" <+> pp j <+> ":=" <+> pp s <+> "to" <+> pp f `indent` pp b
    ForBox    i k mv t z b -> c i $ "for" <+> pp k <+> mb ("->" <+>) mv <+> "in" <+> pp t <+> pp z `indent` pp b
    WhileLoop i f b        -> c i $ "while" <+> pp f `indent` pp b
    Seq       i es         -> c i $ "block {" `indent` sparseBlock es `above` "}"
    Lambda    i ps ty b    -> c i $ (("function" `indent` tuple ps) `indent` (":" <+> pp ty)) `indent` pp b
    MapPatch  i z bs       -> c i $ "patch" `indent` pp z `above` "with" <+> "map" `indent` list bs
    SetPatch  i z bs       -> c i $ "patch" `indent` pp z `above` "with" <+> "set" `indent` list bs
    RecordUpd i r up       -> c i $ pp r `indent` "with" <+> "record" `indent` list up
    WrongExpr   err        -> pp err

instance HasComments i => Pretty (Alt i) where
  pp = \case
    Alt i p b -> c i $ "|" <+> pp p <+> "->" `indent` pp b
    WrongAlt err -> pp err

instance HasComments i => Pretty (MapBinding i) where
  pp = \case
    MapBinding i k v -> c i $ pp k <+> "->" `indent` pp v
    WrongMapBinding err -> pp err

instance HasComments i => Pretty (Assignment i) where
  pp = \case
    Assignment      i n e -> c i $ pp n <+> "=" `indent` pp e
    WrongAssignment   err -> pp err

instance HasComments i => Pretty (FieldAssignment i) where
  pp = \case
    FieldAssignment      i n e -> c i $ pp n <+> "=" `indent` pp e
    WrongFieldAssignment   err -> pp err

instance HasComments i => Pretty (Constant i) where
  pp = \case
    Int           i z   -> c i $ pp z
    Nat           i z   -> c i $ pp z
    String        i z   -> c i $ pp z
    Float         i z   -> c i $ pp z
    Bytes         i z   -> c i $ pp z
    Tez           i z   -> c i $ pp z
    WrongConstant   err -> pp err

instance HasComments i => Pretty (QualifiedName i) where
  pp = \case
    QualifiedName i src path -> c i $ pp src <> sepByDot path
    WrongQualifiedName err   -> pp err

instance HasComments i => Pretty (Pattern i) where
  pp = \case
    IsConstr     i ctor arg  -> c i $ pp ctor <> maybe empty pp arg
    IsConstant   i z         -> c i $ pp z
    IsVar        i name      -> c i $ pp name
    IsCons       i h t       -> c i $ pp h <+> ("#" <+> pp t)
    IsWildcard   i           -> c i $ "_"
    IsList       i l         -> c i $ list l
    IsTuple      i t         -> c i $ tuple t
    WrongPattern   err       -> pp err


instance HasComments i => Pretty (Name i) where
  pp = \case
    Name      i raw -> c i $ pp raw
    WrongName err   -> pp err

instance HasComments i => Pretty (Path i) where
  pp = \case
    At i n -> c i $ pp n
    Ix i j -> c i $ pp j
    WrongPath err -> pp err

instance HasComments i => Pretty (TField i) where
  pp = \case
    TField i n t -> c i $ pp n <> ":" `indent` pp t
    WrongTField err -> pp err

instance HasComments i => Pretty (LHS i) where
  pp = \case
    LHS i qn mi -> c i $ pp qn <> foldMap (brackets . pp) mi
    WrongLHS err -> pp err
