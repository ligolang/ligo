
{- TODO(kirill.andreev): add offsets to ranges, store verbatim in Wrong* -}

module AST.Types where

import Control.Monad.State

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void

import Parser
import ParseTree
import Pretty

import Debug.Trace

type TODO = Text

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

instance Stubbed (Name info) where stub = WrongName

instance Show (Name info) where
  show = \case
    Name _ raw  -> Text.unpack raw
    WrongName r -> "(Name? " ++ show r ++ ")"

instance Pretty (Contract i) where
  pp = \case
    Contract _ decls ->
      hang "(* contract *)" 2 do
        vcat $ punctuate "\n" $ map (($$ empty) . pp) decls

    WrongContract err ->
      pp err

instance Pretty (Declaration i) where
  pp = \case
    ValueDecl _ binding -> pp binding
    TypeDecl  _ n ty    -> hang ("type" <+> pp n <+> "=") 2 (pp ty)
    Action    _ e       -> pp e
    Include   _ f       -> "#include" <+> pp f
    WrongDecl err       -> pp err

instance Pretty (Binding i) where
  pp = \case
    Irrefutable  _ pat expr -> error "irrefs in pascaligo?"
    Function     _ isRec name params ty body ->
      hang
        ( fsep
          [ if isRec then "recursive" else empty
          , "function"
          , pp name
          , tuple params
          , ":"
          , pp ty
          , "is"
          ]
        )
        2
        (pp body)
    Var   _ name ty value ->
      hang
        ("var" <+> pp name <+> ":" <+> pp ty <+> ":=")
        2
        (pp value)
    Const _ name ty body ->
      hang
        ("const" <+> pp name <+> ":" <+> pp ty <+> "=")
        2
        (pp body)
    WrongBinding err ->
      pp err

instance Pretty (VarDecl i) where
  pp = \case
    Decl _ mutability name ty -> fsep
      [ pp mutability
      , pp name
      , ":"
      , pp ty
      ]
    WrongVarDecl err ->
      pp err

instance Pretty (Mutable i) where
  pp = \case
    Mutable      _   -> "var"
    Immutable    _   -> "const"
    WrongMutable err -> pp err

instance Pretty (Type i) where
  pp = \case
    TArrow    _ dom codom -> parens (pp dom <+> "->" <+> pp codom)
    TRecord   _ fields    -> "record [" <> (vcat $ map pp fields) <> "]"
    TVar      _ name      -> pp name
    TSum      _ variants  -> vcat $ map pp variants
    TProduct  _ elements  -> fsep $ punctuate " *" $ map pp elements
    TApply    _ f xs      -> pp f <> parens (fsep $ punctuate "," $ map pp xs)
    WrongType   err       -> pp err
    where
      ppField (name, ty) = pp name <> ": " <> pp ty <> ";"

instance Pretty (Variant i) where
  pp = \case
    Variant _ ctor (Just ty) -> hang ("|" <+> pp ctor <+> "of") 2 (pp ty)
    Variant _ ctor  _        -> "|" <+> pp ctor
    WrongVariant err -> pp err

instance Pretty (Expr i) where
  pp = \case
    Let       _ decls body -> "block {" $$ (nest 2 $ vcat $ punctuate "\n" $ map pp decls) $$ "}" $$ "with" $$ nest 2 (pp body)
    Apply     _ f xs       -> pp f <+> tuple xs
    Constant  _ constant   -> pp constant
    Ident     _ qname      -> pp qname
    BinOp     _ l o r      -> parens (pp l <+> pp o <+> pp r)
    UnOp      _   o r      -> parens (pp o <+> pp r)
    Record    _ az         -> "record [" <> (fsep $ punctuate ";" $ map pp az) <> "]"
    If        _ b t e      -> fsep ["if" <+> pp b, hang "then" 2 $ pp t, hang "else" 2 $ pp e]
    Assign    _ l r        -> hang (pp l <+> ":=") 2 (pp r)
    List      _ l          -> "list [" <> fsep (punctuate ";" $ map pp l) <> "]"
    Set       _ l          -> "set [" <> fsep (punctuate ";" $ map pp l) <> "]"
    Tuple     _ l          -> "(" <> fsep (punctuate "," $ map pp l) <> ")"
    Annot     _ n t        -> ("(" <> pp n) <+> ":" <+> (pp t <> ")")
    Attrs     _ ts         -> "attributes [" <> fsep (punctuate ";" $ map pp ts) <> "]"
    BigMap    _ bs         -> "big_map [" <> fsep (punctuate ";" $ map pp bs) <> "]"
    Map       _ bs         ->     "map [" <> fsep (punctuate ";" $ map pp bs) <> "]"
    MapRemove _ k m        -> hang ("remove" <+> pp k) 0 ("from" <+> "map" <+> pp m)
    SetRemove _ k s        -> hang ("remove" <+> pp k) 0 ("from" <+> "set" <+> pp s)
    Indexing  _ a i        -> pp a <> brackets (pp i)
    Case      _ s az       -> hang ("case" <+> pp s <+> "of") 2 (vcat $ map pp az)
    Skip      _            -> "skip"
    ForLoop   _ i s f b    -> hang ("for" <+> pp i <+> ":=" <+> pp s <+> "to" <+> pp f) 2 (pp b)
    ForBox    _ k mv t c b -> hang ("for" <+> (pp k <> maybe empty ((" ->" <+>) . pp) mv) <+> "in" <+> pp t <+> pp c) 2 (pp b)
    WhileLoop _ f b        -> hang ("while" <+> pp f) 2 (pp b)
    Seq       _ es         -> hang (hang "block {" 2 (vcat $ map pp es)) 0 "}"
    Lambda    _ ps ty b    -> parens (hang ("function" <+> ("(" <> fsep (punctuate "," $ map pp ps) <> ") :") <+> pp ty) 2 (pp b))
    MapPatch  _ c bs       -> hang (hang "patch" 2 (pp c)) 0 (hang ("with" <+> "map") 2 ("[" <> fsep (punctuate ";" $ map pp bs) <> "]"))
    SetPatch  _ c bs       -> hang (hang "patch" 2 (pp c)) 0 (hang ("with" <+> "set") 2 ("[" <> fsep (punctuate ";" $ map pp bs) <> "]"))
    RecordUpd _ r up       -> hang (pp r) 2 (hang ("with" <+> "record") 2 ("[" <> fsep (punctuate ";" $ map pp up) <> "]"))
    WrongExpr   err        -> pp err

instance Pretty (Alt info) where
  pp = \case
    Alt _ p b -> hang ("|" <+> pp p <+> "->") 2 (pp b)
    WrongAlt err -> pp err

instance Pretty (MapBinding i) where
  pp = \case
    MapBinding _ k v -> hang (pp k <+> "->") 2 (pp v)
    WrongMapBinding err -> pp err

instance Pretty (Assignment i) where
  pp = \case
    Assignment      _ n e -> pp n <+> "=" <+> pp e
    WrongAssignment   err -> pp err

instance Pretty (FieldAssignment i) where
  pp = \case
    FieldAssignment      _ n e -> pp n <+> "=" <+> pp e
    WrongFieldAssignment   err -> pp err

instance Pretty (Constant i) where
  pp = \case
    Int           _ c   -> pp c
    Nat           _ c   -> pp c
    String        _ c   -> pp c
    Float         _ c   -> pp c
    Bytes         _ c   -> pp c
    Tez           _ c   -> pp c
    WrongConstant   err -> pp err

instance Pretty (QualifiedName i) where
  pp = \case
    QualifiedName _ src path -> pp src <> cat (map (("." <>) . pp) path)
    WrongQualifiedName err   -> pp err

instance Pretty (Pattern info) where
  pp = \case
    IsConstr     _ ctor arg  -> pp ctor <> maybe empty pp arg
    IsConstant   _ c         -> pp c
    IsVar        _ name      -> pp name
    IsCons       _ h t       -> pp h <+> "#" <+> pp t
    IsWildcard   _           -> "_"
    IsList       _ l         -> "[" <> fsep (punctuate ";" $ map pp l) <> "]"
    IsTuple      _ t         -> "(" <> fsep (punctuate "," $ map pp t) <> ")"
    WrongPattern   err       -> pp err


instance Pretty (Name i) where
  pp = \case
    Name      _ raw -> pp raw
    WrongName err   -> pp err

instance Pretty (Path i) where
  pp = \case
    At _ n -> pp n
    Ix _ i -> pp i
    WrongPath err -> pp err

instance Pretty (TField i) where
  pp = \case
    TField _ n t -> hang (pp n <> ":") 2 (pp t)
    WrongTField err -> pp err

instance Pretty (LHS i) where
  pp = \case
    LHS _ qn mi -> pp qn <> foldMap (brackets . pp) mi
    WrongLHS err -> pp err

tuple :: Pretty p => [p] -> Doc
tuple xs = parens (fsep $ punctuate "," $ map pp xs)