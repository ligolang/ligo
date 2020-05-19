
{-
  The AST and auxillary types along with their pretty-printers.

  TODO: Untangle pretty-printing mess into combinators.
  TODO: Store offending text verbatim in Wrong*.
-}

module AST.Errors where

import Parser
import AST.Types

class HasErrors h where
  errors :: h -> [Error]

instance {-# OVERLAPPABLE #-} (HasErrors a, Foldable f) => HasErrors (f a) where
  errors = foldMap errors

instance HasErrors (Contract i) where
  errors = \case
    Contract _ ds  -> errors ds
    WrongContract err -> return err

-- data Contract info
--   = Contract      info [Declaration info]
--   | WrongContract      Error

instance HasErrors (Declaration i) where
  errors = \case
    ValueDecl _ bind -> errors bind
    TypeDecl _ n ty -> errors n <> errors ty
    Action _ e -> errors e
    Include _ _ -> fail "text"
    WrongDecl err -> return err

-- data Declaration info
--   = ValueDecl info (Binding info)
--   | TypeDecl  info (Name info) (Type info)
--   | Action    info (Expr info)
--   | Include   info Text
--   | WrongDecl      Error

instance HasErrors (Binding i) where
  errors = \case
    Irrefutable _ a b -> errors a <> errors b
    Function _ _ a b c d -> errors a <> errors b <> errors c <> errors d
    Var _ a b c -> errors a <> errors b <> errors c
    Const _ a b c -> errors a <> errors b <> errors c
    WrongBinding e -> return e

-- data Binding info
--   = Irrefutable  info (Pattern info) (Expr info)
--   | Function     info Bool (Name info) [VarDecl info] (Type info) (Expr info)
--   | Var          info (Name info) (Type info) (Expr info)
--   | Const        info (Name info) (Type info) (Expr info)
--   | WrongBinding      Error

instance HasErrors (VarDecl i) where
  errors = \case
    Decl _ a b c -> errors a <> errors b <> errors c
    WrongVarDecl e -> return e

-- data VarDecl info
--   = Decl         info (Mutable info) (Name info) (Type info)
--   | WrongVarDecl      Error

instance HasErrors (Mutable i) where
  errors = \case
    WrongMutable e -> return e
    _              -> fail "none"

-- data Mutable info
--   = Mutable      info
--   | Immutable    info
--   | WrongMutable      Error

instance HasErrors (Type i) where
  errors = \case
    TArrow _ a b -> errors a <> errors b
    TRecord _ fs -> errors fs
    TVar _ a -> errors a
    TSum _ cs -> errors cs
    TProduct _ es -> errors es
    TApply _ f xs -> errors f <> errors xs

-- data Type info
--   = TArrow    info  (Type info) (Type info)
--   | TRecord   info [TField info]
--   | TVar      info  (Name info)
--   | TSum      info [Variant info]
--   | TProduct  info  [Type info]
--   | TApply    info  (Name info) [Type info]
--   | WrongType      Error

instance HasErrors (Variant i) where
  errors = \case
    Variant _ a b -> errors a <> errors b
    WrongVariant e -> return e

-- data Variant info
--   = Variant info (Name info) (Maybe (Type info))
--   | WrongVariant Error

instance HasErrors (TField i) where
  errors = \case
    TField _ a b -> errors a <> errors b
    WrongTField e -> return e

-- data TField info
--   = TField info (Name info) (Type info)
--   | WrongTField Error

instance HasErrors (Expr i) where
  errors = \case
    Let _ ds b -> errors ds <> errors b
    Apply _ f xs -> errors f <> errors xs
    Constant _ c -> errors c
    Ident _ q -> errors q
    BinOp _ l _ r -> errors l <> errors r
    UnOp _ _ o -> errors o
    Record _ fs -> errors fs
    If _ a b c -> errors a <> errors b <> errors c
    Assign _ a b -> errors a <> errors b
    List _ l -> errors l
    Set _ l -> errors l
    Tuple _ l -> errors l
    Annot _ a b -> errors a <> errors b
    Attrs _ _ -> fail "none"
    BigMap _ l -> errors l
    Map _ l -> errors l
    MapRemove _ a b -> errors a <> errors b
    SetRemove _ a b -> errors a <> errors b
    Indexing _ a b -> errors a <> errors b
    Case _ a bs -> errors a <> errors bs
    Skip _ -> fail "none"
    ForLoop _ a b c d -> errors a <> errors b <> errors c <> errors d
    WhileLoop _ a b -> errors a <> errors b
    Seq _ ds -> errors ds
    Lambda _ ps b c -> errors ps <> errors b <> errors c
    ForBox _ a b _ c d -> errors a <> errors b <> errors c <> errors d
    MapPatch _ a bs -> errors a <> errors bs
    SetPatch _ a bs -> errors a <> errors bs
    RecordUpd _ a bs -> errors a <> errors bs
    WrongExpr e -> return e

-- data Expr info
--   = Let       info [Declaration info] (Expr info)
--   | Apply     info (Expr info) [Expr info]
--   | Constant  info (Constant info)
--   | Ident     info (QualifiedName info)
--   | BinOp     info (Expr info) Text (Expr info)
--   | UnOp      info Text (Expr info)
--   | Record    info [Assignment info]
--   | If        info (Expr info) (Expr info) (Expr info)
--   | Assign    info (LHS info) (Expr info)
--   | List      info [Expr info]
--   | Set       info [Expr info]
--   | Tuple     info [Expr info]
--   | Annot     info (Expr info) (Type info)
--   | Attrs     info [Text]
--   | BigMap    info [MapBinding info]
--   | Map       info [MapBinding info]
--   | MapRemove info (Expr info) (QualifiedName info)
--   | SetRemove info (Expr info) (QualifiedName info)
--   | Indexing  info (QualifiedName info) (Expr info)
--   | Case      info (Expr info) [Alt info]
--   | Skip      info
--   | ForLoop   info (Name info) (Expr info) (Expr info) (Expr info)
--   | WhileLoop info (Expr info) (Expr info)
--   | Seq       info [Declaration info]
--   | Lambda    info [VarDecl info] (Type info) (Expr info)
--   | ForBox    info (Name info) (Maybe (Name info)) Text (Expr info) (Expr info)
--   | MapPatch  info (QualifiedName info) [MapBinding info]
--   | SetPatch  info (QualifiedName info) [Expr info]
--   | RecordUpd info (QualifiedName info) [FieldAssignment info]
--   | WrongExpr      Error

instance HasErrors (Alt i) where
  errors = \case
    Alt _ a b -> errors a <> errors b
    WrongAlt e -> return e

-- data Alt info
--   = Alt info (Pattern info) (Expr info)
--   | WrongAlt Error

instance HasErrors (LHS i) where
  errors = \case
    LHS _ a b -> errors a <> errors b
    WrongLHS e -> return e

-- data LHS info
--   = LHS info (QualifiedName info) (Maybe (Expr info))
--   | WrongLHS Error

instance HasErrors (MapBinding i) where
  errors = \case
    MapBinding _ a b -> errors a <> errors b
    WrongMapBinding e -> return e

-- data MapBinding info
--   = MapBinding info (Expr info) (Expr info)
--   | WrongMapBinding Error

instance HasErrors (Assignment i) where
  errors = \case
    Assignment _ a b -> errors a <> errors b
    WrongAssignment e -> return e

-- data Assignment info
--   = Assignment info (Name info) (Expr info)
--   | WrongAssignment Error

instance HasErrors (FieldAssignment i) where
  errors = \case
    FieldAssignment _ a b -> errors a <> errors b
    WrongFieldAssignment e -> return e

-- data FieldAssignment info
--   = FieldAssignment info (QualifiedName info) (Expr info)
--   | WrongFieldAssignment Error

instance HasErrors (Constant i) where
  errors = \case
    WrongConstant e -> return e
    _ -> fail "none"

-- data Constant info
--   = Int     info Text
--   | Nat     info Text
--   | String  info Text
--   | Float   info Text
--   | Bytes   info Text
--   | Tez     info Text
--   | WrongConstant Error

instance HasErrors (Pattern i) where
  errors = \case
    IsConstr _ a b -> errors a <> errors b
    IsConstant _ c -> errors c
    IsVar _ a -> errors a
    IsCons _ a b -> errors a <> errors b
    IsWildcard _ -> fail "none"
    IsList _ l -> errors l
    IsTuple _ l -> errors l
    WrongPattern e -> return e

-- data Pattern info
--   = IsConstr     info (Name info) (Maybe (Pattern info))
--   | IsConstant   info (Constant info)
--   | IsVar        info (Name info)
--   | IsCons       info (Pattern info) (Pattern info)
--   | IsWildcard   info
--   | IsList       info [Pattern info]
--   | IsTuple      info [Pattern info]
--   | WrongPattern      Error

instance HasErrors (QualifiedName i) where
  errors = \case
    QualifiedName _ a b -> errors a <> errors b
    WrongQualifiedName e -> return e

-- data QualifiedName info
--   = QualifiedName
--     { qnInfo   :: info
--     , qnSource :: Name info
--     , qnPath   :: [Path info]
--     }
--   | WrongQualifiedName Error

instance HasErrors (Path i) where
  errors = \case
    At _ a -> errors a
    Ix _ _ -> fail "none"
    WrongPath e -> return e

-- data Path info
--   = At info (Name info)
--   | Ix info Text
--   | WrongPath Error

instance HasErrors (Name i) where
  errors = \case
    WrongName e -> return e
    _ -> fail "none"

-- data Name info = Name
--   { info    :: info
--   , raw     :: Text
--   }
--   | WrongName Error
