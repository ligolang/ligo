
{-
  The AST and auxillary types along with their pretty-printers.

  TODO: Untangle pretty-printing mess into combinators.
  TODO: Store offending text verbatim in Wrong*.
-}

module AST.Scope where

import Control.Lens hiding (Const, List)
import Control.Monad.State

import Data.Text (Text)
import Data.Traversable
import Data.Foldable

import Parser
import Range
import AST.Types

class UpdatableScopes a where
  updateScopes :: HasRange r => (r -> ScopeM s) -> a r -> ScopeM (a s)

type ScopeM = State [Env]

newtype Env = Env
  { _eDecls :: [ScopedDecl]
  }
  deriving newtype (Semigroup, Monoid)

data ScopedDecl = ScopedDecl
  { _sdName   :: Maybe Text
  , _sdOrigin :: Maybe Range
  , _sdBody   :: Maybe Range
  , _sdType   :: Maybe (Either (Type ()) Kind)
  }

data Kind = Star

block :: ScopeM a -> ScopeM a
block action = do
  modify \(top : rest) -> top : top : rest  -- inheriting outer scope
  res <- action
  modify tail                               -- dropping current frame
  return res

define :: ScopedDecl -> ScopeM ()
define sd = do
  modify \(Env top : rest) -> Env (sd : top) : rest

def
  :: ( HasRange i
     , Foldable g
     , Stubbed (g i)
     )
  => Name i
  -> g i
  -> Type j
  -> ScopeM ()
def n b ty =
  define $ ScopedDecl
    (n^?raw)
    (n^?folded.location)
    (b^?treeRange)
    (Just $ Left $ void ty)

defP
  :: ( HasRange i
     , Foldable g
     , Stubbed (g i)
     )
  => Name i
  -> g i
  -> Maybe (Type j)
  -> ScopeM ()
defP n b mty =
  define $ ScopedDecl
    (n^?raw)
    (n^?folded.location)
    (b^?treeRange)
    (fmap (Left . void) mty)

defParam
  :: ( HasRange i
     )
  => Name i
  -> Type j
  -> ScopeM ()
defParam n ty =
  define $ ScopedDecl
    (n^?raw)
    (n^?folded.location)
    Nothing
    (Just $ Left $ void ty)

defI
  :: ( HasRange i
     )
  => Name i
  -> ScopeM ()
defI n =
  define $ ScopedDecl
    (n^?raw)
    (n^?folded.location)
    Nothing
    Nothing

defE
  :: ( HasRange i
     , Foldable f
     , Stubbed (f i)
     )
  => Name i
  -> f i
  -> ScopeM ()
defE n b =
  define $ ScopedDecl
    (n^?raw)
    (n^?folded.location)
    (b^?treeRange)
    Nothing

defType
  :: ( Foldable g
     , HasRange i
     , Stubbed (g i)
     )
  => Name i
  -> g i
  -> Kind
  -> ScopeM ()
defType n b ki =
  define $ ScopedDecl
    (n^?raw)
    (n^?folded.location)
    (b^?treeRange)
    (Just (Right ki))

treeRange
  :: ( Foldable f
     , HasRange a
     , Stubbed (f a)
     )
  => Fold (f a) Range
treeRange = folded.location

instance UpdatableScopes Contract where
  updateScopes update = \case
    Contract i ds -> do
      block do
        Contract
          <$> update i
          <*> for ds (updateScopes update)

    WrongContract e -> do
      return $ WrongContract e


-- data Contract info
--   = Contract      info [Declaration info]
--   | WrongContract      Error

instance UpdatableScopes Declaration where
  updateScopes update = \case
    ValueDecl i bind -> do
      ValueDecl
        <$> update i
        <*> updateScopes update bind

    TypeDecl i n ty -> do
      defType n ty Star

      TypeDecl
        <$> update i
        <*> updateScopes update n
        <*> updateScopes update ty

    Action i expr -> do
      Action
        <$> update i
        <*> updateScopes update expr

    Include i s -> do
      Include
        <$> update i
        <*> pure s

    WrongDecl e -> do
      return $ WrongDecl e

-- data Declaration info
--   = ValueDecl info (Binding info)
--   | TypeDecl  info (Name info) (Type info)
--   | Action    info (Expr info)
--   | Include   info Text
--   | WrongDecl      Error

instance UpdatableScopes Binding where
  updateScopes update = \case
    Irrefutable i p e -> do
      res <- Irrefutable
        <$> update i
        <*> updateScopes update p
        <*> updateScopes update e

      for_ (p^..patternNames) \name -> do
        defE name e

      return res

    Function i recur n params ty body -> do
      let
        returns = telescope ((void) <$> params)
        defineHere = def n body (returns (void ty))

      when recur do
        defineHere

      res <- block do
        Function
          <$> update i
          <*> pure recur
          <*> updateScopes update n
          <*> traverse (updateScopes update) params
          <*> updateScopes update ty
          <*> updateScopes update body

      unless recur do
        defineHere

      return res

    Var i n ty e -> do
      res <- Var
        <$> update i
        <*> updateScopes update n
        <*> updateScopes update ty
        <*> updateScopes update e

      def n e ty

      return res

    Const i n ty e -> do
      res <- Const
        <$> update i
        <*> updateScopes update n
        <*> updateScopes update ty
        <*> updateScopes update e

      def n e ty

      return res

    WrongBinding e -> do
      return $ WrongBinding e

telescope :: [VarDecl i] -> Type () -> Type ()
telescope = flip $ foldr \case
  Decl _ _ _ ty -> TArrow () (void ty)
  WrongVarDecl e -> TArrow () (WrongType e)

-- data Binding info
--   = Irrefutable  info (Pattern info) (Expr info)
--   | Function     info Bool (Name info) [VarDecl info] (Type info) (Expr info)
--   | Var          info (Name info) (Type info) (Expr info)
--   | Const        info (Name info) (Type info) (Expr info)
--   | WrongBinding      Error

instance UpdatableScopes VarDecl where
  updateScopes update = \case
    Decl i mut n ty -> do
      res <- Decl
        <$> update i
        <*> updateScopes update mut
        <*> updateScopes update  n
        <*> updateScopes update ty

      defParam n ty

      return res

    WrongVarDecl e -> do
      return $ WrongVarDecl e

-- data VarDecl info
--   = Decl         info (Mutable info) (Name info) (Type info)
--   | WrongVarDecl      Error

instance UpdatableScopes Mutable where
  updateScopes update = \case
    Mutable i -> Mutable   <$> update i
    Immutable i -> Immutable <$> update i
    WrongMutable e -> return $ WrongMutable e

-- data Mutable info
--   = Mutable      info
--   | Immutable    info
--   | WrongMutable      Error

instance UpdatableScopes Type where
  updateScopes update = \case
    TArrow i cod dom -> do
      TArrow
        <$> update i
        <*> updateScopes update cod
        <*> updateScopes update dom

    TRecord i fs -> do
      TRecord
        <$> update i
        <*> traverse (updateScopes update) fs

    TVar i n -> do
      TVar
        <$> update i
        <*> updateScopes update n

    TSum i vs -> do
      TSum
        <$> update i
        <*> traverse (updateScopes update) vs

    TProduct i es -> do
      TProduct
        <$> update i
        <*> traverse (updateScopes update) es

    TApply i f xs -> do
      TApply
        <$> update i
        <*> updateScopes update f
        <*> traverse (updateScopes update) xs

    WrongType e -> do
      return $ WrongType e

-- data Type info
--   = TArrow    info  (Type info) (Type info)
--   | TRecord   info [TField info]
--   | TVar      info  (Name info)
--   | TSum      info [Variant info]
--   | TProduct  info  [Type info]
--   | TApply    info  (Name info) [Type info]
--   | WrongType      Error

instance UpdatableScopes Variant where
  updateScopes update = \case
    Variant i n mty -> do
      res <- Variant
        <$> update i
        <*> updateScopes update n
        <*> traverse (updateScopes update) mty

      defParam n $ case mty of
        Just it -> TArrow () (void it) (TVar () (Name () "unknown"))
        Nothing -> TVar () (Name () "unknown")

      return res

    WrongVariant e -> do
      return $ WrongVariant e

-- data Variant info
--   = Variant info (Name info) (Maybe (Type info))
--   | WrongVariant Error

instance UpdatableScopes TField where
  updateScopes update = \case
    TField i a b -> do
      TField
        <$> update i
        <*> updateScopes update a
        <*> updateScopes update b

    WrongTField e -> do
      return $ WrongTField e

-- data TField info
--   = TField info (Name info) (Type info)
--   | WrongTField Error

instance UpdatableScopes Expr where
  updateScopes update = \case
    Let i ds b -> do
      s <- update i
      block do
        Let s
          <$> traverse (updateScopes update) ds
          <*> updateScopes update b

    Apply i f xs ->
      Apply
        <$> update i
        <*> updateScopes update f
        <*> traverse (updateScopes update) xs

    Constant i c ->
      Constant
        <$> update i
        <*> updateScopes update c

    Ident i qn -> do
      Ident
        <$> update i
        <*> updateScopes update qn

    BinOp i l op r -> do
      BinOp
        <$> update i
        <*> updateScopes update l
        <*> pure op
        <*> updateScopes update r

    UnOp i op r -> do
      UnOp
        <$> update i
        <*> pure op
        <*> updateScopes update r

    Record i fs -> do
      Record
        <$> update i
        <*> traverse (updateScopes update) fs

    If i a b c -> do
      If
        <$> update i
        <*> updateScopes update a
        <*> updateScopes update b
        <*> updateScopes update c

    Assign i l r -> do
      Assign
        <$> update i
        <*> updateScopes update l
        <*> updateScopes update r

    List i ls -> do
      List
        <$> update i
        <*> traverse (updateScopes update) ls

    Set i ls -> do
      Set
        <$> update i
        <*> traverse (updateScopes update) ls

    Tuple i ls -> do
      Tuple
        <$> update i
        <*> traverse (updateScopes update) ls

    Annot i e ty -> do
      Annot
        <$> update i
        <*> updateScopes update e
        <*> updateScopes update ty

    Attrs i az -> do
      Attrs
        <$> update i
        <*> pure az

    BigMap i ms -> do
      BigMap
        <$> update i
        <*> traverse (updateScopes update) ms

    Map i ms -> do
      Map
        <$> update i
        <*> traverse (updateScopes update) ms

    MapRemove i e qn -> do
      MapRemove
        <$> update i
        <*> updateScopes update e
        <*> updateScopes update qn

    SetRemove i e qn -> do
      SetRemove
        <$> update i
        <*> updateScopes update e
        <*> updateScopes update qn

    Indexing i q e -> do
      Indexing
        <$> update i
        <*> updateScopes update q
        <*> updateScopes update e

    Case i e az -> do
      Case
        <$> update i
        <*> updateScopes update e
        <*> traverse (updateScopes update) az

    Skip i -> Skip <$> update i

    ForLoop i n a b c -> do
      block do
        defI n
        ForLoop
          <$> update i
          <*> updateScopes update n
          <*> updateScopes update a
          <*> updateScopes update b
          <*> updateScopes update c

    WhileLoop i a b -> do
      WhileLoop
        <$> update i
        <*> updateScopes update a
        <*> updateScopes update b

    Seq i ds -> do
      block do
        Seq
          <$> update i
          <*> traverse (updateScopes update) ds

    Lambda i ps ty b -> do
      block do
        Lambda
          <$> update i
          <*> traverse (updateScopes update) ps
          <*> updateScopes update ty
          <*> updateScopes update b

    ForBox i a mb t e f -> do
      block do
        defI a
        ForBox
          <$> update i
          <*> updateScopes update a
          <*> traverse (updateScopes update) mb
          <*> pure t
          <*> updateScopes update e
          <*> updateScopes update f

    MapPatch i q bs -> do
      MapPatch
        <$> update i
        <*> updateScopes update q
        <*> traverse (updateScopes update) bs

    SetPatch i q bs -> do
      SetPatch
        <$> update i
        <*> updateScopes update q
        <*> traverse (updateScopes update) bs

    RecordUpd i q fs -> do
      RecordUpd
        <$> update i
        <*> updateScopes update q
        <*> traverse (updateScopes update) fs

    WrongExpr e -> do
      return $ WrongExpr e

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

instance UpdatableScopes Alt where
  updateScopes update = \case
    Alt i p e -> do
      block do
        s  <- update i
        p' <- updateScopes update p
        for_ (p^..patternNames) \name -> do
          defI name

        Alt s p'
          <$> updateScopes update e

    WrongAlt e -> do
      return $ WrongAlt e

-- data Alt info
--   = Alt info (Pattern info) (Expr info)
--   | WrongAlt Error

instance UpdatableScopes LHS where
  updateScopes update = \case
    LHS i q me -> do
      LHS
        <$> update i
        <*> updateScopes update q
        <*> traverse (updateScopes update) me

    WrongLHS e -> do
      return $ WrongLHS e

-- data LHS info
--   = LHS info (QualifiedName info) (Maybe (Expr info))
--   | WrongLHS Error

instance UpdatableScopes MapBinding where
  updateScopes update = \case
    MapBinding i e f -> do
      MapBinding
        <$> update i
        <*> updateScopes update e
        <*> updateScopes update f

    WrongMapBinding e -> do
      return $ WrongMapBinding e

-- data MapBinding info
--   = MapBinding info (Expr info) (Expr info)
--   | WrongMapBinding Error

instance UpdatableScopes Assignment where
  updateScopes update = \case
    Assignment i n e -> do
      Assignment
        <$> update i
        <*> updateScopes update n
        <*> updateScopes update e

    WrongAssignment e -> do
      return $ WrongAssignment e

-- data Assignment info
--   = Assignment info (Name info) (Expr info)
--   | WrongAssignment Error

instance UpdatableScopes FieldAssignment where
  updateScopes update = \case
    FieldAssignment i q e -> do
      FieldAssignment
        <$> update i
        <*> updateScopes update q
        <*> updateScopes update e

    WrongFieldAssignment e -> do
      return $ WrongFieldAssignment e

-- data FieldAssignment info
--   = FieldAssignment info (QualifiedName info) (Expr info)
--   | WrongFieldAssignment Error

instance UpdatableScopes Constant where
  updateScopes update = \case
    Int i t -> Int <$> update i <*> pure t
    Nat i t -> Nat <$> update i <*> pure t
    String i t -> String <$> update i <*> pure t
    Float i t -> Float <$> update i <*> pure t
    Bytes i t -> Bytes <$> update i <*> pure t
    Tez i t -> Tez <$> update i <*> pure t
    WrongConstant e -> return $ WrongConstant e

-- data Constant info
--   = Int     info Text
--   | Nat     info Text
--   | String  info Text
--   | Float   info Text
--   | Bytes   info Text
--   | Tez     info Text
--   | WrongConstant Error

patternNames :: Fold (Pattern i) (Name i)
patternNames act = go
  where
    go = \case
      IsConstr i n p -> IsConstr i <$> act n <*> traverse go p
      IsConstant i c -> pure $ IsConstant i c
      IsVar i n      -> IsVar i <$> act n
      IsCons i h t   -> IsCons i <$> go h <*> go t
      IsWildcard i   -> pure $ IsWildcard i
      IsList i ps    -> IsList i <$> traverse go ps
      IsTuple i ps   -> IsTuple i <$> traverse go ps
      WrongPattern e -> pure $ WrongPattern e

instance UpdatableScopes Pattern where
  updateScopes update = \case
    IsConstr i n mp -> do
      IsConstr
        <$> update i
        <*> updateScopes update n
        <*> traverse (updateScopes update) mp

    IsConstant i c -> do
      IsConstant
        <$> update i
        <*> updateScopes update c

    IsVar i n -> do
      IsVar
        <$> update i
        <*> updateScopes update n

    IsCons i h t ->
      IsCons
        <$> update i
        <*> updateScopes update h
        <*> updateScopes update t

    IsWildcard i -> IsWildcard <$> update i

    IsList i l ->
      IsList
        <$> update i
        <*> traverse (updateScopes update) l

    IsTuple i l ->
      IsTuple
        <$> update i
        <*> traverse (updateScopes update) l

    WrongPattern e -> do
      return $ WrongPattern e

-- data Pattern info
--   = IsConstr     info (Name info) (Maybe (Pattern info))
--   | IsConstant   info (Constant info)
--   | IsVar        info (Name info)
--   | IsCons       info (Pattern info) (Pattern info)
--   | IsWildcard   info
--   | IsList       info [Pattern info]
--   | IsTuple      info [Pattern info]
--   | WrongPattern      Error

instance UpdatableScopes QualifiedName where
  updateScopes update = \case
    QualifiedName i n ps -> do
      QualifiedName
        <$> update i
        <*> updateScopes update n
        <*> traverse (updateScopes update) ps

    WrongQualifiedName e -> do
      return $ WrongQualifiedName e

-- data QualifiedName info
--   = QualifiedName
--     { qnInfo   :: info
--     , qnSource :: Name info
--     , qnPath   :: [Path info]
--     }
--   | WrongQualifiedName Error

instance UpdatableScopes Path where
  updateScopes update = \case
    At i n -> At <$> update i <*> updateScopes update n
    Ix i n -> Ix <$> update i <*> pure n
    WrongPath e -> return $ WrongPath e

-- data Path info
--   = At info (Name info)
--   | Ix info Text
--   | WrongPath Error

instance UpdatableScopes Name where
  updateScopes update = \case
    Name i r -> Name <$> update i <*> pure r
    WrongName e -> do
      return $ WrongName e

-- data Name info = Name
--   { info    :: info
--   , raw     :: Text
--   }
--   | WrongName Error
