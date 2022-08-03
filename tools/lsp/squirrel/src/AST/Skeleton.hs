{- | The AST and auxiliary types along with their pretty-printers.

     The comments for fields in types are the type before it was made untyped.
-}

{-# LANGUAGE DeriveGeneric #-}

module AST.Skeleton
  ( SomeLIGO (..)
  , LIGO
  , Tree'
  , RawLigoList
  , Lang (..)
  , reasonLIGOKeywords, cameLIGOKeywords, pascaLIGOKeywords, jsLIGOKeywords
  , Name (..), QualifiedName (..), Pattern (..), RecordFieldPattern (..)
  , Constant (..), FieldAssignment (..), MapBinding (..), Alt (..), Expr (..)
  , Collection (..), TField (..), Variant (..), Type (..), Binding (..)
  , RawContract (..), TypeName (..), TypeVariableName (..), FieldName (..)
  , Verbatim (..), Error (..), Ctor (..), NameDecl (..), Preprocessor (..)
  , PreprocessorCommand (..), ModuleName (..), ModuleAccess (..)
  , TypeParams (..), PatchableExpr (..), CaseOrDefaultStm (..)

  , getLIGO
  , setLIGO
  , nestedLIGO
  , withNestedLIGO
  ) where

import Control.Lens.Lens (Lens, lens)
import Data.Functor.Classes (Eq1 (..))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import GHC.Generics (Generic)

import Duplo.Pretty (PP (..), Pretty (..))
import Duplo.Tree (Tree)

import Diagnostic (MessageDetail)
import Product (Product)

data SomeLIGO xs = SomeLIGO Lang (LIGO xs)

nestedLIGO :: Lens (SomeLIGO xs) (SomeLIGO xs') (LIGO xs) (LIGO xs')
nestedLIGO = lens getLIGO setLIGO

getLIGO :: SomeLIGO xs -> LIGO xs
getLIGO (SomeLIGO _ ligo) = ligo

setLIGO :: SomeLIGO xs -> LIGO ys -> SomeLIGO ys
setLIGO (SomeLIGO d _) = SomeLIGO d

withNestedLIGO
  :: Functor f => SomeLIGO xs -> (LIGO xs -> f (LIGO xs')) -> f (SomeLIGO xs')
withNestedLIGO = flip nestedLIGO

instance Pretty (LIGO xs) => Show (SomeLIGO xs) where
  show = show . PP

instance Pretty (LIGO xs) => Pretty (SomeLIGO xs) where
  pp (SomeLIGO _ nested) = pp nested

-- | The AST for Pascali... wait. It is, em, universal one.
--
--   TODO: Rename; add stuff if CamelLIGO/ReasonLIGO needs something.
type LIGO xs = Tree' RawLigoList xs
type Tree' fs xs = Tree fs (Product xs)

type RawLigoList =
  [ Name, QualifiedName, Pattern, RecordFieldPattern, Constant, FieldAssignment
  , MapBinding, Alt, Expr, Collection, TField, Variant, Type, Binding
  , RawContract, TypeName, TypeVariableName, FieldName, Verbatim
  , Error, Ctor, NameDecl, Preprocessor, PreprocessorCommand, PatchableExpr
  , ModuleName, ModuleAccess, TypeParams, CaseOrDefaultStm
  ]

-- TODO (LIGO-169): Implement a parser for JsLIGO.
data Lang
  = Pascal
  | Caml
  | Reason
  | Js
  deriving stock Show

pascaLIGOKeywords :: HashSet Text
pascaLIGOKeywords = HashSet.fromList
  [ "is", "begin", "end", "function", "var", "const", "recursive", "type", "set"
  , "map", "list", "big_map", "module", "type", "case", "of", "block", "from"
  , "step", "skip", "if", "then", "else", "record", "remove", "patch", "while"
  , "for", "to", "in", "or", "and", "contains", "mod", "not", "nil"
  ]

cameLIGOKeywords :: HashSet Text
cameLIGOKeywords = HashSet.fromList
  [ "in", "struct", "begin", "end", "match", "with", "rec", "if", "then", "else"
  , "let", "module", "type", "of", "fun", "or", "mod", "land", "lor", "lxor"
  , "lsl", "lsr", "not"
  ]

reasonLIGOKeywords :: HashSet Text
reasonLIGOKeywords = HashSet.fromList
  [ "rec", "if", "else", "switch", "let", "module", "type", "or", "mod", "land"
  , "lor", "lxor", "lsl", "lsr"
  ]

jsLIGOKeywords :: HashSet Text
jsLIGOKeywords = HashSet.fromList
  [ "else", "if", "let", "const", "type", "return", "switch", "case", "default"
  , "as", "break", "namespace", "import", "export", "while", "for", "of"
  ]

-- Let 'Accessor' be either 'FieldName' or a 'Text'ual representation of an
-- index (a number).

newtype RawContract it
  = RawContract [it] -- ^ [Declaration]
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveFor1List

data Binding it
  = BFunction     IsRec it [it] (Maybe it) it -- ^ (Name) (Parameters) (Type) (Expr)
  | BParameter    it (Maybe it) -- ^ (Pattern) (Type)
  | BVar          it (Maybe it) (Maybe it) -- ^ (Pattern) (Type) (Expr)
  | BConst        it (Maybe it) (Maybe it) -- ^ (Pattern) (Type) (Expr)
  | BTypeDecl     it (Maybe it) it -- ^ (Name) (Maybe (TypeParams)) (Type)
  | BAttribute    it -- ^ (Name)
  | BInclude      it
  | BImport       it it
  | BModuleDecl   it [it] -- ^ (Name) (Expr)
  | BModuleAlias  it [it]   -- ^ (Name) (Name)
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data TypeParams it
  = TypeParam it  -- ^ (TypeVariableName)
  | TypeParams [it]  -- ^ [TypeVariableName]
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

type IsRec = Bool

data Type it
  = TArrow    it it    -- ^ (Type) (Type)
  | TRecord   [it]     -- ^ [TField]
  | TSum      [it]     -- ^ [Variant]
  | TProduct  [it]     -- ^ [Type]
  | TApply    it [it]  -- ^ (Name) [Type]
  | TString   it       -- ^ (TString)
  | TWildcard
  | TVariable it       -- ^ (TypeVariableName)
  | TParen    it       -- ^ (Type)
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data Variant it
  = Variant it (Maybe it)  -- (Name) (Maybe (Type))
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data TField it
  = TField it (Maybe it)  -- (Name) (Maybe (Type))
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

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
  | Assign    it it    -- (Name) (Expr)
  | AssignOp  it it it -- (Name) Text (Expr)
  | List      [it] -- [Expr]
  | ListAccess it [it] -- (Name) [Indexes]
  | Set       [it] -- [Expr]
  | Tuple     [it] -- [Expr]
  | Annot     it it -- (Expr) (Type)
  | Attrs     [it]
  | BigMap    [it] -- [MapBinding]
  | Map       [it] -- [MapBinding]
  | Remove    it it it -- (Expr) (Collection) (Expr)
  | Case      it [it]                  -- (Expr) [Alt]
  | Skip
  | Break
  | Return    (Maybe it) -- (Expr)
  | SwitchStm it [it]    -- (Expr) [CaseOrDefaultStm]
  | ForLoop   it it it (Maybe it) it              -- (Name) (Expr) (Expr) (Expr)
  | WhileLoop it it                    -- (Expr) (Expr)
  | ForOfLoop it it it                 -- (Expr) (Expr) (Expr)
  | Seq       [it]                     -- [Declaration]
  | Block     [it]                     -- [Declaration]
  | Lambda    [it] (Maybe it) it               -- [VarDecl] (Maybe (Type)) (Expr)
  | ForBox    it (Maybe it) it it it -- (Name) (Maybe (Name)) (Collection) (Expr) (Expr)
  | Patch     it it -- (Expr) (Expr)
  | RecordUpd it [it] -- (QualifiedName) [FieldAssignment]
  | Michelson it it -- (Verbatim) (Type)
  | Paren     it -- (Expr)
  deriving stock (Generic, Functor, Foldable, Traversable)

data PatchableExpr it
  = PatchableExpr it it  -- (Collection) (Expr)
  deriving stock (Generic, Functor, Foldable, Traversable)

-- Different productions only allow different collections, for example:
-- Remove: CMap | CSet
-- Patch: CList | CMap | CSet
-- ForBox: CList | CSet
-- But we chose to reuse them here to make it simpler.
data Collection it
  = CList
  | CMap
  | CSet
  deriving stock (Generic, Functor, Foldable, Traversable)

newtype Verbatim it
  = Verbatim Text
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype Preprocessor it
  = Preprocessor it -- (PreprocessorCommand)
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveFor1Field

data Alt it
  = Alt it it -- (Pattern) (Expr)
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data CaseOrDefaultStm it
  = CaseStm it [it] -- (Expr) [Expr]
  | DefaultStm [it] -- [Expr]
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data MapBinding it
  = MapBinding it it -- (Expr) (Expr)
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data FieldAssignment it
  = FieldAssignment [it] it -- [Accessor] (Expr)
  | Spread it -- (Name)
  | Capture it -- Accessor
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data Constant it
  = CInt     Text
  | CNat     Text
  | CString  Text
  | CFloat   Text
  | CBytes   Text
  | CTez     Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

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
  | IsRecord     [it] -- [RecordFieldPattern]
  | IsParen      it   -- (Pattern)
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

-- Used specifically in record destructuring
data RecordFieldPattern it
  = IsRecordField it it
  | IsRecordCapture it
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data ModuleAccess it = ModuleAccess
  { maPath  :: [it] -- [Name]
  , maField :: it -- Accessor
  }
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

data QualifiedName it
  = QualifiedName
    { qnSource ::  it -- Name
    , qnPath   :: [it] -- [Accessor]
    }
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

newtype Name it = Name
  { _raw     :: Text
  }
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype NameDecl it = NameDecl
  { _raw     :: Text
  }
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype ModuleName it = ModuleName Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype TypeName it = TypeName Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype TypeVariableName it = TypeVariableName Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype Ctor it = Ctor Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype PreprocessorCommand it = PreprocessorCommand Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

newtype FieldName it = FieldName Text
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)
  deriving Eq1 via DefaultEq1DeriveForText

data Error it = Error MessageDetail [it]
  deriving stock (Generic, Eq, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

-- TODO: is used also for comparing nodes in which the order
-- of elements is unimportant (for instance sum types) so
-- they may be mistreated as unequal.
liftEqList :: (a -> b -> Bool) -> [a] -> [b] -> Bool
liftEqList _ []       []       = True
liftEqList f (x : xs) (y : ys) = f x y && liftEqList f xs ys
liftEqList _ _        _        = False

liftEqMaybe :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
liftEqMaybe _ Nothing  Nothing  = True
liftEqMaybe f (Just x) (Just y) = f x y
liftEqMaybe _ _        _        = False

-- TODO
-- class GEq1 f where
--   gLiftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

-- class Eq1' (f :: * -> *) where
--   liftEq' :: (a -> b -> Bool) -> f a -> f b -> Bool
--   default
--     liftEq'
--       :: ( Generic (f a), Generic (f b)
--          , GEq1 (G.Rep (f a)), GEq1 (G.Rep (f b))
--          )
--       => (a -> b -> Bool) -> f a -> f b -> Bool
--   liftEq' f a b = gLiftEq f (G.from a) (G.from b)

-- newtype T a = T a deriving stock Generic

-- instance Eq1 T where
--   liftEq = liftEq'

newtype DefaultEq1DeriveForText it =
  DefaultEq1DeriveForText Text

instance Eq1 DefaultEq1DeriveForText where
  liftEq _ (DefaultEq1DeriveForText a) (DefaultEq1DeriveForText b) = a == b

newtype DefaultEq1DeriveFor1Field it =
  DefaultEq1DeriveFor1Field it

instance Eq1 DefaultEq1DeriveFor1Field where
  liftEq f (DefaultEq1DeriveFor1Field a) (DefaultEq1DeriveFor1Field b) = f a b

data DefaultEq1DeriveFor2Field it =
  DefaultEq1DeriveFor2Field it it

instance Eq1 DefaultEq1DeriveFor2Field where
  liftEq f (DefaultEq1DeriveFor2Field a b) (DefaultEq1DeriveFor2Field c d) = f a c && f b d

newtype DefaultEq1DeriveFor1List it =
  DefaultEq1DeriveFor1List [it]

instance Eq1 DefaultEq1DeriveFor1List where
  liftEq f (DefaultEq1DeriveFor1List a) (DefaultEq1DeriveFor1List b) = liftEqList f a b

--------------------------------------------------------------------------------
instance Eq1 Alt where
  liftEq f (Alt pa ea) (Alt pb eb) = f pa pb && f ea eb

instance Eq1 MapBinding where
  liftEq f (MapBinding a b) (MapBinding c d) = f a c && f b d

instance Eq1 RecordFieldPattern where
  liftEq f (IsRecordField la ba) (IsRecordField lb bb) = f la lb && f ba bb
  liftEq f (IsRecordCapture la) (IsRecordCapture lb) = f la lb
  liftEq _ _ _ = False

instance Eq1 FieldAssignment where
  liftEq f (FieldAssignment as a) (FieldAssignment bs b) =
    liftEqList f as bs && f a b
  liftEq f (Spread a) (Spread b) = f a b
  liftEq _ _ _ = False

instance Eq1 Constant where
  liftEq _ (CInt a) (CInt b) = a == b
  liftEq _ (CNat a) (CNat b) = a == b
  liftEq _ (CString a) (CString b) = a == b
  liftEq _ (CFloat a) (CFloat b) = a == b
  liftEq _ (CBytes a) (CBytes b) = a == b
  liftEq _ (CTez a) (CTez b) = a == b
  liftEq _ _ _ = False

-- FIXME: Missing a lot of comparisons!
instance Eq1 Expr where
  liftEq f (Constant a) (Constant b) = f a b
  liftEq f (Ident a) (Ident b) = f a b
  liftEq f (List as) (List bs) = liftEqList f as bs
  liftEq f (Tuple as) (Tuple bs) = liftEqList f as bs
  liftEq f (Annot ea ta) (Annot eb tb) = f ea eb && f ta tb
  liftEq f (Set xs) (Set ys) = liftEqList f xs ys
  liftEq f (Map xs) (Map ys) = liftEqList f xs ys
  liftEq f (BigMap xs) (BigMap ys) = liftEqList f xs ys
  liftEq _ _ _ = False

instance Eq1 PatchableExpr where
  liftEq f (PatchableExpr c1 a) (PatchableExpr c2 b) = f c1 c2 && f a b

instance Eq1 Collection where
  liftEq _ CList    CList   = True
  liftEq _ CMap     CMap    = True
  liftEq _ CSet     CSet    = True
  liftEq _ _        _       = False

instance Eq1 Error where
  -- liftEq _ _ _ = error "Cannot compare `Error` nodes"
  liftEq _ _ _ = False

instance Eq1 Binding where
  -- liftEq _ _ _ = error "Cannot compare `Binding`"
  liftEq _ _ _ = False

instance Eq1 TypeParams where
  liftEq f (TypeParam a) (TypeParam b) = f a b
  liftEq f (TypeParams as) (TypeParams bs) = liftEqList f as bs
  liftEq _ _ _ = False

instance Eq1 Type where
  liftEq f (TArrow a b) (TArrow c d) = f a c && f b d
  liftEq f (TRecord xs) (TRecord ys) = liftEqList f xs ys
  liftEq f (TSum xs) (TSum ys) = liftEqList f xs ys
  liftEq f (TProduct xs) (TProduct ys) = liftEqList f xs ys
  liftEq f (TString x) (TString y) = f x y
  liftEq _ TWildcard TWildcard = True
  liftEq f (TVariable a) (TVariable b) = f a b
  liftEq f (TParen a) (TParen b) = f a b
  liftEq _ _ _ = False

instance Eq1 Variant where
  liftEq f (Variant an (Just at)) (Variant bn (Just bt)) = f an bn && f at bt
  liftEq f (Variant an Nothing) (Variant bn Nothing) = f an bn
  liftEq _ _ _ = False

instance Eq1 TField where
  liftEq f (TField an at) (TField bn bt) = f an bn && liftEqMaybe f at bt

instance Eq1 ModuleAccess where
  liftEq f (ModuleAccess ap asrc) (ModuleAccess bp bsrc) =
    f asrc bsrc && liftEqList f ap bp

instance Eq1 QualifiedName where
  liftEq f (QualifiedName asrc ap) (QualifiedName bsrc bp) =
    f asrc bsrc && liftEqList f ap bp

instance Eq1 Pattern where
  liftEq f (IsConstr na mbpa) (IsConstr nb mbpb) =
    f na nb && liftEqMaybe f mbpa mbpb
  liftEq f (IsConstant a) (IsConstant b) = f a b
  liftEq f (IsVar a) (IsVar b) = f a b
  liftEq f (IsCons ha ta) (IsCons hb tb) = f ha hb && f ta tb
  liftEq f (IsAnnot pa ta) (IsAnnot pb tb) = f pa pb && f ta tb
  liftEq _ IsWildcard IsWildcard = True
  liftEq f (IsSpread a) (IsSpread b) = f a b
  liftEq f (IsList xa) (IsList xb) = liftEqList f xa xb
  liftEq f (IsTuple xa) (IsTuple xb) = liftEqList f xa xb
  liftEq _ _ _ = False

instance Eq1 CaseOrDefaultStm where
  liftEq f (CaseStm c s) (CaseStm c' s') =
    f c c' && liftEq f s s'
  liftEq f (DefaultStm s) (DefaultStm s') =
    liftEq f s s'
  liftEq _ _ _ = False
