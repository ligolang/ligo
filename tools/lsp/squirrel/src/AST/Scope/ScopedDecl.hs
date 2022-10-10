module AST.Scope.ScopedDecl
  ( Namespace (..)
  , Scope
  , ScopedDecl (..), sdName, sdOrigin, sdRefs, sdDoc, sdDialect, sdSpec, sdNamespace
  , DeclarationSpecifics (..), _TypeSpec, _ModuleSpec, _ValueSpec
  , TypeVariable (..), tvName
  , TypeParams (..), _TypeParam, _TypeParams
  , TypeDeclSpecifics (..), tdsInitRange, tdsInit
  , Type (..)
  , _RecordType
  , TypeField (..), tfName, tfTspec
  , TypeConstructor (..)
  , ModuleDeclSpecifics (..), mdsInitRange, mdsInit, mdsName
  , Module (..)
  , ValueDeclSpecifics (..), vdsInitRange, vdsParams, vdsTspec
  , Parameter (..)

  , Constant (..)
  , Pattern (..)
  , RecordFieldPattern (..)

  , IsLIGO (..)

  , Accessor
  , accessField
  , lppDeclCategory
  , lppLigoLike
  , fillTypeIntoCon
  , fillTypeParams
  , extractRefName
  ) where

import Control.Applicative ((<|>))
import Control.Lens ((%~), (&), (^?))
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Hashable (Hashable)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sum (inject)
import Data.Text (Text)
import Duplo.Tree (Cofree ((:<)), Element)

import AST.Pretty (Doc, Pretty (pp), lppDialect, sexpr, (<+>))
import AST.Skeleton (LIGO, Lang, RawLigoList)
import AST.Skeleton qualified as LIGO
import Parser (fillInfo)
import Product (Product (Nil))
import Range (Range)
import Util (safeIndex)

newtype Namespace = Namespace
  { getNamespace :: [Text]
  } deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable, Semigroup, Monoid)

instance Pretty Namespace where
  pp (Namespace ns) = sexpr "::" (map pp ns)

type Scope = [ScopedDecl]

data ScopedDecl = ScopedDecl
  { _sdName :: Text
  , _sdOrigin :: Range
  , _sdRefs :: [Range]
  , _sdDoc :: [Text]
  , _sdDialect :: Lang
  , _sdSpec :: DeclarationSpecifics
  , _sdNamespace :: Namespace
  } deriving stock (Show)

data DeclarationSpecifics
  = TypeSpec (Maybe TypeParams) (TypeDeclSpecifics Type)
  | ModuleSpec ModuleDeclSpecifics
  | ValueSpec ValueDeclSpecifics
  deriving stock (Show)

data TypeParams
  = TypeParam (TypeDeclSpecifics TypeVariable)
  | TypeParams [TypeDeclSpecifics TypeVariable]
  deriving stock (Show)

newtype TypeVariable = TypeVariable
  { _tvName :: Text
  }
  deriving stock (Eq, Show)

data TypeDeclSpecifics init = TypeDeclSpecifics
  { _tdsInitRange :: Range
  , _tdsInit :: init
  }
  deriving stock (Eq, Show)

data Type
  = RecordType [TypeField]
  | VariantType (NonEmpty TypeConstructor)
  | TupleType [TypeDeclSpecifics Type]
  | ApplyType Type [Type]
  | AliasType Text
  | ArrowType Type Type
  | VariableType TypeVariable
  | ParenType (TypeDeclSpecifics Type)
  deriving stock (Eq, Show)

data TypeField = TypeField
  { _tfName :: Text
  , _tfTspec :: Maybe (TypeDeclSpecifics Type)
  }
  deriving stock (Eq, Show)

data TypeConstructor = TypeConstructor
  { _tcName :: Text
  , _tcTspec :: Maybe (TypeDeclSpecifics Type)
  }
  deriving stock (Eq, Show)

data ModuleDeclSpecifics = ModuleDeclSpecifics
  { _mdsInitRange :: Range
  -- TODO: parse specifics of module declarations
  -- This will require twaking the parser to handle anything from a 'Binding'
  , _mdsInit :: Module
  , _mdsName :: Text
  } deriving stock (Eq, Show)

data Module
  = ModuleDecl
  | ModuleAlias Namespace
  deriving stock (Eq, Show)

data ValueDeclSpecifics = ValueDeclSpecifics
  { _vdsInitRange :: Maybe Range
  , _vdsParams :: Maybe [Parameter] -- if there are any, it's a function
  , _vdsTspec :: Maybe (TypeDeclSpecifics Type)
  }
  deriving stock (Eq, Show)

data Parameter
  = ParameterPattern Pattern
  | ParameterBinding Pattern (Maybe Type)
  deriving stock (Eq, Show)

data Constant
  = CInt    Text
  | CNat    Text
  | CString Text
  | CFloat  Text
  | CBytes  Text
  | CTez    Text
  deriving stock (Eq, Show)

data Pattern
  = IsConstr     Text (Maybe Pattern)
  | IsConstant   Constant
  | IsVar        Text
  | IsCons       Pattern Pattern
  | IsAnnot      Pattern Type  -- Semantically `Var`
  | IsWildcard
  | IsSpread     Pattern
  | IsList       [Pattern]
  | IsTuple      [Pattern]
  | IsRecord     [RecordFieldPattern]
  | IsParen      Pattern
  deriving stock (Eq, Show)

data RecordFieldPattern
  = IsRecordField Text Pattern
  | IsRecordCapture Text
  deriving stock (Eq, Show)

instance Eq ScopedDecl where
  sd1 == sd2 =
    _sdNamespace sd1 == _sdNamespace sd2 &&
    _sdName sd1 == _sdName sd2 &&
    _sdOrigin sd1 == _sdOrigin sd2

instance Ord ScopedDecl where
  sd1 `compare` sd2 =
    _sdNamespace sd1 `compare` _sdNamespace sd2 <>
    _sdName sd1 `compare` _sdName sd2 <>
    _sdOrigin sd1 `compare` _sdOrigin sd2

instance Pretty ScopedDecl where
  pp (ScopedDecl n o refs doc _ _ ns) =
    sexpr "decl" [pp n, pp ns, pp o, pp refs, pp doc]

lppDeclCategory :: ScopedDecl -> Doc
lppDeclCategory decl = case _sdSpec decl of
  TypeSpec tparams tspec ->
    maybe mempty (lppLigoLike dialect) tparams <+> lppLigoLike dialect tspec
  ModuleSpec mspec ->
    pp $ _mdsName mspec
  ValueSpec vspec -> case _vdsTspec vspec of
    Nothing -> pp @Text "unknown"
    Just tspec -> lppLigoLike dialect tspec
  where
    dialect = _sdDialect decl

lppLigoLike :: IsLIGO a => Lang -> a -> Doc
lppLigoLike dialect ligoLike = lppDialect dialect (fillInfo (toLIGO ligoLike))

class IsLIGO a where
  toLIGO :: a -> LIGO '[]

instance IsLIGO init => IsLIGO (TypeDeclSpecifics init) where
  toLIGO tspec = toLIGO (_tdsInit tspec)

instance IsLIGO Type where
  toLIGO (RecordType fields) = node (LIGO.TRecord (map toLIGO fields))
  toLIGO (VariantType cons) = node (LIGO.TSum (toLIGO <$> cons))
  toLIGO (TupleType typs) = node (LIGO.TProduct (map toLIGO typs))
  toLIGO (AliasType typ) = node (LIGO.TypeName typ)
  toLIGO (ApplyType name types) = node (LIGO.TApply (toLIGO name) (map toLIGO types))
  toLIGO (ArrowType left right) = node (LIGO.TArrow (toLIGO left) (toLIGO right))
  toLIGO (VariableType var) = node (LIGO.TVariable (toLIGO var))
  toLIGO (ParenType typ) = node (LIGO.TParen (toLIGO typ))

instance IsLIGO TypeParams where
  toLIGO (TypeParam t) = node (LIGO.TypeParam (toLIGO t))
  toLIGO (TypeParams ts) = node (LIGO.TypeParams (map toLIGO ts))

instance IsLIGO TypeVariable where
  toLIGO (TypeVariable t) = node (LIGO.TypeVariableName t)

instance IsLIGO TypeField where
  toLIGO TypeField{ .. } = node
    (LIGO.TField (node (LIGO.FieldName _tfName)) (toLIGO <$> _tfTspec))

instance IsLIGO TypeConstructor where
  toLIGO TypeConstructor{ .. } = node
    (LIGO.Variant (node (LIGO.Ctor _tcName)) (toLIGO <$> _tcTspec))

instance IsLIGO Parameter where
  toLIGO (ParameterPattern pat) = toLIGO pat
  toLIGO (ParameterBinding pat typM) = node (LIGO.BParameter (toLIGO pat) (toLIGO <$> typM))

instance IsLIGO Constant where
  toLIGO (CInt i) = node (LIGO.CInt i)
  toLIGO (CNat n) = node (LIGO.CNat n)
  toLIGO (CString s) = node (LIGO.CString s)
  toLIGO (CFloat f) = node (LIGO.CFloat f)
  toLIGO (CBytes b) = node (LIGO.CBytes b)
  toLIGO (CTez t) = node (LIGO.CTez t)

instance IsLIGO Pattern where
  toLIGO (IsConstr name patM) = node (LIGO.IsConstr (node (LIGO.Ctor name)) (toLIGO <$> patM))
  toLIGO (IsConstant constant) = node (LIGO.IsConstant (toLIGO constant))
  toLIGO (IsVar name) = node (LIGO.IsVar (node (LIGO.NameDecl name)))
  toLIGO (IsCons left right) = node (LIGO.IsCons (toLIGO left) (toLIGO right))
  toLIGO (IsAnnot pat typ) = node (LIGO.IsAnnot (toLIGO pat) (toLIGO typ))
  toLIGO IsWildcard = node LIGO.IsWildcard
  toLIGO (IsSpread pat) = node (LIGO.IsSpread (toLIGO pat))
  toLIGO (IsList pats) = node (LIGO.IsList (toLIGO <$> pats))
  toLIGO (IsTuple pats) = node (LIGO.IsTuple (toLIGO <$> pats))
  toLIGO (IsRecord pats) = node (LIGO.IsRecord (toLIGO <$> pats))
  toLIGO (IsParen pat) = node (LIGO.IsParen (toLIGO pat))

instance IsLIGO RecordFieldPattern where
  toLIGO (IsRecordField name body) = node (LIGO.IsRecordField (node (LIGO.FieldName name)) (toLIGO body))
  toLIGO (IsRecordCapture name) = node (LIGO.IsRecordCapture (node (LIGO.NameDecl name)))

node :: Element f RawLigoList => f (LIGO '[]) -> LIGO '[]
node element = Nil :< inject element

$(makeLenses ''ScopedDecl)
$(makePrisms ''DeclarationSpecifics)
$(makeLenses ''TypeDeclSpecifics)
$(makeLenses ''ModuleDeclSpecifics)
$(makeLenses ''ValueDeclSpecifics)
$(makePrisms ''Type)
$(makePrisms ''TypeParams)
$(makeLenses ''TypeVariable)
$(makeLenses ''TypeField)

-- | Assuming that 'typDecl' is a declaration of a type containing a constructor
-- that has a declaration 'conDecl', specify 'conDecl''s type as that of
-- 'typDecl'.
fillTypeIntoCon :: ScopedDecl -> ScopedDecl -> ScopedDecl
fillTypeIntoCon typDecl conDecl
  = conDecl & sdSpec . _ValueSpec . vdsTspec %~ (<|> Just tspec)
  where
    typ = AliasType (_sdName typDecl)
    tspec = TypeDeclSpecifics
      { _tdsInitRange = _sdOrigin typDecl
      , _tdsInit = typ
      }

-- | Assuming that 'typeDecl' contains a '_sdSpec' which is a 'TypeSpec', try to
-- fill its field with the provided params, if they are not filled already.
fillTypeParams :: TypeParams -> ScopedDecl -> ScopedDecl
fillTypeParams newParams typeDecl = typeDecl
  { _sdSpec = case _sdSpec typeDecl of
      TypeSpec oldParams tspec -> TypeSpec (oldParams <|> Just newParams) tspec
      spec                     -> spec
  }

-- | If the type is just a reference to another type, extract a name of that
-- reference.
extractRefName :: Type -> Maybe Text
extractRefName typ = typ ^? _AliasType

type Accessor = Either Int Text

accessField :: TypeDeclSpecifics Type -> Accessor -> Maybe (TypeDeclSpecifics Type)
accessField tspec (Left num) = do
  tupleTspecs <- tspec ^? tdsInit . _TupleType
  safeIndex tupleTspecs num
accessField tspec (Right text) = do
  typeFields <- tspec ^? tdsInit . _RecordType
  fitting <- find ((text ==) . _tfName) typeFields
  _tfTspec fitting
