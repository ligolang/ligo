{-# LANGUAGE RecordWildCards #-}

module AST.Scope.ScopedDecl
  ( ScopedDecl (..)
  , sdName
  , sdOrigin
  , sdRefs
  , sdDoc
  , sdDialect
  , sdSpec
  , DeclarationSpecifics (..)
  , _TypeSpec
  , _ValueSpec
  , TypeDeclSpecifics (..)
  , tdsInitRange
  , tdsInit
  , Type (..)
  , TypeField (..)
  , TypeConstructor (..)
  , ValueDeclSpecifics (..)
  , vdsInitRange
  , vdsParams
  , vdsType
  , Parameter (..)

  , lppDeclCategory

  , fillTypeIntoCon
  ) where

import Control.Applicative ((<|>))
import Control.Lens ((%~), (&))
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Sum (inject)
import Data.Text (Text)
import Duplo.Tree (Cofree ((:<)), Element)

import AST.Pretty (Doc, Pretty (pp), lppDialect, sexpr)
import AST.Skeleton (LIGO, Lang, RawLigoList)
import qualified AST.Skeleton as LIGO
  (Ctor (..), FieldName (..), TField (..), Type (..), TypeName (..), Variant (..))
import Parser (fillInfo)
import Product (Product (Nil))
import Range (Range)

data ScopedDecl = ScopedDecl
  { _sdName :: Text
  , _sdOrigin :: Range
  , _sdRefs :: [Range]
  , _sdDoc :: [Text]
  , _sdDialect :: Lang
  , _sdSpec :: DeclarationSpecifics
  }

data DeclarationSpecifics
  = TypeSpec TypeDeclSpecifics
  | ValueSpec ValueDeclSpecifics

data TypeDeclSpecifics = TypeDeclSpecifics
  { _tdsInitRange :: Range
  , _tdsInit :: Type
  }

data Type
  = RecordType [TypeField]
  | VariantType [TypeConstructor]
  | TupleType [Type]
  | AliasType Text

data TypeField = TypeField
  { _tfName :: Text
  , _tfType :: Type
  }

data TypeConstructor = TypeConstructor
  { _tcName :: Text
  }

data ValueDeclSpecifics = ValueDeclSpecifics
  { _vdsInitRange :: Maybe Range
  , _vdsParams :: Maybe [Parameter] -- if there are any, it's a function
  , _vdsType :: Maybe Type
  }

newtype Parameter = Parameter
  { parPresentation :: Text
  }
  deriving stock Show
  deriving newtype Pretty

instance Eq ScopedDecl where
  sd1 == sd2 = and
    [ pp (_sdName sd1) == pp (_sdName sd2)
    , _sdOrigin sd1 == _sdOrigin sd2
    ]

instance Pretty ScopedDecl where
  pp (ScopedDecl n o refs doc _ _) =
    sexpr "decl" [pp n, pp o, pp refs, pp doc]

lppDeclCategory :: ScopedDecl -> Doc
lppDeclCategory decl = case _sdSpec decl of
  TypeSpec{} -> pp @Text "TYPE"
  ValueSpec vspec -> case _vdsType vspec of
    Nothing -> pp @Text "unknown"
    Just typ -> lppDialect (_sdDialect decl) (fillInfo (toLIGO typ))

class IsLIGO a where
  toLIGO :: a -> LIGO '[]

instance IsLIGO Type where
  toLIGO (RecordType fields) = node (LIGO.TRecord (map toLIGO fields))
  toLIGO (VariantType cons) = node (LIGO.TSum (map toLIGO cons))
  toLIGO (TupleType typs) = node (LIGO.TProduct (map toLIGO typs))
  toLIGO (AliasType typ) = node (LIGO.TypeName typ)

instance IsLIGO TypeField where
  toLIGO TypeField{ .. } = node
    (LIGO.TField (node (LIGO.FieldName _tfName)) (toLIGO _tfType))

instance IsLIGO TypeConstructor where
  toLIGO TypeConstructor{ .. } = node
    (LIGO.Variant (node (LIGO.Ctor _tcName)) Nothing)

node :: Element f RawLigoList => f (LIGO '[]) -> LIGO '[]
node element = Nil :< inject element

$(makeLenses ''ScopedDecl)
$(makePrisms ''DeclarationSpecifics)
$(makeLenses ''TypeDeclSpecifics)
$(makeLenses ''ValueDeclSpecifics)

-- | Assuming that 'typDecl' is a declaration of a type containing a constructor
-- that has a declaration 'conDecl', specify 'conDecl''s type as that of
-- 'typDecl'.
fillTypeIntoCon :: ScopedDecl -> ScopedDecl -> ScopedDecl
fillTypeIntoCon typDecl conDecl
  = conDecl & sdSpec . _ValueSpec . vdsType %~ (<|> Just typ)
  where
    typ = AliasType (_sdName typDecl)
