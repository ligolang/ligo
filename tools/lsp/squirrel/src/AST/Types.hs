
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
  | WrongDecl      Error
  deriving (Show) via PP (Declaration info)

instance Stubbed (Declaration info) where stub = WrongDecl

data Binding info
  = Irrefutable  info (Pattern info) (Expr info)
  | Function     info Bool (Name info) [VarDecl info] (Type info) (Expr info)
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
  | Record    info [(Name info, Type info)]
  | TVar      info  (Name info)
  | Sum       info [(Name info, [Type info])]
  | Product   info  [Type info]
  | TApply    info  (Name info) [Type info]
  | WrongType      Error
  deriving (Show) via PP (Type info)

instance Stubbed (Type info) where stub = WrongType

data Expr info
  = Let       info [Declaration info] (Expr info)
  | Apply     info (Expr info) [Expr info]
  | Constant  info (Constant info)
  | Ident     info (QualifiedName info)
  | WrongExpr      Error
  deriving (Show) via PP (Expr info)

instance Stubbed (Expr info) where stub = WrongExpr

data Constant info
  = Int     info Int
  | String  info Text
  | Float   info Double
  | Bytes   info Text
  | WrongConstant Error
  deriving (Show) via PP (Constant info)

instance Stubbed (Constant info) where stub = WrongConstant

data Pattern info
  = IsConstr     info (Name info) [Pattern info]
  | IsConstant   info (Constant info)
  | IsVar        info (Name info)
  | WrongPattern      Error
  deriving (Show) via PP (Pattern info)

instance Stubbed (Pattern info) where stub = WrongPattern

data QualifiedName info
  = QualifiedName
    { qnInfo   :: info
    , qnSource :: Name info
    , qnPath   :: [Name info]
    }
  | WrongQualifiedName Error
  deriving (Show) via PP (QualifiedName info)

instance Stubbed (QualifiedName info) where stub = WrongQualifiedName

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
        vcat $ map (($$ empty) . pp) decls

    WrongContract err ->
      pp err

instance Pretty (Declaration i) where
  pp = \case
    ValueDecl _ binding -> pp binding
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
    Record    _ fields    -> wrap ["record [", "]"] $ vcat $ map ppField fields
    TVar      _ name      -> pp name
    Sum       _ variants  -> vcat $ map ppCtor variants
    Product   _ elements  -> fsep $ punctuate " *" $ map pp elements
    TApply    _ f xs      -> pp f <> parens (fsep $ punctuate "," $ map pp xs)
    WrongType   err       -> pp err
    where
      ppField (name, ty) = pp name <> ": " <> pp ty <> ";"
      ppCtor  (ctor, fields) =
        "|" <+> pp ctor <+> parens (fsep $ punctuate "," $ map pp fields)

instance Pretty (Expr i) where
  pp = \case
    Let       _ decls body -> hang "let" 2 (vcat $ map pp decls)
                           <> hang "in"  2 (pp body)
    Apply     _ f xs       -> pp f <> tuple xs
    Constant  _ constant   -> pp constant
    Ident     _ qname      -> pp qname
    WrongExpr   err        -> pp err


instance Pretty (Constant i) where
  pp = \case
    Int           _ c   -> int c
    String        _ c   -> doubleQuotes (pp c)
    Float         _ c   -> double c
    Bytes         _ c   -> pp c
    WrongConstant   err -> pp err

instance Pretty (QualifiedName i) where
  pp = \case
    QualifiedName _ src path -> pp src <> cat (map (("." <>) . pp) path)
    WrongQualifiedName err   -> pp err

instance Pretty (Pattern info) where
  pp = \case
    IsConstr     _ ctor args -> pp ctor <> tuple args
    IsConstant   _ c         -> pp c
    IsVar        _ name      -> pp name
    WrongPattern   err       -> pp err


instance Pretty (Name i) where
  pp = \case
    Name      _ raw -> pp raw
    WrongName err   -> pp err

tuple :: Pretty p => [p] -> Doc
tuple xs = parens (fsep $ punctuate "," $ map pp xs)