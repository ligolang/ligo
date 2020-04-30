
{- annotate tree with ranges, add offsets to ranges, store verbatim in Wrong* -}

module AST where

import Control.Monad.State

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void

import Parser
import ParseTree

import Debug.Trace

type TODO = Text

data Contract info
  = Contract      info [Declaration info]
  | WrongContract      Error
  deriving stock (Show)

instance Stubbed (Contract info) where stub = WrongContract

data Declaration info
  = ValueDecl info (Binding info)
  | WrongDecl      Error
  deriving stock (Show)

instance Stubbed (Declaration info) where stub = WrongDecl

data Binding info
  = Irrefutable  info (Pattern info) (Expr info)
  | Function     info Bool (Name info) [VarDecl info] (Type info) TODO
  | WrongBinding      Error
  deriving stock (Show)

instance Stubbed (Binding info) where stub = WrongBinding

data VarDecl info
  = Decl         info (Mutable info) (Name info) (Type info)
  | WrongVarDecl      Error
  deriving stock (Show)

instance Stubbed (VarDecl info) where stub = WrongVarDecl

data Mutable info
  = Mutable      info
  | Immutable    info
  | WrongMutable      Error
  deriving stock (Show)

instance Stubbed (Mutable info) where stub = WrongMutable

data Type info
  = TArrow    info  (Type info) (Type info)
  | Record    info [(Name info, Type info)]
  | TVar      info  (Name info)
  | Sum       info [(Name info, [Type info])]
  | Product   info  [Type info]
  | TApply    info  (Name info) [Type info]
  | WrongType      Error
  deriving stock (Show)

instance Stubbed (Type info) where stub = WrongType

data Expr info
  = Let       info [Declaration info] (Expr info)
  | Apply     info (Expr info) (Expr info)
  | Constant  info (Constant info)
  | Ident     info (QualifiedName info)
  | WrongExpr      Error
  deriving stock (Show)

instance Stubbed (Expr info) where stub = WrongExpr

data Constant info
  = Int     info Int
  | String  info Text
  | Float   info Double
  | Bytes   info Text
  | WrongConstant Error
  deriving stock (Show)

instance Stubbed (Constant info) where stub = WrongConstant

data Pattern info
  = IsConstr     info (Name info) [Pattern info]
  | IsConstant   info (Constant info)
  | IsVar        info (Name info)
  | WrongPattern      Error
  deriving stock (Show)

instance Stubbed (Pattern info) where stub = WrongPattern

data QualifiedName info = QualifiedName
  { source :: Name info
  , path   :: [Name info]
  }
  | WrongQualifiedName Error
  deriving stock (Show)

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

name :: Parser (Name Range)
name = do
  (raw, info) <- range (token "Name")
  return Name {info, raw}

contract :: Parser (Contract Range)
contract = subtree "contract" do
  (decls, info) <- range $ many "declaration" declaration
  return (Contract info decls)

declaration :: Parser (Declaration Range)
declaration =
  field "declaration" do
    (b, info) <- range binding
    return (ValueDecl info b)

par x = do
  consume "("
  a <- x
  consume ")"
  return a

binding :: Parser (Binding Range)
binding = do
  info <- getRange
  "fun_decl" `subtree` do
    recur <- optional do
      field "recursive" do
        token "recursive"
    consume "function"
    name <- field "name" do
      name
    params <- field "parameters" $ par (many "param" paramDecl)
    consume ":"
    ty     <- field "type" type_
    consume "is"
    get >>= traceShowM
    expr   <- field "locals" anything
    return (Function info (recur == Just "recursive") name params ty expr)

paramDecl :: Parser (VarDecl Range)
paramDecl = do
  info <- getRange
  "parameter" `field` do
    info' <- getRange
    mutable <- do
      "access" `subtree` select
        [ do consume "var"
             return $ Mutable info'
        , do consume "const"
             return $ Immutable info'
        ]
    name <- field "name" name
    consume ":"
    ty <- field "type" type_
    return (Decl info mutable name ty)

newtype_ = do
  type_

type_ :: Parser (Type Range)
type_ =
    fun_type
  where
    fun_type :: Parser (Type Range)
    fun_type = do
      info <- getRange
      domain <- field "domain" cartesian
      codomain <- optional do
        consume "->"
        fun_type
      return case codomain of
        Just co -> TArrow info domain co
        Nothing -> domain

    cartesian = do
      info <- getRange
      Product info <$> some "corety" core_type

    core_type = do
      info <- getRange
      select
        [ TVar info <$> typename
        ]

    typename = name

tuple :: Text -> Parser a -> Parser [a]
tuple msg = par . some msg

example = "../../ligo/src/test/contracts/address.ligo"