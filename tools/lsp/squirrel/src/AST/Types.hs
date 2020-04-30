
{- TODO(kirill.andreev): add offsets to ranges, store verbatim in Wrong* -}

module AST.Types where

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
  | Function     info Bool (Name info) [VarDecl info] (Type info) (Expr info)
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
  | Apply     info (Expr info) [Expr info]
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

data QualifiedName info
  = QualifiedName
    { qnInfo   :: info
    , qnSource :: Name info
    , qnPath   :: [Name info]
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
