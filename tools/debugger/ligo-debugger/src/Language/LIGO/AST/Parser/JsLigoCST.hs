module Language.LIGO.AST.Parser.JsLigoCST
  ( CST
  , toAST
  ) where

import Prelude hiding (Const, Element)

import Control.MessagePack (asumMsg, guardMsg, withMsgMap, withMsgVariant, (.:), (.:?))
import Data.Default (def)
import Data.MessagePack (MessagePack)
import Data.MessagePack.Types (MessagePack (..))

import Duplo (fastMake)

import Language.LIGO.AST.Parser.Common
import Language.LIGO.AST.Skeleton (Info, LIGO)
import Language.LIGO.AST.Skeleton qualified as AST
import Language.LIGO.Range

-----------
-- Types --
-----------

-- All these types are mostly mappings
-- for the corresponding types from @CST.ml@.

data CST = CST
  { cstStatements :: Statements
  , cstEof :: WrappedLexeme
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type Statements = [Tuple1 Statement]

data Statement
  = SAttr (Tuple1 Statement)
  | SBlock (Par Statements)
  | SBreak WrappedLexeme
  | SContinue WrappedLexeme
  | SDecl Declaration
  | SExport (Reg ExportStmt)
  | SExpr Expr
  | SFor (Reg ForStmt)
  | SForOf (Reg ForOfStmt)
  | SIf (Reg IfStmt)
  | SReturn (Reg ReturnStmt)
  | SSwitch (Reg SwitchStmt)
  | SWhile (Reg WhileStmt)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type Par a = Reg (Par' a)
newtype Par' a = Par'
  { pInside :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Declaration
  = DFun (Reg FunDecl)
  | DImport ImportDecl
  | DInterface (Reg InterfaceDecl)
  | DNamespace (Reg NamespaceDecl)
  | DType (Reg TypeDecl)
  | DValue (Reg ValueDecl)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type ExportStmt = Tuple1 Declaration

data BinOp a = BinOp
  { boArg1 :: Expr
  , boOp :: a
  , boArg2 :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data UnOp a = UnOp
  { uoOp :: a
  , uoArg :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type SomeBinOp = Reg (BinOp WrappedLexeme)
type SomeUnOp = Reg (UnOp WrappedLexeme)

data Expr
  = EAdd SomeBinOp
  | EAddEq SomeBinOp
  | EAnd SomeBinOp
  | EApp (Reg (Expr, Arguments))
  | EArray (Array Expr)
  | EArrowFun (Reg ArrowFunExpr)
  | EAssign SomeBinOp
  | EAttr (Tuple1 Expr)
  | EBitAnd SomeBinOp
  | EBitAndEq SomeBinOp
  | EBitNeg SomeUnOp
  | EBitOr SomeBinOp
  | EBitOrEq SomeBinOp
  | EBitSl SomeBinOp
  | EBitSlEq SomeBinOp
  | EBitSr SomeBinOp
  | EBitSrEq SomeBinOp
  | EBitXor SomeBinOp
  | EBitXorEq SomeBinOp
  | EBytes WrappedTupleLexeme
  | ECodeInj (Reg CodeInj)
  | EContractOf (Reg ContractOfExpr)
  | ECtorApp (VariantKind Expr)
  | EDiv SomeBinOp
  | EDivEq SomeBinOp
  | EDo (Reg DoExpr)
  | EEqual SomeBinOp
  | EFalse WrappedLexeme
  | EFunction (Reg FunctionExpr)
  | EGeq SomeBinOp
  | EGt SomeBinOp
  | EInt WrappedTupleLexeme
  | ELeq SomeBinOp
  | ELt SomeBinOp
  | EMatch (Reg MatchExpr)
  | EMult SomeBinOp
  | EMultEq SomeBinOp
  | EMutez WrappedTupleLexeme
  | ENamePath (Reg (NamespacePath Expr))
  | ENat WrappedTupleLexeme
  | ENeg SomeUnOp
  | ENeq SomeBinOp
  | ENot SomeUnOp
  | EObject (Object Expr)
  | EOr SomeBinOp
  | EPar (Par Expr)
  | EPostDecr SomeUnOp
  | EPostIncr SomeUnOp
  | EPreDecr SomeUnOp
  | EPreIncr SomeUnOp
  | EProj (Reg Projection)
  | ERem SomeBinOp
  | ERemEq SomeBinOp
  | EString WrappedLexeme
  | ESub SomeBinOp
  | ESubEq SomeBinOp
  | ETernary (Reg Ternary)
  | ETrue WrappedLexeme
  | ETyped (Reg TypedExpr)
  | EUpdate (Par UpdateExpr)
  | EVar WrappedLexeme
  | EVerbatim WrappedLexeme
  | EXor SomeBinOp
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type Array a = Par [Element a]
type Element a = Tuple1 a

data ForStmt = ForStmt
  { fsRange :: Par RangeFor
  , fsForBody :: Maybe Statement
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data RangeFor = RangeFor
  { rfInitialiser :: Maybe Statement
  , rfCondition :: Maybe Expr
  , rfAfterthought :: Maybe [Expr]
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ForOfStmt = ForOfStmt
  { fosRange :: Par RangeOf
  , fosForOfBody :: Statement
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data RangeOf = RangeOf
  { roIndex :: WrappedLexeme
  , roExpr :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data VarKind
  = Let
  | Const
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data IfStmt = IfStmt
  { isTest :: Par Expr
  , isIfSo :: Tuple1 Statement
  , isIfNot :: Maybe (Tuple1 Statement)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type ReturnStmt = Tuple1 (Maybe Expr)

data SwitchStmt = SwitchStmt
  { ssSubject :: Par Expr
  , ssCases :: Par Cases
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Cases
  = AllCases AllCases
  | Default (Reg SwitchDefault)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type AllCases = ([Reg SwitchCase], Maybe (Reg SwitchDefault))

data SwitchCase = SwitchCase
  { scExpr :: Expr
  , scCaseBody :: Maybe Statements
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype SwitchDefault = SwitchDefault
  { sdDefaultBody :: Maybe Statements
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data WhileStmt = WhileStmt
  { wsInvariant :: Par Expr
  , wsWhileBody :: Statement
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data FunDecl = FunDecl
  { fdFunName :: WrappedLexeme
  , fdGenerics :: Maybe Generics
  , fdParameters :: FunParams
  , fdRhsType :: Maybe TypeAnnotation
  , fdFunBody :: Par Statements
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type TypeAnnotation = Tuple1 TypeExpr

data TypeExpr
  = TApp (Reg (TypeExpr, TypeCtorArgs))
  | TAttr (Tuple1 TypeExpr)
  | TArray ArrayType
  | TFun FunType
  | TInt WrappedTupleLexeme
  | TNamePath (Reg (NamespacePath TypeExpr))
  | TObject (Object TypeExpr)
  | TPar (Par TypeExpr)
  | TParameterOf (Reg ParameterOfType)
  | TString WrappedLexeme
  | TUnion UnionType
  | TVar WrappedLexeme
  | TVariant VariantType
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type VariantType = Reg (NonEmpty (VariantKind TypeExpr))

data VariantKind a
  = Variant (Reg (Variant a))
  | Bracketed (Reg (BracketedVariant a))
  | Legacy (Reg (LegacyVariant a))
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype Variant a = VariantC
  { vTuple :: CtorApp a
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype BracketedVariant a = BracketedVariant
  { bvTuple :: Par (BracketedVariantArgs a)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data BracketedVariantArgs a = BracketedVariantArgs
  { bvaCtor :: a
  , bvaArgs :: Maybe (Tuple1 [a])
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype LegacyVariant a = LegacyVariant
  { lvTuple :: Par (LegacyVariantArgs a)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data LegacyVariantArgs a = LegacyVariantArgs
  { lvaCtor :: WrappedLexeme
  , lvaArgs :: [Tuple1 a]
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type UnionType = Reg (NonEmpty (Object TypeExpr))

newtype ParameterOfType = ParameterOfType
  { potNamespacePath :: NamespaceSelection
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data NamespaceSelection
  = MPath (Reg (NamespacePath WrappedLexeme))
  | MAlias WrappedLexeme
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type FunType = Reg (FunTypeParams, TypeExpr)

type FunTypeParams = Par [Reg FunTypeParam]

type FunTypeParam = (Pattern, TypeAnnotation)

data Pattern
  = PArray (Array Pattern)
  | PAttr (Tuple1 Pattern)
  | PBytes WrappedTupleLexeme
  | PCtorApp (VariantKind Pattern)
  | PFalse WrappedLexeme
  | PInt WrappedTupleLexeme
  | PMutez WrappedTupleLexeme
  | PNamePath (Reg (NamespacePath Pattern))
  | PNat WrappedTupleLexeme
  | PObject (Object Pattern)
  | PString WrappedLexeme
  | PTrue WrappedLexeme
  | PTyped (Reg TypedPattern)
  | PVar WrappedLexeme
  | PVerbatim WrappedLexeme
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type TypedPattern = (Pattern, TypeAnnotation)

type ArrayType = Par [TypeExpr]

type TypeCtorArgs = Par [TypeExpr]

type FunParams = Par [Pattern]

type Generics = Par [WrappedLexeme]

data ImportDecl
  = ImportAlias (Reg ImportAlias)
  | ImportAllAs (Reg ImportAllAs)
  | ImportFrom (Reg ImportFrom)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ImportAlias = ImportAliasC
  { iacAlias :: WrappedLexeme
  , iacNamespacePath :: NamespaceSelection
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ImportAllAs = ImportAllAsC
  { iaacAlias :: WrappedLexeme
  , iaacFilePath :: WrappedLexeme
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ImportFrom = ImportFromC
  { ifcImported :: Par [WrappedLexeme]
  , ifcFilePath :: WrappedLexeme
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data InterfaceDecl = InterfaceDecl
  { idIntfName :: WrappedLexeme
  , idIntfBody :: IntfBody
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type IntfBody = Par IntrEntries

type IntrEntries = [IntfEntry]

data IntfEntry
  = IAttr (Tuple1 IntfEntry)
  | IType (Reg IntfType)
  | IConst (Reg IntfConst)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data IntfType = IntfType
  { itTypeName :: WrappedLexeme
  , itTypeRhs :: Maybe (Tuple1 TypeExpr)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data IntfConst = IntfConst
  { icConstName :: WrappedLexeme
  , icConstType :: TypeAnnotation
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data NamespaceDecl = NamespaceDecl
  { ndNamespaceName :: WrappedLexeme
  , ndNamespaceType :: Maybe Interface
  , ndNamespaceBody :: Par Statements
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type Interface = Reg (Tuple1 IntfExpr)

data IntfExpr
  = IBody IntfBody
  | IPath NamespaceSelection
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data TypeDecl = TypeDecl
  { tdName :: WrappedLexeme
  , tdGenerics :: Maybe Generics
  , tdTypeExpr :: TypeExpr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ValueDecl = ValueDecl
  { vdKind :: VarKind
  , vdBindings :: NonEmpty (Reg ValBinding)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ValBinding = ValBinding
  { vbPattern :: Pattern
  , vbGenerics :: Maybe Generics
  , vbRhsType :: Maybe TypeAnnotation
  , vbRhsExpr :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type Arguments = Par [Expr]

data ArrowFunExpr = ArrowFunExpr
  { afeGenerics :: Maybe Generics
  , afeParameters :: ArrowFunParams
  , afeRhsType :: Maybe TypeAnnotation
  , afeFunBody :: FunBody
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data FunBody
  = StmtBody (Par Statements)
  | ExprBody Expr
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ArrowFunParams
  = ParParams FunParams
  | NakedParam Pattern
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data CodeInj = CodeInj
  { ciLanguage :: WrappedLexeme
  , ciCode :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype ContractOfExpr = ContractOfExpr
  { coeNamespacePath :: Par NamespaceSelection
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type CtorApp a = Tuple1 (App a)

data App a
  = ZeroArg CtorAppKind
  | MultArg (CtorAppKind, Par (NonEmpty a))
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data CtorAppKind
  = CtorStr WrappedLexeme
  | CtorName WrappedLexeme
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype DoExpr = DoExpr
  { deStatements :: Par Statements
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data FunctionExpr = FunctionExpr
  { feGenerics :: Maybe Generics
  , feParameters :: ArrowFunParams
  , feRhsType :: Maybe TypeAnnotation
  , feFunBody :: FunBody
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data MatchExpr = MatchExpr
  { meSubject :: Par Expr
  , meClauses :: Par MatchClauses
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data MatchClauses
  = AllClauses AllMatchClauses
  | DefaultClause (Reg MatchDefault)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type AllMatchClauses = ([Reg MatchClause], Maybe (Reg MatchDefault))

data MatchClause = MatchClause
  { mcFilter :: Par Pattern
  , mcClauseExpr :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype MatchDefault = MatchDefault
  { mdDefaultExpr :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data NamespacePath a = NamespacePath
  { npNamespacePath :: [WrappedLexeme]
  , npProperty :: a
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

type Object a = Par [Reg (Property a)]

data Property a = Property
  { pPropertyId :: PropertyId
  , pPropertyRhs :: Maybe (Tuple1 a)
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

data PropertyId
  = FInt WrappedTupleLexeme
  | FName WrappedLexeme
  | FStr WrappedLexeme
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Projection = Projection
  { pObjectOrArray :: Expr
  , pPropertyPath :: [Selection]
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Selection
  = PropertyName (Tuple1 WrappedLexeme)
  | PropertyStr (Par WrappedLexeme)
  | Component (Par WrappedTupleLexeme)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Ternary = Ternary
  { tCondition :: Expr
  , tTruthy :: Expr
  , tFalsy :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type TypedExpr = (Expr, TypeExpr)

data UpdateExpr = UpdateExpr
  { ueObject :: Expr -- field called _object
  , ueUpdates :: [Reg (Property Expr)]
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-----------------
-- MessagePack --
-----------------

instance MessagePack CST where
  fromObjectWith _ = withMsgMap "CST" \o -> do
    cstStatements <- o .: "statements"
    cstEof <- o .: "eof"
    pure CST{..}

instance MessagePack Statement where
  fromObjectWith cfg = withMsgVariant "Statement" \(name, arg) -> asumMsg
    [ SAttr     <$> (guardMsg (name == "S_Attr"    ) >> fromObjectWith cfg arg)
    , SBlock    <$> (guardMsg (name == "S_Block"   ) >> fromObjectWith cfg arg)
    , SBreak    <$> (guardMsg (name == "S_Break"   ) >> fromObjectWith cfg arg)
    , SContinue <$> (guardMsg (name == "S_Continue") >> fromObjectWith cfg arg)
    , SDecl     <$> (guardMsg (name == "S_Decl"    ) >> fromObjectWith cfg arg)
    , SExport   <$> (guardMsg (name == "S_Export"  ) >> fromObjectWith cfg arg)
    , SExpr     <$> (guardMsg (name == "S_Expr"    ) >> fromObjectWith cfg arg)
    , SFor      <$> (guardMsg (name == "S_For"     ) >> fromObjectWith cfg arg)
    , SForOf    <$> (guardMsg (name == "S_ForOf"   ) >> fromObjectWith cfg arg)
    , SIf       <$> (guardMsg (name == "S_If"      ) >> fromObjectWith cfg arg)
    , SReturn   <$> (guardMsg (name == "S_Return"  ) >> fromObjectWith cfg arg)
    , SSwitch   <$> (guardMsg (name == "S_Switch"  ) >> fromObjectWith cfg arg)
    , SWhile    <$> (guardMsg (name == "S_While"   ) >> fromObjectWith cfg arg)
    ]

instance (MessagePack a) => MessagePack (Par' a) where
  fromObjectWith _ = withMsgMap "Par'" \o -> do
    pInside <- o .: "inside"
    pure Par'{..}

instance MessagePack Declaration where
  fromObjectWith cfg = withMsgVariant "Declaration" \(name, arg) -> asumMsg
    [ DFun       <$> (guardMsg (name == "D_Fun"      ) >> fromObjectWith cfg arg)
    , DImport    <$> (guardMsg (name == "D_Import"   ) >> fromObjectWith cfg arg)
    , DInterface <$> (guardMsg (name == "D_Interface") >> fromObjectWith cfg arg)
    , DNamespace <$> (guardMsg (name == "D_Namespace") >> fromObjectWith cfg arg)
    , DType      <$> (guardMsg (name == "D_Type"     ) >> fromObjectWith cfg arg)
    , DValue     <$> (guardMsg (name == "D_Value"    ) >> fromObjectWith cfg arg)
    ]

instance MessagePack Expr where
  fromObjectWith cfg = withMsgVariant "Expr" \(name, arg) -> asumMsg
    [ EAdd        <$> (guardMsg (name == "E_Add"       ) >> fromObjectWith cfg arg)
    , EAddEq      <$> (guardMsg (name == "E_AddEq"     ) >> fromObjectWith cfg arg)
    , EAnd        <$> (guardMsg (name == "E_And"       ) >> fromObjectWith cfg arg)
    , EApp        <$> (guardMsg (name == "E_App"       ) >> fromObjectWith cfg arg)
    , EArray      <$> (guardMsg (name == "E_Array"     ) >> fromObjectWith cfg arg)
    , EArrowFun   <$> (guardMsg (name == "E_ArrowFun"  ) >> fromObjectWith cfg arg)
    , EAssign     <$> (guardMsg (name == "E_Assign"    ) >> fromObjectWith cfg arg)
    , EAttr       <$> (guardMsg (name == "E_Attr"      ) >> fromObjectWith cfg arg)
    , EBitAnd     <$> (guardMsg (name == "E_BitAnd"    ) >> fromObjectWith cfg arg)
    , EBitAndEq   <$> (guardMsg (name == "E_BitAndEq"  ) >> fromObjectWith cfg arg)
    , EBitNeg     <$> (guardMsg (name == "E_BitNeg"    ) >> fromObjectWith cfg arg)
    , EBitOr      <$> (guardMsg (name == "E_BitOr"     ) >> fromObjectWith cfg arg)
    , EBitOrEq    <$> (guardMsg (name == "E_BitOrEq"   ) >> fromObjectWith cfg arg)
    , EBitSl      <$> (guardMsg (name == "E_BitSl"     ) >> fromObjectWith cfg arg)
    , EBitSlEq    <$> (guardMsg (name == "E_BitSlEq"   ) >> fromObjectWith cfg arg)
    , EBitSr      <$> (guardMsg (name == "E_BitSr"     ) >> fromObjectWith cfg arg)
    , EBitSrEq    <$> (guardMsg (name == "E_BitSrEq"   ) >> fromObjectWith cfg arg)
    , EBitXor     <$> (guardMsg (name == "E_BitXor"    ) >> fromObjectWith cfg arg)
    , EBitXorEq   <$> (guardMsg (name == "E_BitXorEq"  ) >> fromObjectWith cfg arg)
    , EBytes      <$> (guardMsg (name == "E_Bytes"     ) >> fromObjectWith cfg arg)
    , ECodeInj    <$> (guardMsg (name == "E_CodeInj"   ) >> fromObjectWith cfg arg)
    , EContractOf <$> (guardMsg (name == "E_ContractOf") >> fromObjectWith cfg arg)
    , ECtorApp    <$> (guardMsg (name == "E_CtorApp"   ) >> fromObjectWith cfg arg)
    , EDiv        <$> (guardMsg (name == "E_Div"       ) >> fromObjectWith cfg arg)
    , EDivEq      <$> (guardMsg (name == "E_DivEq"     ) >> fromObjectWith cfg arg)
    , EDo         <$> (guardMsg (name == "E_Do"        ) >> fromObjectWith cfg arg)
    , EEqual      <$> (guardMsg (name == "E_Equal"     ) >> fromObjectWith cfg arg)
    , EFalse      <$> (guardMsg (name == "E_False"     ) >> fromObjectWith cfg arg)
    , EFunction   <$> (guardMsg (name == "E_Function"  ) >> fromObjectWith cfg arg)
    , EGeq        <$> (guardMsg (name == "E_Geq"       ) >> fromObjectWith cfg arg)
    , EGt         <$> (guardMsg (name == "E_Gt"        ) >> fromObjectWith cfg arg)
    , EInt        <$> (guardMsg (name == "E_Int"       ) >> fromObjectWith cfg arg)
    , ELeq        <$> (guardMsg (name == "E_Leq"       ) >> fromObjectWith cfg arg)
    , ELt         <$> (guardMsg (name == "E_Lt"        ) >> fromObjectWith cfg arg)
    , EMatch      <$> (guardMsg (name == "E_Match"     ) >> fromObjectWith cfg arg)
    , EMult       <$> (guardMsg (name == "E_Mult"      ) >> fromObjectWith cfg arg)
    , EMultEq     <$> (guardMsg (name == "E_MultEq"    ) >> fromObjectWith cfg arg)
    , EMutez      <$> (guardMsg (name == "E_Mutez"     ) >> fromObjectWith cfg arg)
    , ENamePath   <$> (guardMsg (name == "E_NamePath"  ) >> fromObjectWith cfg arg)
    , ENat        <$> (guardMsg (name == "E_Nat"       ) >> fromObjectWith cfg arg)
    , ENeg        <$> (guardMsg (name == "E_Neg"       ) >> fromObjectWith cfg arg)
    , ENeq        <$> (guardMsg (name == "E_Neq"       ) >> fromObjectWith cfg arg)
    , ENot        <$> (guardMsg (name == "E_Not"       ) >> fromObjectWith cfg arg)
    , EObject     <$> (guardMsg (name == "E_Object"    ) >> fromObjectWith cfg arg)
    , EOr         <$> (guardMsg (name == "E_Or"        ) >> fromObjectWith cfg arg)
    , EPar        <$> (guardMsg (name == "E_Par"       ) >> fromObjectWith cfg arg)
    , EPostDecr   <$> (guardMsg (name == "E_PostDecr"  ) >> fromObjectWith cfg arg)
    , EPostIncr   <$> (guardMsg (name == "E_PostIncr"  ) >> fromObjectWith cfg arg)
    , EPreDecr    <$> (guardMsg (name == "E_PreDecr"   ) >> fromObjectWith cfg arg)
    , EPreIncr    <$> (guardMsg (name == "E_PreIncr"   ) >> fromObjectWith cfg arg)
    , EProj       <$> (guardMsg (name == "E_Proj"      ) >> fromObjectWith cfg arg)
    , ERem        <$> (guardMsg (name == "E_Rem"       ) >> fromObjectWith cfg arg)
    , ERemEq      <$> (guardMsg (name == "E_RemEq"     ) >> fromObjectWith cfg arg)
    , EString     <$> (guardMsg (name == "E_String"    ) >> fromObjectWith cfg arg)
    , ESub        <$> (guardMsg (name == "E_Sub"       ) >> fromObjectWith cfg arg)
    , ESubEq      <$> (guardMsg (name == "E_SubEq"     ) >> fromObjectWith cfg arg)
    , ETernary    <$> (guardMsg (name == "E_Ternary"   ) >> fromObjectWith cfg arg)
    , ETrue       <$> (guardMsg (name == "E_True"      ) >> fromObjectWith cfg arg)
    , ETyped      <$> (guardMsg (name == "E_Typed"     ) >> fromObjectWith cfg arg)
    , EUpdate     <$> (guardMsg (name == "E_Update"    ) >> fromObjectWith cfg arg)
    , EVar        <$> (guardMsg (name == "E_Var"       ) >> fromObjectWith cfg arg)
    , EVerbatim   <$> (guardMsg (name == "E_Verbatim"  ) >> fromObjectWith cfg arg)
    , EXor        <$> (guardMsg (name == "E_Xor"       ) >> fromObjectWith cfg arg)
    ]

instance (MessagePack a) => MessagePack (VariantKind a) where
  fromObjectWith cfg = withMsgVariant "VariantKind" \(name, arg) -> asumMsg
    [ Variant   <$> (guardMsg (name == "Variant"  ) >> fromObjectWith cfg arg)
    , Bracketed <$> (guardMsg (name == "Bracketed") >> fromObjectWith cfg arg)
    , Legacy    <$> (guardMsg (name == "Legacy"   ) >> fromObjectWith cfg arg)
    ]

instance (MessagePack a) => MessagePack (BracketedVariant a) where
  fromObjectWith _ = withMsgMap "BracketedVariant" \o -> do
    bvTuple <- o .: "tuple"
    pure BracketedVariant{..}

instance (MessagePack a) => MessagePack (BracketedVariantArgs a) where
  fromObjectWith _ = withMsgMap "BracketedVariantArgs" \o -> do
    bvaCtor <- o .: "ctor"
    bvaArgs <- o .:? "args"
    pure BracketedVariantArgs{..}

instance (MessagePack a) => MessagePack (LegacyVariant a) where
  fromObjectWith _ = withMsgMap "LegacyVariant" \o -> do
    lvTuple <- o .: "tuple"
    pure LegacyVariant{..}

instance (MessagePack a) => MessagePack (LegacyVariantArgs a) where
  fromObjectWith _ = withMsgMap "LegacyVariantArgs" \o -> do
    lvaCtor <- o .: "ctor"
    lvaArgs <- o .: "args"
    pure LegacyVariantArgs{..}

instance MessagePack ForStmt where
  fromObjectWith _ = withMsgMap "ForStmt" \o -> do
    fsRange <- o .: "range"
    fsForBody <- o .:? "for_body"
    pure ForStmt{..}

instance MessagePack ForOfStmt where
  fromObjectWith _ = withMsgMap "ForOfStmt" \o -> do
    fosRange <- o .: "range"
    fosForOfBody <- o .: "for_of_body"
    pure ForOfStmt{..}

instance MessagePack IfStmt where
  fromObjectWith _ = withMsgMap "IfStmt" \o -> do
    isTest <- o .: "test"
    isIfSo <- o .: "if_so"
    isIfNot <- o .:? "if_not"
    pure IfStmt{..}

instance MessagePack SwitchStmt where
  fromObjectWith _ = withMsgMap "SwitchStmt" \o -> do
    ssSubject <- o .: "subject"
    ssCases <- o .: "cases"
    pure SwitchStmt{..}

instance MessagePack WhileStmt where
  fromObjectWith _ = withMsgMap "WhileStmt" \o -> do
    wsInvariant <- o .: "invariant"
    wsWhileBody <- o .: "while_body"
    pure WhileStmt{..}

instance MessagePack FunDecl where
  fromObjectWith _ = withMsgMap "FunDecl" \o -> do
    fdFunName <- o .: "fun_name"
    fdGenerics <- o .:? "generics"
    fdParameters <- o .: "parameters"
    fdRhsType <- o .:? "rhs_type"
    fdFunBody <- o .: "fun_body"
    pure FunDecl{..}

instance MessagePack ImportDecl where
  fromObjectWith cfg = withMsgVariant "ImportDecl" \(name, arg) -> asumMsg
    [ ImportAlias <$> (guardMsg (name == "ImportAlias") >> fromObjectWith cfg arg)
    , ImportAllAs <$> (guardMsg (name == "ImportAllAs") >> fromObjectWith cfg arg)
    , ImportFrom  <$> (guardMsg (name == "ImportFrom" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack InterfaceDecl where
  fromObjectWith _ = withMsgMap "InterfaceDecl" \o -> do
    idIntfName <- o .: "intf_name"
    idIntfBody <- o .: "intf_body"
    pure InterfaceDecl{..}

instance MessagePack NamespaceDecl where
  fromObjectWith _ = withMsgMap "NamespaceDecl" \o -> do
    ndNamespaceName <- o .: "namespace_name"
    ndNamespaceType <- o .:? "namespace_type"
    ndNamespaceBody <- o .: "namespace_body"
    pure NamespaceDecl{..}

instance MessagePack TypeDecl where
  fromObjectWith _ = withMsgMap "TypeDecl" \o -> do
    tdName <- o .: "name"
    tdGenerics <- o .:? "generics"
    tdTypeExpr <- o .: "type_expr"
    pure TypeDecl{..}

instance MessagePack ValueDecl where
  fromObjectWith _ = withMsgMap "ValueDecl" \o -> do
    vdKind <- o .: "kind"
    vdBindings <- o .: "bindings"
    pure ValueDecl{..}

instance (MessagePack a) => MessagePack (BinOp a) where
  fromObjectWith _ = withMsgMap "BinOp" \o -> do
    boArg1 <- o .: "arg1"
    boOp <- o .: "op"
    boArg2 <- o .: "arg2"
    pure BinOp{..}

instance (MessagePack a) => MessagePack (UnOp a) where
  fromObjectWith _ = withMsgMap "UnOp" \o -> do
    uoOp <- o .: "op"
    uoArg <- o .: "arg"
    pure UnOp{..}

instance (MessagePack a) => MessagePack (App a) where
  fromObjectWith cfg = withMsgVariant "App" \(name, arg) -> asumMsg
    [ ZeroArg <$> (guardMsg (name == "ZeroArg") >> fromObjectWith cfg arg)
    , MultArg <$> (guardMsg (name == "MultArg") >> fromObjectWith cfg arg)
    ]

instance MessagePack CtorAppKind where
  fromObjectWith cfg = withMsgVariant "CtorAppKind" \(name, arg) -> asumMsg
    [ CtorStr  <$> (guardMsg (name == "CtorStr" ) >> fromObjectWith cfg arg)
    , CtorName <$> (guardMsg (name == "CtorName") >> fromObjectWith cfg arg)
    ]

instance (MessagePack a) => MessagePack (NamespacePath a) where
  fromObjectWith _ = withMsgMap "NamespacePath" \o -> do
    npNamespacePath <- o .: "namespace_path"
    npProperty <- o .: "property"
    pure NamespacePath{..}

instance (MessagePack a) => MessagePack (Property a) where
  fromObjectWith _ = withMsgMap "Property" \o -> do
    pPropertyId <- o .: "property_id"
    pPropertyRhs <- o .:? "property_rhs"
    pure Property{..}

instance MessagePack ArrowFunExpr where
  fromObjectWith _ = withMsgMap "ArrowFunExpr" \o -> do
    afeGenerics <- o .:? "generics"
    afeParameters <- o .: "parameters"
    afeRhsType <- o .:? "rhs_type"
    afeFunBody <- o .: "fun_body"
    pure ArrowFunExpr{..}

instance MessagePack CodeInj where
  fromObjectWith _ = withMsgMap "CodeInj" \o -> do
    ciLanguage <- o .: "language"
    ciCode <- o .: "code"
    pure CodeInj{..}

instance MessagePack ContractOfExpr where
  fromObjectWith _ = withMsgMap "ContractOfExpr" \o -> do
    coeNamespacePath <- o .: "namespace_path"
    pure ContractOfExpr{..}

instance MessagePack DoExpr where
  fromObjectWith _ = withMsgMap "DoExpr" \o -> do
    deStatements <- o .: "statements"
    pure DoExpr{..}

instance MessagePack FunctionExpr where
  fromObjectWith _ = withMsgMap "FunctionExpr" \o -> do
    feGenerics <- o .:? "generics"
    feParameters <- o .: "parameters"
    feRhsType <- o .:? "rhs_type"
    feFunBody <- o .: "fun_body"
    pure FunctionExpr{..}

instance MessagePack MatchExpr where
  fromObjectWith _ = withMsgMap "MatchExpr" \o -> do
    meSubject <- o .: "subject"
    meClauses <- o .: "clauses"
    pure MatchExpr{..}

instance MessagePack Projection where
  fromObjectWith _ = withMsgMap "Projection" \o -> do
    pObjectOrArray <- o .: "object_or_array"
    pPropertyPath <- o .: "property_path"
    pure Projection{..}

instance MessagePack Ternary where
  fromObjectWith _ = withMsgMap "Ternary" \o -> do
    tCondition <- o .: "condition"
    tTruthy <- o .: "truthy"
    tFalsy <- o .: "falsy"
    pure Ternary{..}

instance MessagePack TypeExpr where
  fromObjectWith cfg = withMsgVariant "TypeExpr" \(name, arg) -> asumMsg
    [ TApp         <$> (guardMsg (name == "T_App"        ) >> fromObjectWith cfg arg)
    , TAttr        <$> (guardMsg (name == "T_Attr"       ) >> fromObjectWith cfg arg)
    , TArray       <$> (guardMsg (name == "T_Array"      ) >> fromObjectWith cfg arg)
    , TFun         <$> (guardMsg (name == "T_Fun"        ) >> fromObjectWith cfg arg)
    , TInt         <$> (guardMsg (name == "T_Int"        ) >> fromObjectWith cfg arg)
    , TNamePath    <$> (guardMsg (name == "T_NamePath"   ) >> fromObjectWith cfg arg)
    , TObject      <$> (guardMsg (name == "T_Object"     ) >> fromObjectWith cfg arg)
    , TPar         <$> (guardMsg (name == "T_Par"        ) >> fromObjectWith cfg arg)
    , TParameterOf <$> (guardMsg (name == "T_ParameterOf") >> fromObjectWith cfg arg)
    , TString      <$> (guardMsg (name == "T_String"     ) >> fromObjectWith cfg arg)
    , TUnion       <$> (guardMsg (name == "T_Union"      ) >> fromObjectWith cfg arg)
    , TVar         <$> (guardMsg (name == "T_Var"        ) >> fromObjectWith cfg arg)
    , TVariant     <$> (guardMsg (name == "T_Variant"    ) >> fromObjectWith cfg arg)
    ]

instance MessagePack UpdateExpr where
  fromObjectWith _ = withMsgMap "UpdateExpr" \o -> do
    ueObject <- o .: "_object"
    ueUpdates <- o .: "updates"
    pure UpdateExpr{..}

instance MessagePack RangeFor where
  fromObjectWith _ = withMsgMap "RangeFor" \o -> do
    rfInitialiser <- o .:? "initialiser"
    rfCondition <- o .:? "condition"
    rfAfterthought <- o .:? "afterthought"
    pure RangeFor{..}

instance MessagePack RangeOf where
  fromObjectWith _ = withMsgMap "RangeOf" \o -> do
    roIndex <- o .: "index"
    roExpr <- o .: "expr"
    pure RangeOf{..}

instance MessagePack Cases where
  fromObjectWith cfg = withMsgVariant "Cases" \(name, arg) -> asumMsg
    [ AllCases <$> (guardMsg (name == "AllCases") >> fromObjectWith cfg arg)
    , Default  <$> (guardMsg (name == "Default" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack Pattern where
  fromObjectWith cfg = withMsgVariant "Pattern" \(name, arg) -> asumMsg
    [ PArray    <$> (guardMsg (name == "P_Array"   ) >> fromObjectWith cfg arg)
    , PAttr     <$> (guardMsg (name == "P_Attr"    ) >> fromObjectWith cfg arg)
    , PBytes    <$> (guardMsg (name == "P_Bytes"   ) >> fromObjectWith cfg arg)
    , PCtorApp  <$> (guardMsg (name == "P_CtorApp" ) >> fromObjectWith cfg arg)
    , PFalse    <$> (guardMsg (name == "P_False"   ) >> fromObjectWith cfg arg)
    , PInt      <$> (guardMsg (name == "P_Int"     ) >> fromObjectWith cfg arg)
    , PMutez    <$> (guardMsg (name == "P_Mutez"   ) >> fromObjectWith cfg arg)
    , PNamePath <$> (guardMsg (name == "P_NamePath") >> fromObjectWith cfg arg)
    , PNat      <$> (guardMsg (name == "P_Nat"     ) >> fromObjectWith cfg arg)
    , PObject   <$> (guardMsg (name == "P_Object"  ) >> fromObjectWith cfg arg)
    , PString   <$> (guardMsg (name == "P_String"  ) >> fromObjectWith cfg arg)
    , PTrue     <$> (guardMsg (name == "P_True"    ) >> fromObjectWith cfg arg)
    , PTyped    <$> (guardMsg (name == "P_Typed"   ) >> fromObjectWith cfg arg)
    , PVar      <$> (guardMsg (name == "P_Var"     ) >> fromObjectWith cfg arg)
    , PVerbatim <$> (guardMsg (name == "P_Verbatim") >> fromObjectWith cfg arg)
    ]

instance MessagePack ImportAlias where
  fromObjectWith _ = withMsgMap "ImportAlias" \o -> do
    iacAlias <- o .: "alias"
    iacNamespacePath <- o .: "namespace_path"
    pure ImportAliasC{..}

instance MessagePack ImportAllAs where
  fromObjectWith _ = withMsgMap "ImportAllAs" \o -> do
    iaacAlias <- o .: "alias"
    iaacFilePath <- o .: "file_path"
    pure ImportAllAsC{..}

instance MessagePack ImportFrom where
  fromObjectWith _ = withMsgMap "ImportFrom" \o -> do
    ifcImported <- o .: "imported"
    ifcFilePath <- o .: "file_path"
    pure ImportFromC{..}

instance MessagePack IntfEntry where
  fromObjectWith cfg = withMsgVariant "IntfEntry" \(name, arg) -> asumMsg
    [ IAttr  <$> (guardMsg (name == "I_Attr" ) >> fromObjectWith cfg arg)
    , IType  <$> (guardMsg (name == "I_Type" ) >> fromObjectWith cfg arg)
    , IConst <$> (guardMsg (name == "I_Const") >> fromObjectWith cfg arg)
    ]

instance MessagePack IntfExpr where
  fromObjectWith cfg = withMsgVariant "IntfExpr" \(name, arg) -> asumMsg
    [ IBody <$> (guardMsg (name == "I_Body") >> fromObjectWith cfg arg)
    , IPath <$> (guardMsg (name == "I_Path") >> fromObjectWith cfg arg)
    ]

instance MessagePack VarKind where
  fromObjectWith _ = withMsgVariant "VarKind" \(name, _) -> asumMsg
    [ Let   <$ guardMsg (name == "Let"  )
    , Const <$ guardMsg (name == "Const")
    ]

instance MessagePack ValBinding where
  fromObjectWith _ = withMsgMap "ValBinding" \o -> do
    vbPattern <- o .: "pattern"
    vbGenerics <- o .:? "generics"
    vbRhsType <- o .:? "rhs_type"
    vbRhsExpr <- o .: "rhs_expr"
    pure ValBinding{..}

instance MessagePack PropertyId where
  fromObjectWith cfg = withMsgVariant "PropertyId" \(name, arg) -> asumMsg
    [ FInt  <$> (guardMsg (name == "F_Int" ) >> fromObjectWith cfg arg)
    , FName <$> (guardMsg (name == "F_Name") >> fromObjectWith cfg arg)
    , FStr  <$> (guardMsg (name == "F_Str" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack ArrowFunParams where
  fromObjectWith cfg = withMsgVariant "ArrowFunParams" \(name, arg) -> asumMsg
    [ ParParams  <$> (guardMsg (name == "ParParams" ) >> fromObjectWith cfg arg)
    , NakedParam <$> (guardMsg (name == "NakedParam") >> fromObjectWith cfg arg)
    ]

instance MessagePack FunBody where
  fromObjectWith cfg = withMsgVariant "FunBody" \(name, arg) -> asumMsg
    [ StmtBody <$> (guardMsg (name == "StmtBody") >> fromObjectWith cfg arg)
    , ExprBody <$> (guardMsg (name == "ExprBody") >> fromObjectWith cfg arg)
    ]

instance MessagePack NamespaceSelection where
  fromObjectWith cfg = withMsgVariant "NamespaceSelection" \(name, arg) -> asumMsg
    [ MPath  <$> (guardMsg (name == "M_Path" ) >> fromObjectWith cfg arg)
    , MAlias <$> (guardMsg (name == "M_Alias") >> fromObjectWith cfg arg)
    ]

instance MessagePack MatchClauses where
  fromObjectWith cfg = withMsgVariant "MatchClauses" \(name, arg) -> asumMsg
    [ AllClauses    <$> (guardMsg (name == "AllClauses"   ) >> fromObjectWith cfg arg)
    , DefaultClause <$> (guardMsg (name == "DefaultClause") >> fromObjectWith cfg arg)
    ]

instance MessagePack Selection where
  fromObjectWith cfg = withMsgVariant "Selection" \(name, arg) -> asumMsg
    [ PropertyName <$> (guardMsg (name == "PropertyName") >> fromObjectWith cfg arg)
    , PropertyStr  <$> (guardMsg (name == "PropertyStr" ) >> fromObjectWith cfg arg)
    , Component    <$> (guardMsg (name == "Component"   ) >> fromObjectWith cfg arg)
    ]

instance MessagePack ParameterOfType where
  fromObjectWith _ = withMsgMap "ParameterOfType" \o -> do
    potNamespacePath <- o .: "namespace_path"
    pure ParameterOfType{..}

instance (MessagePack a) => MessagePack (Variant a) where
  fromObjectWith _ = withMsgMap "Variant" \o -> do
    vTuple <- o .: "tuple"
    pure VariantC{..}

instance MessagePack SwitchCase where
  fromObjectWith _ = withMsgMap "SwitchCase" \o -> do
    scExpr <- o .: "expr"
    scCaseBody <- o .:? "case_body"
    pure SwitchCase{..}

instance MessagePack SwitchDefault where
  fromObjectWith _ = withMsgMap "SwitchDefault" \o -> do
    sdDefaultBody <- o .:? "default_body"
    pure SwitchDefault{..}

instance MessagePack IntfType where
  fromObjectWith _ = withMsgMap "IntfType" \o -> do
    itTypeName <- o .: "type_name"
    itTypeRhs <- o .:? "type_rhs"
    pure IntfType{..}

instance MessagePack IntfConst where
  fromObjectWith _ = withMsgMap "IntfConst" \o -> do
    icConstName <- o .: "const_name"
    icConstType <- o .: "const_type"
    pure IntfConst{..}

instance MessagePack MatchClause where
  fromObjectWith _ = withMsgMap "MatchClause" \o -> do
    mcFilter <- o .: "filter"
    mcClauseExpr <- o .: "clause_expr"
    pure MatchClause{..}

instance MessagePack MatchDefault where
  fromObjectWith _ = withMsgMap "MatchDefault" \o -> do
    mdDefaultExpr <- o .: "default_expr"
    pure MatchDefault{..}

----------------
-- Conversion --
----------------

toAST :: CST -> LIGO Info
toAST CST{..} =
  let
    firstRange = point 1 1
    (lastRange, _) = unpackWrap cstEof
  in fastMake (firstRange `merged` lastRange) (AST.RawContract $ statementConv . unTuple1 <$> cstStatements)
  where
    statementsToSeq :: Par Statements -> LIGO Info
    statementsToSeq (unpackReg -> (r, Par' stmts)) =
      fastMake r (AST.Seq $ statementConv . unTuple1 <$> stmts)

    statementConv :: Statement -> LIGO Info
    statementConv = \case
      SAttr (Tuple1 stmt) -> statementConv stmt
      SBlock stmts -> statementsToSeq stmts
      SBreak (unpackWrap -> (r, _)) -> fastMake r AST.Break
      SContinue (unpackWrap -> (r, _)) -> fastMake r AST.Continue
      SDecl decl -> declConv decl
      SExport (unpackReg -> (r, Tuple1 decl)) ->
        fastMake r (AST.BExport $ declConv decl)
      SExpr expr -> exprConv expr
      SFor (unpackReg -> (r, ForStmt{..})) ->
        let
          Par' RangeFor{..} = rValue fsRange
          initMb = statementConv <$> rfInitialiser
          condMb = exprConv <$> rfCondition
          afterMb = maybe [] (fmap exprConv) rfAfterthought
          bodyMb = statementConv <$> fsForBody
        in fastMake r (AST.ForLoop initMb condMb afterMb bodyMb)
      SForOf (unpackReg -> (r, ForOfStmt{..})) ->
        let
          Par' RangeOf{..} = rValue fosRange
          index = makeWrappedLexeme AST.Name roIndex
          ofExpr = exprConv roExpr
          body = statementConv fosForOfBody
        in fastMake r (AST.ForOfLoop index ofExpr body)
      SIf (unpackReg -> (r, IfStmt{..})) ->
        let
          (condR, Par' condExpr) = unpackReg isTest
          cond = fastMake condR (AST.Paren $ exprConv condExpr)
          onTrue = statementConv $ unTuple1 isIfSo
          onFalse = statementConv . unTuple1 <$> isIfNot
        in fastMake r (AST.If cond onTrue onFalse)
      SReturn (unpackReg -> (r, Tuple1 exprMb)) ->
        fastMake r (AST.Return $ exprConv <$> exprMb)
      SSwitch (unpackReg -> (r, SwitchStmt{..})) ->
        let
          (switchR, Par' switchExpr) = unpackReg ssSubject
          switch = fastMake switchR (AST.Paren $ exprConv switchExpr)
          Par' cases = rValue ssCases

          (allCases, defMb) =
            case cases of
              AllCases inner -> inner
              Default defCase -> ([], Just defCase)

          mapSwitchCase :: Reg SwitchCase -> LIGO Info
          mapSwitchCase (unpackReg -> (r', SwitchCase{..})) =
            let
              caseExpr = exprConv scExpr
              caseBody = maybe [] (fmap (statementConv . unTuple1)) scCaseBody
            in fastMake r' (AST.CaseStm caseExpr caseBody)

          mapDefCase :: Reg SwitchDefault -> LIGO Info
          mapDefCase (unpackReg -> (r', SwitchDefault{..})) =
            let caseBody = maybe [] (fmap (statementConv . unTuple1)) sdDefaultBody
            in fastMake r' (AST.DefaultStm caseBody)

          casesBranches = mconcat $ catMaybes
            [ Just $ mapSwitchCase <$> allCases
            , (:[]) . mapDefCase <$> defMb
            ]

        in fastMake r (AST.SwitchStm switch casesBranches)
      SWhile (unpackReg -> (r, WhileStmt{..})) ->
        let
          (invR, Par' invExpr) = unpackReg wsInvariant
          invariant = fastMake invR (AST.Paren $ exprConv invExpr)
          body = statementConv wsWhileBody
        in fastMake r (AST.WhileLoop invariant body)

    declConv :: Declaration -> LIGO Info
    declConv = \case
      DFun (unpackReg -> (r, FunDecl{..})) ->
        let
          funName = makeWrappedLexeme AST.Name fdFunName
          typNames = maybe [] genericsConv fdGenerics
          params = funParamsConv fdParameters
          typAnnMb = typeAnnotationConv <$> fdRhsType
          body = statementsToSeq fdFunBody
        in fastMake r (AST.BFunction True funName typNames params typAnnMb body)
      DImport importDecl ->
        case importDecl of
          ImportAlias (unpackReg -> (r, ImportAliasC{..})) ->
            let
              alias = makeWrappedLexeme AST.ModuleName iacAlias
              moduleName = namespaceSelectionConv iacNamespacePath
            in fastMake r (AST.BImport (Just alias) [] moduleName)
          ImportAllAs (unpackReg -> (r, ImportAllAsC{..})) ->
            let
              alias = makeWrappedLexeme AST.ModuleName iaacAlias
              (r', _) = unpackWrap iaacFilePath
              moduleName = fastMake r' (AST.ModuleAccess [] (makeWrappedLexeme AST.ModuleName iaacFilePath))
            in fastMake r (AST.BImport (Just alias) [] moduleName)
          ImportFrom (unpackReg -> (r, ImportFromC{..})) ->
            let
              Par' (fmap (makeWrappedLexeme AST.Name) -> vars) = rValue ifcImported
              (r', _) = unpackWrap ifcFilePath
              moduleName = fastMake r' (AST.ModuleAccess [] (makeWrappedLexeme AST.ModuleName ifcFilePath))
            in fastMake r (AST.BImport Nothing vars moduleName)
      DInterface (unpackReg -> (r, InterfaceDecl{..})) ->
        let
          intfName = makeWrappedLexeme AST.ModuleName idIntfName
          intfBody = intfBodyConv idIntfBody
        in fastMake r (AST.BSignature intfName intfBody)
      DNamespace (unpackReg -> (r, NamespaceDecl{..})) ->
        let
          name = makeWrappedLexeme AST.ModuleName ndNamespaceName
          intfMb = interfaceConv <$> ndNamespaceType

          (bodyR, Par' stmts) = unpackReg ndNamespaceBody
          body = fastMake bodyR (AST.ModuleExpr $ statementConv . unTuple1 <$> stmts)
        in fastMake r (AST.BModuleDecl name intfMb body)
      DType (unpackReg -> (r, TypeDecl{..})) ->
        let
          typName = makeWrappedLexeme AST.TypeVariableName tdName

          typVarsMb = do
            vars <- tdGenerics
            let (r', _) = unpackReg vars
            pure $ fastMake r' (AST.QuotedTypeParams $ genericsConv vars)

          typExpr = typeExprConv tdTypeExpr
        in fastMake r (AST.BTypeDecl typName typVarsMb typExpr)
      DValue (unpackReg -> (r, ValueDecl{..})) ->
        case vdBindings of
          oneBinding :| [] -> mapValBinding (Just r) vdKind oneBinding
          _ -> fastMake r (AST.BDeclarationSeq $ mapValBinding Nothing vdKind <$> toList vdBindings)
      where
        mapValBinding :: Maybe Range -> VarKind -> Reg ValBinding -> LIGO Info
        mapValBinding rangeMb kind (unpackReg -> (r, ValBinding{..})) =
          let
            bindingRange = fromMaybe r rangeMb
            pat = patternConv vbPattern
            typVars = maybe [] genericsConv vbGenerics
            typAnnMb = typeAnnotationConv <$> vbRhsType
            expr = exprConv vbRhsExpr
          in fastMake bindingRange
            case kind of
              Let -> AST.BVar pat typVars typAnnMb (Just expr)
              Const -> AST.BConst True pat typVars typAnnMb (Just expr)

    interfaceConv :: Interface -> LIGO Info
    interfaceConv (unpackReg -> (_, Tuple1 intfExpr)) =
      case intfExpr of
        IBody body -> intfBodyConv body
        IPath path -> namespaceSelectionConv path

    intfBodyConv :: IntfBody -> LIGO Info
    intfBodyConv (unpackReg -> (r, Par' entries)) =
      fastMake r (AST.Signature $ intfEntryConv <$> entries)
      where
        intfEntryConv :: IntfEntry -> LIGO Info
        intfEntryConv = \case
          IAttr (Tuple1 entry) -> intfEntryConv entry
          IType (unpackReg -> (r', IntfType{..})) ->
            let
              tvName = makeWrappedLexeme AST.TypeVariableName itTypeName
              typMb = typeExprConv . unTuple1 <$> itTypeRhs
            in fastMake r' (AST.SType tvName typMb)
          IConst (unpackReg -> (r', IntfConst{..})) ->
            let
              name = makeWrappedLexeme AST.Name icConstName
              typ = typeAnnotationConv icConstType
            in fastMake r' (AST.SValue name typ)

    genericsConv :: Generics -> [LIGO Info]
    genericsConv (unpackReg -> (_, Par' vars)) = vars
      <&> makeWrappedLexeme AST.TypeVariableName

    funParamsConv :: FunParams -> [LIGO Info]
    funParamsConv (unpackReg -> (_, Par' patterns)) =
      patternConv <$> patterns

    typeAnnotationConv :: TypeAnnotation -> LIGO Info
    typeAnnotationConv (Tuple1 typExpr) = typeExprConv typExpr

    namespaceSelectionConv :: NamespaceSelection -> LIGO Info
    namespaceSelectionConv selection =
      let
        (r, namespacePath) = unpackReg $ toNamespacePath selection
      in fastMake r (namespacePathConv namespacePath)
      where
        toNamespacePath :: NamespaceSelection -> Reg (NamespacePath (LIGO Info))
        toNamespacePath = \case
          MPath regPath -> makeWrappedLexeme AST.ModuleName <<$>> regPath
          MAlias name -> Reg (wRegion name) (NamespacePath [] (makeWrappedLexeme AST.ModuleName name))

    namespacePathConv :: NamespacePath (LIGO Info) -> AST.ModuleAccess (LIGO Info)
    namespacePathConv NamespacePath{..} =
      let
        moduleParts = npNamespacePath
          <&> makeWrappedLexeme AST.ModuleName
      in AST.ModuleAccess moduleParts npProperty

    exprConv :: Expr -> LIGO Info
    exprConv = \case
      EAdd add -> makeBinOp add
      EAddEq addEq -> makeBinOp addEq
      EAnd andOp -> makeBinOp andOp
      EApp (unpackReg -> (r, (expr, rValue -> Par' args))) ->
        fastMake r (AST.Apply (exprConv expr) (exprConv <$> args))
      EArray (unpackReg -> (r, Par' lst)) ->
        let
          elts = exprConv . unTuple1 <$> lst
        in fastMake r (AST.List elts)
      EArrowFun (unpackReg -> (r, ArrowFunExpr{..})) ->
        let
          params = arrowFunParamsConv afeParameters
          typVars = maybe [] genericsConv afeGenerics
          typMb = typeAnnotationConv <$> afeRhsType
          body =
            case afeFunBody of
              StmtBody stmts -> statementsToSeq stmts
              ExprBody expr -> exprConv expr
        in fastMake r (AST.Lambda params typVars typMb body)
      EAssign (unpackReg -> (r, BinOp{..})) ->
        let
          lhs = exprConv boArg1
          op = makeWrappedLexeme AST.Op boOp
          rhs = exprConv boArg2
        in fastMake r (AST.AssignOp lhs op rhs)
      EAttr (Tuple1 expr) -> exprConv expr
      EBitAnd bitAnd -> makeBinOp bitAnd
      EBitAndEq bitAndEq -> makeBinOp bitAndEq
      EBitNeg bitNeg -> makeUnOp bitNeg
      EBitOr bitOr -> makeBinOp bitOr
      EBitOrEq bitOrEq -> makeBinOp bitOrEq
      EBitSl sl -> makeBinOp sl
      EBitSlEq slEq -> makeBinOp slEq
      EBitSr sr -> makeBinOp sr
      EBitSrEq srEq -> makeBinOp srEq
      EBitXor bitXor -> makeBinOp bitXor
      EBitXorEq bitXorEq -> makeBinOp bitXorEq
      EBytes (unpackWrap -> (r, Tuple1 bts)) -> makeConstantExpr r (AST.CBytes bts)
      ECodeInj (unpackReg -> (r, CodeInj{..})) ->
        let
          lang = makeWrappedLexeme AST.Attr ciLanguage
        in fastMake r (AST.CodeInj lang (exprConv ciCode))
      EContractOf (unpackReg -> (r, ContractOfExpr{..})) ->
        let
          Par' selection = rValue coeNamespacePath
        in fastMake r (AST.Contract $ namespaceSelectionConv selection)
      ECtorApp ctorApp ->
        let
          (r, ctor, args) = extractCtorAndArgs exprConv ctorApp
        in fastMake r (AST.Apply ctor (exprConv <$> args))
      EDiv divOp -> makeBinOp divOp
      EDivEq divEq -> makeBinOp divEq
      EDo (unpackReg -> (r, DoExpr{..})) ->
        let
          Par' stmts = rValue deStatements
        in fastMake r (AST.EDo $ statementConv . unTuple1 <$> stmts)
      EEqual equal -> makeBinOp equal
      EFalse (unpackWrap -> (r, _)) -> fastMake r AST.EFalse
      EFunction (unpackReg -> (r, FunctionExpr{..})) ->
        let
          params = arrowFunParamsConv feParameters
          typVars = maybe [] genericsConv feGenerics
          typMb = typeAnnotationConv <$> feRhsType
          body =
            case feFunBody of
              StmtBody stmts -> statementsToSeq stmts
              ExprBody expr -> exprConv expr
        in fastMake r (AST.Lambda params typVars typMb body)
      EGeq geq -> makeBinOp geq
      EGt gt -> makeBinOp gt
      EInt (unpackWrap -> (r, Tuple1 n)) -> makeConstantExpr r (AST.CInt n)
      ELeq leq -> makeBinOp leq
      ELt lt -> makeBinOp lt
      EMatch (unpackReg -> (r, MatchExpr{..})) ->
        let
          (subjR, Par' subject) = unpackReg meSubject
          subjectExpr = fastMake subjR (AST.Paren $ exprConv subject)
          Par' clauses = rValue meClauses

          (allClauses, defMb) =
            case clauses of
              AllClauses inner -> inner
              DefaultClause defClause -> ([], Just defClause)

          mapMatchClause :: Reg MatchClause -> LIGO Info
          mapMatchClause (unpackReg -> (r', MatchClause{..})) =
            let
              (filtR, Par' filt') = unpackReg mcFilter
              filt = fastMake filtR (AST.Paren $ patternConv filt')
              clauseExpr = exprConv mcClauseExpr
            in fastMake r' (AST.Alt filt clauseExpr)

          mapDefClause :: Reg MatchDefault -> LIGO Info
          mapDefClause (unpackReg -> (r', MatchDefault{..})) =
            let defaultExpr = exprConv mdDefaultExpr
            in fastMake r' (AST.Default defaultExpr)

          clauseBranches = mconcat $ catMaybes
            [ Just $ mapMatchClause <$> allClauses
            , (:[]) . mapDefClause <$> defMb
            ]

        in fastMake r (AST.Case subjectExpr clauseBranches)
      EMult mult -> makeBinOp mult
      EMultEq multEq -> makeBinOp multEq
      EMutez (unpackWrap -> (r, Tuple1 mutez)) -> makeConstantExpr r (AST.CTez mutez)
      ENamePath (unpackReg -> (r, path)) ->
        let
          moduleAccess = namespacePathConv $ exprConv <$> path
        in fastMake r moduleAccess
      ENat (unpackWrap -> (r, Tuple1 n)) -> makeConstantExpr r (AST.CNat n)
      ENeg neg -> makeUnOp neg
      ENeq neq -> makeBinOp neq
      ENot notOp -> makeUnOp notOp
      EObject (unpackReg -> (r, Par' properties)) ->
        fastMake r (AST.Record $ propertyExprConv <$> properties)
      EOr orOp -> makeBinOp orOp
      EPar (unpackReg -> (r, Par' expr)) ->
        fastMake r (AST.Paren $ exprConv expr)
      EPostDecr postDecr -> makeUnOp postDecr
      EPostIncr postIncr -> makeUnOp postIncr
      EPreDecr preDecr -> makeUnOp preDecr
      EPreIncr preIncr -> makeUnOp preIncr
      EProj (unpackReg -> (r, Projection{..})) ->
        let
          selectionConv :: Selection -> LIGO Info
          selectionConv = \case
            PropertyName (Tuple1 name) -> makeWrappedLexeme AST.Name name
            PropertyStr (unpackWrap . pInside . rValue -> (r', str)) ->
              makeConstantExpr r' (AST.CString $ escapeText str)
            Component (unpackWrap . pInside . rValue -> (r', Tuple1 n)) ->
              makeConstantExpr r' (AST.CInt n)

          fieldName = exprConv pObjectOrArray
          path = selectionConv <$> pPropertyPath
        in fastMake r (AST.QualifiedName fieldName path)
      ERem remOp -> makeBinOp remOp
      ERemEq remEq -> makeBinOp remEq
      EString (unpackWrap -> (r, str)) -> makeConstantExpr r (AST.CString $ escapeText str)
      ESub subOp -> makeBinOp subOp
      ESubEq subEq -> makeBinOp subEq
      ETernary (unpackReg -> (r, Ternary{..})) ->
        let
          cond = exprConv tCondition
          onTrue = exprConv tTruthy
          onFalse = exprConv tFalsy
        in fastMake r (AST.Ternary cond onTrue onFalse)
      ETrue (unpackWrap -> (r, _)) -> fastMake r AST.ETrue
      ETyped (unpackReg -> (r, (expr, typ))) ->
        fastMake r (AST.Annot (exprConv expr) (typeExprConv typ))
      EUpdate (unpackReg -> (r, Par' UpdateExpr{..})) ->
        let
          name = exprConv ueObject
          updates = propertyExprConv <$> ueUpdates
        in fastMake r (AST.RecordUpd name updates)
      EVar v -> makeWrappedLexeme AST.Name v
      EVerbatim verb -> makeWrappedLexeme AST.Verbatim verb
      EXor xorOp -> makeBinOp xorOp
      where
        makeBinOp :: SomeBinOp -> LIGO Info
        makeBinOp (unpackReg -> (r, BinOp{..})) =
          let
            op = makeWrappedLexeme AST.Op boOp
          in fastMake r (AST.BinOp (exprConv boArg1) op (exprConv boArg2))

        makeUnOp :: SomeUnOp -> LIGO Info
        makeUnOp (unpackReg -> (r, UnOp{..})) =
          let
            op = makeWrappedLexeme AST.Op uoOp
          in fastMake r (AST.UnOp op (exprConv uoArg))

        makeConstantExpr :: Range -> AST.Constant (LIGO Info) -> LIGO Info
        makeConstantExpr r c = fastMake r $ AST.Constant (fastMake r c)

        arrowFunParamsConv :: ArrowFunParams -> [LIGO Info]
        arrowFunParamsConv = \case
          ParParams (rValue -> Par' params) -> patternConv <$> params
          NakedParam param -> [patternConv param]

        propertyExprConv :: Reg (Property Expr) -> LIGO Info
        propertyExprConv (unpackReg -> (r, Property{..})) =
          let accessor = propertyIdConv makeConstantExpr pPropertyId in
            case pPropertyRhs of
              Nothing -> fastMake r (AST.Capture accessor)
              Just (Tuple1 rhs) -> fastMake r (AST.FieldAssignment [accessor] (exprConv rhs))

    propertyIdConv :: (Range -> AST.Constant (LIGO Info) -> LIGO Info) -> PropertyId -> LIGO Info
    propertyIdConv makeConstant = \case
      FInt (unpackWrap -> (rInt, Tuple1 n)) -> makeConstant rInt (AST.CInt n)
      FName name -> makeWrappedLexeme AST.Name name
      FStr (unpackWrap -> (rStr, str)) ->
        makeConstant rStr (AST.CString $ escapeText str)

    extractCtorAndArgs :: (a -> LIGO Info) -> VariantKind a -> (Range, LIGO Info, [a])
    extractCtorAndArgs ctorConv = \case
      Variant (unpackReg -> (r, VariantC{..})) ->
        case unTuple1 vTuple of
          ZeroArg ctor -> (r, ctorAppKindConv ctor, [])
          MultArg (ctor, rValue -> Par' args) -> (r, ctorAppKindConv ctor, toList args)
      Bracketed (unpackReg -> (r, BracketedVariant{..})) ->
        let
          Par' BracketedVariantArgs{..} = rValue bvTuple
        in (r, ctorConv bvaCtor, maybe [] unTuple1 bvaArgs)
      Legacy (unpackReg -> (r, LegacyVariant{..})) ->
        let
          Par' LegacyVariantArgs{..} = rValue lvTuple
        in (r, makeWrappedLexeme AST.Name lvaCtor, unTuple1 <$> lvaArgs)
      where
        ctorAppKindConv :: CtorAppKind -> LIGO Info
        ctorAppKindConv = \case
          CtorStr str -> makeWrappedLexeme (AST.CString . escapeText) str
          CtorName name -> makeWrappedLexeme AST.Name name

    patternConv :: Pattern -> LIGO Info
    patternConv = \case
      PArray (unpackReg -> (r, Par' lst)) ->
        let
          elts = patternConv . unTuple1 <$> lst
        in fastMake r (AST.IsList elts)
      PAttr (Tuple1 pat) -> patternConv pat
      PBytes (unpackWrap -> (r, Tuple1 bts)) -> makeConstantPattern r (AST.CBytes bts)
      PCtorApp ctorApp ->
        let
          (r, ctor, args) = extractCtorAndArgs patternConv ctorApp
        in fastMake r (AST.IsConstr ctor (patternConv <$> args))
      PFalse (unpackWrap -> (r, _)) -> fastMake r AST.IsFalse
      PInt (unpackWrap -> (r, Tuple1 n)) -> makeConstantPattern r (AST.CInt n)
      PMutez (unpackWrap -> (r, Tuple1 mutez)) -> makeConstantPattern r (AST.CTez mutez)
      PNamePath (unpackReg -> (r, path)) ->
        fastMake r (namespacePathConv $ patternConv <$> path)
      PNat (unpackWrap -> (r, Tuple1 n)) -> makeConstantPattern r (AST.CNat n)
      PObject (unpackReg -> (r, Par' properties)) ->
        fastMake r (AST.IsRecord $ propertyPatternConv <$> properties)
      PString (unpackWrap -> (r, str)) -> makeConstantPattern r (AST.CString $ escapeText str)
      PTrue (unpackWrap -> (r, _)) -> fastMake r AST.IsTrue
      PTyped (unpackReg -> (r, (pat, typ))) ->
        fastMake r (AST.IsAnnot (patternConv pat) (typeAnnotationConv typ))
      PVar var@(unpackWrap -> (r, v))
        | v == "_" -> fastMake r AST.IsWildcard
        | otherwise ->
          let nameDecl = makeWrappedLexeme AST.NameDecl var in
          fastMake r (AST.IsVar nameDecl)
      PVerbatim verb -> makeWrappedLexeme AST.Verbatim verb
      where
        makeConstantPattern :: Range -> AST.Constant (LIGO Info) -> LIGO Info
        makeConstantPattern r c = fastMake r $ AST.IsConstant (fastMake r c)

        propertyPatternConv :: Reg (Property Pattern) -> LIGO Info
        propertyPatternConv (unpackReg -> (r, Property{..})) =
          let accessor = propertyIdConv makeConstantPattern pPropertyId in
            case pPropertyRhs of
              Nothing -> fastMake r (AST.IsRecordCapture accessor)
              Just (Tuple1 rhs) -> fastMake r (AST.IsRecordField accessor (patternConv rhs))

    typeExprConv :: TypeExpr -> LIGO Info
    typeExprConv = \case
      TApp (unpackReg -> (r, (typ, (rValue -> Par' args)))) ->
        let
          f = typeExprConv typ
          xs = typeExprConv <$> args
        in fastMake r (AST.TApply f xs)
      TAttr (Tuple1 typ) -> typeExprConv typ
      TArray (unpackReg -> (r, Par' lst)) ->
        let
          elts = typeExprConv <$> lst
        in fastMake r (AST.TProduct elts)
      TFun (unpackReg -> (r, (typeParams, codomain))) ->
        let
          (paramsR, Par' params) = unpackReg typeParams
          domain = fastMake paramsR (AST.TProduct $ patternConv . PTyped <$> params)
        in fastMake r (AST.TArrow domain (typeExprConv codomain))
      TInt n@(unpackWrap -> (r, _)) ->
        fastMake r (AST.TInt $ makeWrappedLexeme AST.CInt (unTuple1 <$> n))
      TNamePath (unpackReg -> (r, path)) ->
        fastMake r (namespacePathConv $ typeExprConv <$> path)
      TObject (unpackReg -> (r, Par' properties)) ->
        let
          propertyConv :: Reg (Property TypeExpr) -> LIGO Info
          propertyConv (unpackReg -> (r', Property{..})) =
            let
              field =
                case pPropertyId of
                  FInt n -> typeExprConv (TInt n)
                  FName name -> makeWrappedLexeme AST.Name name
                  FStr str -> typeExprConv (TString str)
              rhsMb = typeExprConv . unTuple1 <$> pPropertyRhs
            in fastMake r' (AST.TField field rhsMb)
        in fastMake r (AST.TRecord def $ propertyConv <$> properties)
      TPar (unpackReg -> (r, Par' typ)) -> fastMake r (AST.TParen $ typeExprConv typ)
      TParameterOf (unpackReg -> (r, ParameterOfType{..})) ->
        fastMake r (AST.TParameter $ namespaceSelectionConv potNamespacePath)
      TString str@(unpackWrap -> (r, _)) ->
        fastMake r (AST.TString $ makeWrappedLexeme AST.CString str)
      TUnion (unpackReg -> (r, objects)) ->
        fastMake r (AST.TSum def $ typeExprConv . TObject <$> objects)
      TVar var@(unpackWrap -> (r, v))
        | v == "_" -> fastMake r AST.TWildcard
        | otherwise -> makeWrappedLexeme AST.TypeName var
      TVariant (unpackReg -> (r, variants)) ->
        let
          variantConv :: VariantKind TypeExpr -> LIGO Info
          variantConv variantKind =
            let
              (r', ctor, args) = extractCtorAndArgs typeExprConv variantKind
            in fastMake r' (AST.Variant ctor (typeExprConv <$> args))
        in fastMake r (AST.TSum def $ variantConv <$> variants)
