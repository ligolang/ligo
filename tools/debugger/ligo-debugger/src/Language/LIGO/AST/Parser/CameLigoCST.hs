module Language.LIGO.AST.Parser.CameLigoCST
  ( CST
  , toAST
  ) where

import Control.MessagePack
  (asumMsg, guardMsg, withMsgArray, withMsgMap, withMsgVariant, (.:!), (.:), (.:?))
import Control.Monad.Validate (refute, runValidate)
import Data.Default (def)
import Data.List.NonEmpty ((<|))
import Data.MessagePack (MessagePack, Object (ObjectNil))
import Data.MessagePack.Types (fromObjectWith)
import Data.Vector qualified as V

import Duplo (Comonad (extract), fastMake)

import Language.LIGO.AST.Parser.Common
import Language.LIGO.AST.Skeleton (Info, LIGO)
import Language.LIGO.AST.Skeleton qualified as AST
import Language.LIGO.Range

-----------
-- Types --
-----------

-- All these types are mostly mappings
-- for the corresponding types from @CST.ml@.

-- | A CST of @CameLIGO@ contract file.
data CST = CST
  { cstDecl :: [Declaration]
    -- ^ Top-level declarations.
  , cstEof :: WrappedLexeme
    -- ^ Eof lexeme.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Top-level declaration.
data Declaration
  = DAttr (Reg (Tuple1 Declaration))
    -- ^ Declaration with attribute.
  | DLet (Reg LetDecl)
    -- ^ Let declaration.
  | DModule (Reg ModuleDecl)
    -- ^ Module declaration.
  | DInclude (Reg ModuleInclude)
    -- ^ Module include declaration.
  | DSignature (Reg SignatureDecl)
    -- ^ Signature declaration.
  | DType (Reg TypeDecl)
    -- ^ Type declaration.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Let declaration.
data LetDecl = LetDecl
  { ldIsRec :: Bool
    -- ^ Is it recursive?
  , ldLetBinding :: LetBinding
    -- ^ Declaration's binding.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Let binding.
data LetBinding = LetBinding
  { lbBinders :: NonEmpty Pattern
    -- ^ Pattern binders.
  , lbTypeParams :: Maybe (Par TypeParams)
    -- ^ Possible type parameters.
  , lbRhsType :: Maybe TypeAnnotation
    -- ^ The type of binding.
  , lbLetRhs :: Expr
    -- ^ Binding's right-hand side.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A pattern for attribute.
type AttrPattern = Tuple1 Pattern

-- | A pattern for list consing.
type ConsPattern = (Pattern, Pattern)

-- | All patterns.
data Pattern
  = PApp (Reg (Pattern, Maybe Pattern))
    -- ^ Application pattern.
  | PAttr AttrPattern
    -- ^ Pattern with attribute.
  | PBytes WrappedTupleLexeme
    -- ^ Bytes pattern.
  | PCons (Reg ConsPattern)
    -- ^ List consing pattern.
  | PCtor WrappedLexeme
    -- ^ Constructor pattern.
  | PFalse WrappedLexeme
    -- ^ @False@ value pattern.
  | PInt WrappedTupleLexeme
    -- ^ Integer pattern.
  | PList (Par [Pattern])
    -- ^ List pattern.
  | PModPath (Reg (ModulePath Pattern))
    -- ^ Module path pattern.
  | PMutez WrappedTupleLexeme
  | PTez WrappedTupleLexeme
  | PNat WrappedTupleLexeme
    -- ^ Natural pattern.
  | PPar (Par Pattern)
    -- ^ Pattern in parenthesis.
  | PRecord (Record (Field WrappedLexeme Pattern))
    -- ^ Record pattern.
  | PString WrappedLexeme
    -- ^ String pattern.
  | PTrue WrappedLexeme
    -- ^ @True@ value pattern.
  | PTuple (Reg (Tuple Pattern))
    -- ^ Tuple pattern.
  | PTyped (Reg (Pattern, TypeAnnotation))
    -- ^ Pattern with type.
  | PUnit (Reg ())
    -- ^ Unit pattern.
  | PVar WrappedLexeme
    -- ^ Variable pattern.
  | PVerbatim WrappedLexeme
    -- ^ Verbatim string pattern.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A value in parenthesis, brackets, braces, etc.
newtype Par' a = Par'
  { pInside :: a
    -- ^ A value inside.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Like @Par'@ but with assigned region.
type Par a = Reg (Par' a)

-- | A convenient synonym for tuples.
type Tuple a = NonEmpty a

-- | A convenient synonym for records.
type Record a = Par [a]

-- | A record field.
data Field lhs rhs
  = Punned (Reg (Punned lhs))
    -- ^ Record field without right-hand side.
  | Complete (Reg (FullField lhs rhs))
    -- ^ Record field with assignment.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Punned record field.
newtype Punned lhs = Pnd
  { pPun :: lhs
    -- ^ Field's name.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Full record field.
data FullField lhs rhs = FullField
  { ffFieldLhs :: lhs
    -- ^ Field's name.
  , ffFieldRhs :: rhs
    -- ^ Field's value.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A module path with accessor.
data ModulePath a = ModulePath
  { mpModulePath :: [WrappedLexeme]
    -- ^ A path to field.
  , mpField :: a
    -- ^ An accessor.
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

-- | A type alias for type parameters.
type TypeParams = Tuple1 (NonEmpty WrappedLexeme)

-- | A type alias for type annotation.
type TypeAnnotation = Tuple1 TypeExpr

-- | A type alias for some binary operation.
type SomeBinOp = Reg (BinOp WrappedLexeme)

-- | A type alias for some unary operation.
type SomeUnOp = Reg (UnOp WrappedLexeme)

-- | An expression with attribute.
type AttrExpr = Tuple1 Expr

-- | A right-hand side of @contract_of@.
type ContractOf = Reg (NonEmpty WrappedLexeme)

-- | All expressions.
data Expr
  = EAdd SomeBinOp
    -- ^ Add expression.
  | EAnd SomeBinOp
    -- ^ And expression
  | EApp (Reg (Expr, NonEmpty Expr))
    -- ^ Expression application.
  | EAssign (Reg Assign)
    -- ^ Assignment expression.
  | EAttr AttrExpr
    -- ^ Expression with attribute.
  | EBytes WrappedTupleLexeme
    -- ^ Bytes expression.
  | ECat SomeBinOp
    -- ^ Concatenation expression.
  | ECodeInj (Reg CodeInj)
    -- ^ Code injection expression.
  | ECond (Reg CondExpr)
    -- ^ Conditional expression.
  | ECons SomeBinOp
    -- ^ List consing expression.
  | EContractOf ContractOf
    -- ^ @contract_of@ expression.
  | ECtor WrappedLexeme
    -- ^ Constructor expression.
  | EDiv SomeBinOp
    -- ^ Division expression.
  | EEqual SomeBinOp
    -- ^ Equality check expression.
  | EFalse WrappedLexeme
    -- ^ @False@ value expression.
  | EFor (Reg ForLoop)
    -- ^ @for@ loop expression.
  | EForIn (Reg ForInLoop)
    -- ^ @for .. in@ loop expression.
  | EFun (Reg FunExpr)
    -- ^ Lambda expression.
  | EGeq SomeBinOp
    -- ^ @>=@ expression
  | EGt SomeBinOp
    -- ^ @>@ expression.
  | EInt WrappedTupleLexeme
    -- ^ Integer expression.
  | ELand SomeBinOp
    -- ^ Bitwise and expression.
  | ELeq SomeBinOp
    -- ^ @<=@ expression.
  | ELetIn (Reg LetIn)
    -- ^ Let-in expression.
  | ELetMutIn (Reg LetMutIn)
    -- ^ Let-mut-in expression.
  | EList (Par [Expr])
    -- ^ List expression.
  | ELor SomeBinOp
    -- ^ Bitwise or expression.
  | ELsl SomeBinOp
    -- ^ Bitwise shift-left expression.
  | ELsr SomeBinOp
    -- ^ Bitwise shift-right expression.
  | ELt SomeBinOp
    -- ^ @<@ expression.
  | ELxor SomeBinOp
    -- ^ Bitwise xor expression.
  | EMatch (Reg MatchExpr)
    -- ^ Match expression.
  | EMod SomeBinOp
    -- ^ Modulo expression.
  | EModIn (Reg ModuleIn)
    -- ^ Local module declaration expression.
  | EModPath (Reg (ModulePath Expr))
    -- ^ Module path expression.
  | EMult SomeBinOp
    -- ^ Multiplication expression.
  | EMutez WrappedTupleLexeme
  | ETez WrappedTupleLexeme
  | ENat WrappedTupleLexeme
    -- ^ Natural expression.
  | ENeg SomeUnOp
    -- ^ Negation expression.
  | ENeq SomeBinOp
    -- ^ Not-equality check expression.
  | ENot SomeUnOp
    -- ^ Logical negation expression.
  | EOr SomeBinOp
    -- ^ Logical or expression.
  | EPar (Par Expr)
    -- ^ Expression in parenthesis.
  | EProj (Reg Projection)
    -- ^ Projection expression.
  | ERecord (Record (Field WrappedLexeme Expr))
    -- ^ Record expression.
  | ERevApp SomeBinOp
    -- ^ Reverse application expression.
  | ESeq (Reg SequenceExpr)
    -- ^ A sequence of expressions.
  | EString WrappedLexeme
    -- ^ String expression.
  | ESub SomeBinOp
    -- ^ Subtraction expression.
  | ETrue WrappedLexeme
    -- ^ @True@ value expression.
  | ETuple (Reg (Tuple Expr))
    -- ^ Tuple expression.
  | ETyped (Par TypedExpr)
    -- ^ Expression with type.
  | ETypeIn (Reg TypeIn)
    -- ^ Local type declaration expression.
  | EUnit (Reg ())
    -- ^ Unit expression.
  | EUpdate (Par UpdateExpr)
    -- ^ Record update expression.
  | EVar WrappedLexeme
    -- ^ Variable expression.
  | EVerbatim WrappedLexeme
    -- ^ Verbatim string expression.
  | EWhile (Reg WhileLoop)
    -- ^ @while@ loop expression.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A binary operation.
data BinOp a = BinOp
  { boArg1 :: Expr
    -- ^ Left-hand side.
  , boOp :: a
    -- ^ Operator.
  , boArg2 :: Expr
    -- ^ Right-hand side.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | An unary operation.
data UnOp a = UnOp
  { uoOp :: a
    -- ^ Operator.
  , uoArg :: Expr
    -- ^ Operand.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | An assignment expression.
data Assign = Assign
  { aBinder :: WrappedLexeme
    -- ^ Left-hand side binder.
  , aAss :: WrappedLexeme
    -- ^ Assignment operator.
  , aExpr :: Expr
    -- ^ Right-hand side expression.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A code injection expression.
data CodeInj = CodeInj
  { ciLanguage :: Wrap (Reg Text)
    -- ^ A language of injection.
  , ciCode :: Expr
    -- ^ A code to inject.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A conditional expression.
data CondExpr = CondExpr
  { ceTest :: Expr
    -- ^ A condition to test.
  , ceIfSo :: Expr
    -- ^ @True@ branch of @if@ expression.
  , ceIfNot :: Maybe (Tuple1 Expr)
    -- ^ @False@ branch of @if@ expression.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A @for@ loop expression.
data ForLoop = ForLoop
  { flIndex :: WrappedLexeme
    -- ^ A name of index.
  , flEqual :: WrappedLexeme
    -- ^ Equality sign.
  , flBound1 :: Expr
    -- ^ Start value.
  , flDirection :: Direction
    -- ^ Direction of loop.
  , flBound2 :: Expr
    -- ^ End value.
  , flBody :: Reg LoopBody
    -- ^ Loop's body.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A @for .. in@ loop expression.
data ForInLoop = ForInLoop
  { filPattern :: Pattern
    -- ^ Pattern to loop.
  , filCollection :: Expr
    -- ^ Collection with values.
  , filBody :: Reg LoopBody
    -- ^ Loop's body.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Lambda expression.
data FunExpr = FunExpr
  { feTypeParams :: Maybe (Par TypeParams)
    -- ^ Possible type parameters.
  , feBinders :: NonEmpty Pattern
    -- ^ Value binders.
  , feRhsType :: Maybe TypeAnnotation
    -- ^ Lambda's return type.
  , feBody :: Expr
    -- ^ Lambda's body.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Let-in expression.
data LetIn = LetIn
  { liKwdLet :: WrappedLexeme -- needed for getting a start location for binding
    -- ^ @let@ keyword.
  , liIsRec :: Bool -- if "kwd_rec" is absent then this field is True
    -- ^ Is the definition a recursive one.
  , liBinding :: Reg LetBinding
    -- ^ Binding value.
  , liBody :: Expr
    -- ^ Expression after @in@ keyword.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Let-mut-in expression.
data LetMutIn = LetMutIn
  { lmiKwdLet :: WrappedLexeme -- same as for @LetIn@
    -- ^ @let@ keyword.
  , lmiBinding :: Reg LetBinding
    -- ^ Binding value.
  , lmiBody :: Expr
    -- ^ Expression after @in@ keyword.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A match expression.
data MatchExpr = MatchExpr
  { meSubject :: Expr
    -- ^ Scrutinee.
  , meClauses :: Reg (NonEmpty (Reg MatchClause))
    -- ^ Cases.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Module-in expression.
data ModuleIn = ModuleIn
  { miModDecl :: Reg ModuleDecl
    -- ^ Declaration of the module.
  , miBody :: Expr
    -- ^ Expression after @in@ keyword.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Tuple's or record's projection expression.
data Projection = Projection
  { pRecordOrTuple :: Expr
    -- ^ Projected expression.
  , pFieldPath :: NonEmpty Selection
    -- ^ Projection selection.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | An expression with type.
type TypedExpr = (Expr, TypeAnnotation)

-- | Type-in expression.
data TypeIn = TypeIn
  { tiTypeDecl :: Reg TypeDecl
    -- ^ Declaration of the type.
  , tiBody :: Expr
    -- ^ Expression after @in@ keyword.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Record update expression.
data UpdateExpr = UpdateExpr
  { ueRecord :: Expr
    -- ^ A record to update.
  , ueUpdates :: NonEmpty (Field Path Expr)
    -- ^ A list of field updates.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A sequence of expressions.
newtype SequenceExpr = SequenceExpr
  { seElements :: [Expr]
    -- ^ Expressions in the sequence.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | @while@ loop expression.
data WhileLoop = WhileLoop
  { wlCond :: Expr
    -- ^ Condition of the loop.
  , wlBody :: Reg LoopBody
    -- ^ Loop's body.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A direction in @for@ loop.
data Direction
  = Upto WrappedLexeme
    -- ^ @upto@ keyword.
  | Downto WrappedLexeme
    -- ^ @downto@ keyword.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Loop's body.
newtype LoopBody = LoopBody
  { lbSeqExpr :: [Expr]
    -- ^ Expressions in the loop.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A match clause.
data MatchClause = MatchClause
  { mcPattern :: Pattern
    -- ^ A left-hand side pattern.
  , mcRhs :: Expr
    -- ^ A right-hand side expression.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A type of selection.
data Selection
  = FieldName WrappedLexeme
    -- ^ Field name.
  | Component WrappedTupleLexeme
    -- ^ An index of the tuple's component.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A path to record field.
data Path
  = Name WrappedLexeme
    -- ^ Standalone name.
  | Path (Reg Projection)
    -- ^ A projection.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Module declaration.
data ModuleDecl = ModuleDecl
  { mdName :: WrappedLexeme
    -- ^ Module's name.
  , mdAnnotation :: Maybe (Tuple1 SignatureExpr)
    -- ^ Module's signature.
  , mdModuleExpr :: ModuleExpr
    -- ^ Module's right-hand side.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Module expression.
data ModuleExpr
  = MBody (Reg ModuleBody)
    -- ^ Module's body
  | MPath (Reg (ModulePath WrappedLexeme))
    -- ^ A path to module.
  | MVar WrappedLexeme
    -- ^ A standalone module's name.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Module's body
newtype ModuleBody = ModuleBody
  { mbDeclarations :: [Declaration]
    -- ^ A list of module's declarations.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A type declaration.
data TypeDecl = TypeDecl
  { tdParams :: Maybe TypeVars
    -- ^ Type parameters.
  , tdName :: WrappedLexeme
    -- ^ Type name.
  , tdTypeExpr :: TypeExpr
    -- ^ Type definition.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Type variables.
data TypeVars
  = TVSingle TypeVar
    -- ^ A standalone type variable.
  | TVTuple (Par (Tuple TypeVar))
    -- ^ Tupled variables.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Type variable.
type TypeVar = Reg (Tuple1 WrappedLexeme)

-- | A type with attribute.
type TypeAttr = Tuple1 TypeExpr

-- | A tuple's type.
type Cartesian = (TypeExpr, NonEmpty TypeExpr)

-- | For all type.
type ForAll = (NonEmpty TypeVar, TypeExpr)

-- | An arrow type.
type FunType = (TypeExpr, TypeExpr)

-- | Module path inside @parameter_of@.
type ParameterOf = NonEmpty WrappedLexeme

-- | All type expressions.
data TypeExpr
  = TApp (Reg (TypeExpr, TypeCtorArg))
    -- ^ Type application.
  | TArg TypeVar
    -- ^ Quoted type variable.
  | TAttr TypeAttr
    -- ^ Type with attribute.
  | TCart (Reg Cartesian)
    -- ^ Tuple type.
  | TForAll (Reg ForAll)
    -- ^ For all type.
  | TFun (Reg FunType)
    -- ^ Arrow type.
  | TInt WrappedTupleLexeme
    -- ^ Integer singleton.
  | TModPath (Reg (ModulePath TypeExpr))
    -- ^ Module path type.
  | TPar (Par TypeExpr)
    -- ^ Type in parenthesis.
  | TParameterOf (Reg ParameterOf)
    -- ^ @parameter_of@ type.
  | TRecord (Record (Reg FieldDecl))
    -- ^ Record type.
  | TString WrappedLexeme
    -- ^ String singleton.
  | TVar WrappedLexeme
    -- ^ Type variable.
  | TSum (Reg SumType)
    -- ^ Variant type.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Type application arguments.
data TypeCtorArg
  = TCSingle TypeExpr
    -- ^ Standalone argument.
  | TCTuple (Par (Tuple TypeExpr))
    -- ^ Tupled arguments.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Record field declaration.
data FieldDecl = FieldDecl
  { fdFieldName :: WrappedLexeme
    -- ^ A name of field.
  , fdFieldType :: Maybe TypeAnnotation
    -- ^ Field's type annotation.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Variant type.
newtype SumType = SumType
  { vtVariants :: NonEmpty (Reg Variant)
    -- ^ Variant cases.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | One variant case.
data Variant = Variant
  { vCtor :: WrappedLexeme
    -- ^ Constructor name.
  , vCtorArgs :: Maybe (Tuple1 TypeExpr)
    -- ^ Constructor arguments.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Signature's right-hand side.
data SignatureExpr
  = SSig (Reg SignatureBody)
    -- ^ Signature's body.
  | SPath (Reg (ModulePath WrappedLexeme))
    -- ^ A path to signature.
  | SVar WrappedLexeme
    -- ^ A standalone signature's name.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Signature's body.
newtype SignatureBody = SignatureBody
  { sbSigItems :: [SigItem]
    -- ^ A list of signature's items.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Signature item with attribute.
type SigAttr = Tuple1 SigItem

-- | Signature include.
type SigInclude = Tuple1 SignatureExpr

-- | A type definition in signature.
data SigType = SigType
  { stTypeVars :: Maybe TypeVars
    -- ^ Type variables.
  , stTypeName :: WrappedLexeme
    -- ^ Type's name.
  , stTypeRhs :: Maybe (Tuple1 TypeExpr)
    -- ^ Possible type definition.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A value definition in signature.
data SigValue = SigValue
  { svVar :: WrappedLexeme
    -- ^ Value's name.
  , svValType :: TypeExpr
    -- ^ Value's type.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A signature item.
data SigItem
  = SAttr (Reg SigAttr)
    -- ^ Signature item with attribute.
  | SInclude (Reg SigInclude)
    -- ^ Include signature item.
  | SType (Reg SigType)
    -- ^ Type definition signature item.
  | SValue (Reg SigValue)
    -- ^ Value definition signature item.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Include module declaration.
newtype ModuleInclude = ModuleInclude
  { miModuleExpr :: ModuleExpr
    -- ^ Module to include.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Signature declaration.
data SignatureDecl = SignatureDecl
  { sdName :: WrappedLexeme
    -- ^ Signature's name.
  , sdSignatureExpr :: SignatureExpr
    -- ^ Signature's right-hand side.
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-----------------
-- MessagePack --
-----------------

instance MessagePack CST where
  fromObjectWith _ = withMsgMap "CST" \o -> do
    cstDecl <- o .: "decl"
    cstEof <- o .: "eof"
    pure CST{..}

instance MessagePack Declaration where
  fromObjectWith cfg = withMsgVariant "Declaration" \(name, arg) -> asumMsg
    [ DAttr      <$> (guardMsg (name == "D_Attr"     ) >> fromObjectWith cfg arg)
    , DLet       <$> (guardMsg (name == "D_Let"      ) >> fromObjectWith cfg arg)
    , DModule    <$> (guardMsg (name == "D_Module"   ) >> fromObjectWith cfg arg)
    , DInclude   <$> (guardMsg (name == "D_Include"  ) >> fromObjectWith cfg arg)
    , DSignature <$> (guardMsg (name == "D_Signature") >> fromObjectWith cfg arg)
    , DType      <$> (guardMsg (name == "D_Type"     ) >> fromObjectWith cfg arg)
    ]

instance MessagePack ModuleInclude where
  fromObjectWith _ = withMsgMap "ModuleInclude" \o -> do
    miModuleExpr <- o .: "module_expr"
    pure ModuleInclude{..}

instance MessagePack SignatureDecl where
  fromObjectWith _ = withMsgMap "SignatureDecl" \o -> do
    sdName <- o .: "name"
    sdSignatureExpr <- o .: "signature_expr"
    pure SignatureDecl{..}

instance MessagePack LetDecl where
  fromObjectWith cfg = withMsgArray "LetDecl" \arr ->
    case V.toList arr of
      [ObjectNil, arg] -> do
        let ldIsRec = False
        ldLetBinding <- fromObjectWith cfg arg
        pure LetDecl{..}
      [arg] -> do
        let ldIsRec = True
        ldLetBinding <- fromObjectWith cfg arg
        pure LetDecl{..}
      _ -> refute "Expected let_decl"

instance MessagePack LetBinding where
  fromObjectWith _ = withMsgMap "LetBinding" \o -> do
    lbBinders <- o .: "binders"
    lbTypeParams <- o .:? "type_params"
    lbRhsType <- o .:? "rhs_type"
    lbLetRhs <- o .: "let_rhs"
    pure LetBinding{..}

instance MessagePack Pattern where
  fromObjectWith cfg = withMsgVariant "Pattern" \(name, arg) -> asumMsg
    [ PApp      <$> (guardMsg (name == "P_App"     ) >> fromObjectWith cfg arg)
    , PAttr     <$> (guardMsg (name == "P_Attr"    ) >> fromObjectWith cfg arg)
    , PBytes    <$> (guardMsg (name == "P_Bytes"   ) >> fromObjectWith cfg arg)
    , PCons     <$> (guardMsg (name == "P_Cons"    ) >> fromObjectWith cfg arg)
    , PCtor     <$> (guardMsg (name == "P_Ctor"    ) >> fromObjectWith cfg arg)
    , PFalse    <$> (guardMsg (name == "P_False"   ) >> fromObjectWith cfg arg)
    , PInt      <$> (guardMsg (name == "P_Int"     ) >> fromObjectWith cfg arg)
    , PList     <$> (guardMsg (name == "P_List"    ) >> fromObjectWith cfg arg)
    , PModPath  <$> (guardMsg (name == "P_ModPath" ) >> fromObjectWith cfg arg)
    , PMutez    <$> (guardMsg (name == "P_Mutez"   ) >> fromObjectWith cfg arg)
    , PTez      <$> (guardMsg (name == "P_Tez"     ) >> fromObjectWith cfg arg)
    , PNat      <$> (guardMsg (name == "P_Nat"     ) >> fromObjectWith cfg arg)
    , PPar      <$> (guardMsg (name == "P_Par"     ) >> fromObjectWith cfg arg)
    , PRecord   <$> (guardMsg (name == "P_Record"  ) >> fromObjectWith cfg arg)
    , PString   <$> (guardMsg (name == "P_String"  ) >> fromObjectWith cfg arg)
    , PTrue     <$> (guardMsg (name == "P_True"    ) >> fromObjectWith cfg arg)
    , PTuple    <$> (guardMsg (name == "P_Tuple"   ) >> fromObjectWith cfg arg)
    , PTyped    <$> (guardMsg (name == "P_Typed"   ) >> fromObjectWith cfg arg)
    , PUnit     <$> (guardMsg (name == "P_Unit"    ) >> fromObjectWith cfg arg)
    , PVar      <$> (guardMsg (name == "P_Var"     ) >> fromObjectWith cfg arg)
    , PVerbatim <$> (guardMsg (name == "P_Verbatim") >> fromObjectWith cfg arg)
    ]

instance MessagePack a => MessagePack (Par' a) where
  fromObjectWith _ = withMsgMap "Par" \o -> do
    pInside <- o .: "inside"
    pure Par'{..}

instance MessagePack a => MessagePack (ModulePath a) where
  fromObjectWith _ = withMsgMap "ModulePath" \o -> do
    mpModulePath <- o .: "module_path"
    mpField <- o .: "field"
    pure ModulePath{..}

instance (MessagePack lhs, MessagePack rhs) => MessagePack (Field lhs rhs) where
  fromObjectWith cfg = withMsgVariant "Field" \(name, arg) -> asumMsg
    [ Punned   <$> (guardMsg (name == "Punned"  ) >> fromObjectWith cfg arg)
    , Complete <$> (guardMsg (name == "Complete") >> fromObjectWith cfg arg)
    ]

instance MessagePack lhs => MessagePack (Punned lhs) where
  fromObjectWith _ = withMsgMap "Punned" \o -> do
    pPun <- o .: "pun"
    pure Pnd{..}

instance (MessagePack lhs, MessagePack rhs) => MessagePack (FullField lhs rhs) where
  fromObjectWith _ = withMsgMap "FullField" \o -> do
    ffFieldLhs <- o .: "field_lhs"
    ffFieldRhs <- o .: "field_rhs"
    pure FullField{..}

instance MessagePack TypeExpr where
  fromObjectWith cfg = withMsgVariant "TypeExpr" \(name, arg) -> asumMsg
    [ TApp         <$> (guardMsg (name == "T_App"        ) >> fromObjectWith cfg arg)
    , TArg         <$> (guardMsg (name == "T_Arg"        ) >> fromObjectWith cfg arg)
    , TAttr        <$> (guardMsg (name == "T_Attr"       ) >> fromObjectWith cfg arg)
    , TCart        <$> (guardMsg (name == "T_Cart"       ) >> fromObjectWith cfg arg)
    , TForAll      <$> (guardMsg (name == "T_ForAll"     ) >> fromObjectWith cfg arg)
    , TFun         <$> (guardMsg (name == "T_Fun"        ) >> fromObjectWith cfg arg)
    , TInt         <$> (guardMsg (name == "T_Int"        ) >> fromObjectWith cfg arg)
    , TModPath     <$> (guardMsg (name == "T_ModPath"    ) >> fromObjectWith cfg arg)
    , TPar         <$> (guardMsg (name == "T_Par"        ) >> fromObjectWith cfg arg)
    , TRecord      <$> (guardMsg (name == "T_Record"     ) >> fromObjectWith cfg arg)
    , TString      <$> (guardMsg (name == "T_String"     ) >> fromObjectWith cfg arg)
    , TSum         <$> (guardMsg (name == "T_Sum"        ) >> fromObjectWith cfg arg)
    , TVar         <$> (guardMsg (name == "T_Var"        ) >> fromObjectWith cfg arg)
    , TParameterOf <$> (guardMsg (name == "T_ParameterOf") >> fromObjectWith cfg arg)
    ]

instance MessagePack TypeCtorArg where
  fromObjectWith cfg = withMsgVariant "TypeCtorArg" \(name, arg) -> asumMsg
    [ TCSingle <$> (guardMsg (name == "TC_Single") >> fromObjectWith cfg arg)
    , TCTuple  <$> (guardMsg (name == "TC_Tuple" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack FieldDecl where
  fromObjectWith _ = withMsgMap "FieldDecl" \o -> do
    fdFieldName <- o .: "field_name"
    fdFieldType <- o .:? "field_type"
    pure FieldDecl{..}

instance MessagePack SumType where
  fromObjectWith _ = withMsgMap "SumType" \o -> do
    vtVariants <- o .: "variants"
    pure SumType{..}

instance MessagePack Variant where
  fromObjectWith _ = withMsgMap "Variant" \o -> do
    vCtor <- o .: "ctor"
    vCtorArgs <- o .:? "ctor_args"
    pure Variant{..}

instance MessagePack Expr where
  fromObjectWith cfg = withMsgVariant "Expr" \(name, arg) -> asumMsg
    [ EAdd        <$> (guardMsg (name == "E_Add"       ) >> fromObjectWith cfg arg)
    , EAnd        <$> (guardMsg (name == "E_And"       ) >> fromObjectWith cfg arg)
    , EApp        <$> (guardMsg (name == "E_App"       ) >> fromObjectWith cfg arg)
    , EAssign     <$> (guardMsg (name == "E_Assign"    ) >> fromObjectWith cfg arg)
    , EAttr       <$> (guardMsg (name == "E_Attr"      ) >> fromObjectWith cfg arg)
    , EBytes      <$> (guardMsg (name == "E_Bytes"     ) >> fromObjectWith cfg arg)
    , ECat        <$> (guardMsg (name == "E_Cat"       ) >> fromObjectWith cfg arg)
    , ECodeInj    <$> (guardMsg (name == "E_CodeInj"   ) >> fromObjectWith cfg arg)
    , ECond       <$> (guardMsg (name == "E_Cond"      ) >> fromObjectWith cfg arg)
    , ECons       <$> (guardMsg (name == "E_Cons"      ) >> fromObjectWith cfg arg)
    , EContractOf <$> (guardMsg (name == "E_ContractOf") >> fromObjectWith cfg arg)
    , ECtor       <$> (guardMsg (name == "E_Ctor"      ) >> fromObjectWith cfg arg)
    , EDiv        <$> (guardMsg (name == "E_Div"       ) >> fromObjectWith cfg arg)
    , EEqual      <$> (guardMsg (name == "E_Equal"     ) >> fromObjectWith cfg arg)
    , EFalse      <$> (guardMsg (name == "E_False"     ) >> fromObjectWith cfg arg)
    , EFor        <$> (guardMsg (name == "E_For"       ) >> fromObjectWith cfg arg)
    , EForIn      <$> (guardMsg (name == "E_ForIn"     ) >> fromObjectWith cfg arg)
    , EFun        <$> (guardMsg (name == "E_Fun"       ) >> fromObjectWith cfg arg)
    , EGeq        <$> (guardMsg (name == "E_Geq"       ) >> fromObjectWith cfg arg)
    , EGt         <$> (guardMsg (name == "E_Gt"        ) >> fromObjectWith cfg arg)
    , EInt        <$> (guardMsg (name == "E_Int"       ) >> fromObjectWith cfg arg)
    , ELand       <$> (guardMsg (name == "E_Land"      ) >> fromObjectWith cfg arg)
    , ELeq        <$> (guardMsg (name == "E_Leq"       ) >> fromObjectWith cfg arg)
    , ELetIn      <$> (guardMsg (name == "E_LetIn"     ) >> fromObjectWith cfg arg)
    , ELetMutIn   <$> (guardMsg (name == "E_LetMutIn"  ) >> fromObjectWith cfg arg)
    , EList       <$> (guardMsg (name == "E_List"      ) >> fromObjectWith cfg arg)
    , ELor        <$> (guardMsg (name == "E_Lor"       ) >> fromObjectWith cfg arg)
    , ELsl        <$> (guardMsg (name == "E_Lsl"       ) >> fromObjectWith cfg arg)
    , ELsr        <$> (guardMsg (name == "E_Lsr"       ) >> fromObjectWith cfg arg)
    , ELt         <$> (guardMsg (name == "E_Lt"        ) >> fromObjectWith cfg arg)
    , ELxor       <$> (guardMsg (name == "E_Lxor"      ) >> fromObjectWith cfg arg)
    , EMatch      <$> (guardMsg (name == "E_Match"     ) >> fromObjectWith cfg arg)
    , EMod        <$> (guardMsg (name == "E_Mod"       ) >> fromObjectWith cfg arg)
    , EModIn      <$> (guardMsg (name == "E_ModIn"     ) >> fromObjectWith cfg arg)
    , EModPath    <$> (guardMsg (name == "E_ModPath"   ) >> fromObjectWith cfg arg)
    , EMult       <$> (guardMsg (name == "E_Mult"      ) >> fromObjectWith cfg arg)
    , EMutez      <$> (guardMsg (name == "E_Mutez"     ) >> fromObjectWith cfg arg)
    , ETez        <$> (guardMsg (name == "E_Tez"       ) >> fromObjectWith cfg arg)
    , ENat        <$> (guardMsg (name == "E_Nat"       ) >> fromObjectWith cfg arg)
    , ENeg        <$> (guardMsg (name == "E_Neg"       ) >> fromObjectWith cfg arg)
    , ENeq        <$> (guardMsg (name == "E_Neq"       ) >> fromObjectWith cfg arg)
    , ENot        <$> (guardMsg (name == "E_Not"       ) >> fromObjectWith cfg arg)
    , EOr         <$> (guardMsg (name == "E_Or"        ) >> fromObjectWith cfg arg)
    , EPar        <$> (guardMsg (name == "E_Par"       ) >> fromObjectWith cfg arg)
    , EProj       <$> (guardMsg (name == "E_Proj"      ) >> fromObjectWith cfg arg)
    , ERecord     <$> (guardMsg (name == "E_Record"    ) >> fromObjectWith cfg arg)
    , ERevApp     <$> (guardMsg (name == "E_RevApp"    ) >> fromObjectWith cfg arg)
    , ESeq        <$> (guardMsg (name == "E_Seq"       ) >> fromObjectWith cfg arg)
    , EString     <$> (guardMsg (name == "E_String"    ) >> fromObjectWith cfg arg)
    , ESub        <$> (guardMsg (name == "E_Sub"       ) >> fromObjectWith cfg arg)
    , ETrue       <$> (guardMsg (name == "E_True"      ) >> fromObjectWith cfg arg)
    , ETuple      <$> (guardMsg (name == "E_Tuple"     ) >> fromObjectWith cfg arg)
    , ETyped      <$> (guardMsg (name == "E_Typed"     ) >> fromObjectWith cfg arg)
    , ETypeIn     <$> (guardMsg (name == "E_TypeIn"    ) >> fromObjectWith cfg arg)
    , EUnit       <$> (guardMsg (name == "E_Unit"      ) >> fromObjectWith cfg arg)
    , EUpdate     <$> (guardMsg (name == "E_Update"    ) >> fromObjectWith cfg arg)
    , EVar        <$> (guardMsg (name == "E_Var"       ) >> fromObjectWith cfg arg)
    , EVerbatim   <$> (guardMsg (name == "E_Verbatim"  ) >> fromObjectWith cfg arg)
    , EWhile      <$> (guardMsg (name == "E_While"     ) >> fromObjectWith cfg arg)
    ]

instance MessagePack a => MessagePack (BinOp a) where
  fromObjectWith _ = withMsgMap "BinOp" \o -> do
    boArg1 <- o .: "arg1"
    boOp <- o .: "op"
    boArg2 <- o .: "arg2"
    pure BinOp{..}

instance MessagePack Assign where
  fromObjectWith _ = withMsgMap "Assign" \o -> do
    aBinder <- o .: "binder"
    aAss <- o .: "ass"
    aExpr <- o .: "expr"
    pure Assign{..}

instance MessagePack CodeInj where
  fromObjectWith _ = withMsgMap "CodeInj" \o -> do
    ciLanguage <- o .: "language"
    ciCode <- o .: "code"
    pure CodeInj{..}

instance MessagePack CondExpr where
  fromObjectWith _ = withMsgMap "CondExpr" \o -> do
    ceTest <- o .: "test"
    ceIfSo <- o .: "if_so"
    ceIfNot <- o .:? "if_not"
    pure CondExpr{..}

instance MessagePack ForLoop where
  fromObjectWith _ = withMsgMap "ForLoop" \o -> do
    flIndex <- o .: "index"
    flEqual <- o .: "equal"
    flBound1 <- o .: "bound1"
    flDirection <- o .: "direction"
    flBound2 <- o .: "bound2"
    flBody <- o .: "body"
    pure ForLoop{..}

instance MessagePack Direction where
  fromObjectWith cfg = withMsgVariant "Direction" \(name, arg) -> asumMsg
    [ Upto   <$> (guardMsg (name == "Upto"  ) >> fromObjectWith cfg arg)
    , Downto <$> (guardMsg (name == "Downto") >> fromObjectWith cfg arg)
    ]

instance MessagePack LoopBody where
  fromObjectWith _ = withMsgMap "LoopBody" \o -> do
    lbSeqExpr <- o .: "seq_expr"
    pure LoopBody{..}

instance MessagePack ForInLoop where
  fromObjectWith _ = withMsgMap "ForInLoop" \o -> do
    filPattern <- o .: "pattern"
    filCollection <- o .: "collection"
    filBody <- o .: "body"
    pure ForInLoop{..}

instance MessagePack FunExpr where
  fromObjectWith _ = withMsgMap "FunExpr" \o -> do
    feTypeParams <- o .:? "type_params"
    feBinders <- o .: "binders"
    feRhsType <- o .:? "rhs_type"
    feBody <- o .: "body"
    pure FunExpr{..}

instance MessagePack LetIn where
  fromObjectWith _ = withMsgMap "LetIn" \o -> do
    liKwdLet <- o .: "kwd_let"
    let liIsRec = (.:!) @_ @WrappedLexeme o "kwd_rec"
          & runValidate
          & isRight
    liBinding <- o .: "binding"
    liBody <- o .: "body"
    pure LetIn{..}

instance MessagePack LetMutIn where
  fromObjectWith _ = withMsgMap "LetMutIn" \o -> do
    lmiKwdLet <- o .: "kwd_let"
    lmiBinding <- o .: "binding"
    lmiBody <- o .: "body"
    pure LetMutIn{..}

instance MessagePack MatchExpr where
  fromObjectWith _ = withMsgMap "MatchExpr" \o -> do
    meSubject <- o .: "subject"
    meClauses <- o .: "clauses"
    pure MatchExpr{..}

instance MessagePack MatchClause where
  fromObjectWith _ = withMsgMap "MatchClause" \o -> do
    mcPattern <- o .: "pattern"
    mcRhs <- o .: "rhs"
    pure MatchClause{..}

instance MessagePack ModuleIn where
  fromObjectWith _ = withMsgMap "ModuleIn" \o -> do
    miModDecl <- o .: "mod_decl"
    miBody <- o .: "body"
    pure ModuleIn{..}

instance MessagePack ModuleDecl where
  fromObjectWith _ = withMsgMap "ModuleDecl" \o -> do
    mdName <- o .: "name"
    mdAnnotation <- o .: "annotation"
    mdModuleExpr <- o .: "module_expr"
    pure ModuleDecl{..}

instance MessagePack SignatureExpr where
  fromObjectWith cfg = withMsgVariant "SignatureExpr" \(name, arg) -> asumMsg
    [ SSig  <$> (guardMsg (name == "S_Sig" ) >> fromObjectWith cfg arg)
    , SPath <$> (guardMsg (name == "S_Path") >> fromObjectWith cfg arg)
    , SVar  <$> (guardMsg (name == "S_Var" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack SignatureBody where
  fromObjectWith _ = withMsgMap "SignatureBody" \o -> do
    sbSigItems <- o .: "sig_items"
    pure SignatureBody{..}

instance MessagePack SigItem where
  fromObjectWith cfg = withMsgVariant "SigItem" \(name, arg) -> asumMsg
    [ SValue   <$> (guardMsg (name == "S_Value"  ) >> fromObjectWith cfg arg)
    , SType    <$> (guardMsg (name == "S_Type"   ) >> fromObjectWith cfg arg)
    , SInclude <$> (guardMsg (name == "S_Include") >> fromObjectWith cfg arg)
    , SAttr    <$> (guardMsg (name == "S_Attr"   ) >> fromObjectWith cfg arg)
    ]

instance MessagePack SigType where
  fromObjectWith _ = withMsgMap "SigType" \o -> do
    stTypeVars <- o .:? "type_vars"
    stTypeName <- o .: "type_name"
    stTypeRhs <- o .:? "type_rhs"
    pure SigType{..}

instance MessagePack SigValue where
  fromObjectWith _ = withMsgMap "SigValue" \o -> do
    svVar <- o .: "var"
    svValType <- o .: "val_type"
    pure SigValue{..}

instance MessagePack ModuleExpr where
  fromObjectWith cfg = withMsgVariant "ModuleExpr" \(name, arg) -> asumMsg
    [ MBody <$> (guardMsg (name == "M_Body") >> fromObjectWith cfg arg)
    , MPath <$> (guardMsg (name == "M_Path") >> fromObjectWith cfg arg)
    , MVar  <$> (guardMsg (name == "M_Var" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack ModuleBody where
  fromObjectWith _ = withMsgMap "ModuleBody" \o -> do
    mbDeclarations <- o .: "declarations"
    pure ModuleBody{..}

instance MessagePack a => MessagePack (UnOp a) where
  fromObjectWith _ = withMsgMap "UnOp" \o -> do
    uoOp <- o .: "op"
    uoArg <- o .: "arg"
    pure UnOp{..}

instance MessagePack Projection where
  fromObjectWith _ = withMsgMap "Projection" \o -> do
    pRecordOrTuple <- o .: "record_or_tuple"
    pFieldPath <- o .: "field_path"
    pure Projection{..}

instance MessagePack Selection where
  fromObjectWith cfg = withMsgVariant "Selection" \(name, arg) -> asumMsg
    [ FieldName <$> (guardMsg (name == "FieldName") >> fromObjectWith cfg arg)
    , Component <$> (guardMsg (name == "Component") >> fromObjectWith cfg arg)
    ]

instance MessagePack TypeIn where
  fromObjectWith _ = withMsgMap "TypeIn" \o -> do
    tiTypeDecl <- o .: "type_decl"
    tiBody <- o .: "body"
    pure TypeIn{..}

instance MessagePack TypeDecl where
  fromObjectWith _ = withMsgMap "TypeDecl" \o -> do
    tdParams <- o .:? "params"
    tdName <- o .: "name"
    tdTypeExpr <- o .: "type_expr"
    pure TypeDecl{..}

instance MessagePack TypeVars where
  fromObjectWith cfg = withMsgVariant "TypeVars" \(name, arg) -> asumMsg
    [ TVSingle <$> (guardMsg (name == "TV_Single") >> fromObjectWith cfg arg)
    , TVTuple  <$> (guardMsg (name == "TV_Tuple" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack UpdateExpr where
  fromObjectWith _ = withMsgMap "UpdateExpr" \o -> do
    ueRecord <- o .: "record"
    ueUpdates <- o .: "updates"
    pure UpdateExpr{..}

instance MessagePack Path where
  fromObjectWith cfg = withMsgVariant "Path" \(name, arg) -> asumMsg
    [ Name <$> (guardMsg (name == "Name") >> fromObjectWith cfg arg)
    , Path <$> (guardMsg (name == "Path") >> fromObjectWith cfg arg)
    ]

instance MessagePack SequenceExpr where
  fromObjectWith _ = withMsgMap "SequenceExpr" \o -> do
    seElements <- o .: "elements"
    pure SequenceExpr{..}

instance MessagePack WhileLoop where
  fromObjectWith _ = withMsgMap "WhileLoop" \o -> do
    wlCond <- o .: "cond"
    wlBody <- o .: "body"
    pure WhileLoop{..}

----------------
-- Conversion --
----------------

-- | Transform @CameLIGO@ CST into unified AST.
toAST :: CST -> LIGO Info
toAST CST{..} =
  let
    firstRange = point 1 1
    (lastRange, _) = unpackWrap cstEof
  in fastMake (firstRange `merged` lastRange) (AST.RawContract $ declConv <$> cstDecl)
  where
    declConv :: Declaration -> LIGO Info
    declConv = \case
      DAttr      (unpackReg -> (_, (Tuple1 decl))) -> declConv decl
      DLet       (unpackReg -> (r, letDecl))       -> letDeclConv r letDecl
      DModule    modDecl                           -> moduleDeclConv modDecl
      DInclude   includeDecl                       -> includeDeclConv includeDecl
      DSignature signatureDecl                     -> signatureDeclConv signatureDecl
      DType      typeDecl                          -> typeDeclConv typeDecl

    includeDeclConv :: Reg ModuleInclude -> LIGO Info
    includeDeclConv (unpackReg -> (r, ModuleInclude{..})) =
      fastMake r (AST.BInclude $ moduleExprConv miModuleExpr)

    signatureDeclConv :: Reg SignatureDecl -> LIGO Info
    signatureDeclConv (unpackReg -> (r, SignatureDecl{..})) =
      let
        name = makeWrappedLexeme AST.ModuleName sdName
        signature = signatureExprConv sdSignatureExpr
      in fastMake r (AST.BSignature name signature [])

    letDeclConv :: Range -> LetDecl -> LIGO Info
    letDeclConv r LetDecl{ldLetBinding = LetBinding{..}, ..} =
      let
        isRec = ldIsRec
        tvNames = typeParamsConv . pInside . rValue <$> lbTypeParams
          & maybeToList
          & concat
        typ = typeAnnotationConv <$> lbRhsType
        expr = exprConv lbLetRhs
      in
      case lbBinders of
        pat :| [] ->
          fastMake r (AST.BConst isRec (patConv pat) tvNames typ (Just expr))
        PVar varName :| rest ->
          let
            funcName = makeWrappedLexeme AST.Name varName
            params = patConv <$> rest
          in fastMake r (AST.BFunction isRec funcName tvNames params typ expr)
        pat :| _ ->
          fastMake r (AST.BConst isRec (patConv pat) tvNames typ (Just expr))

    typeParamsConv :: TypeParams -> [LIGO Info]
    typeParamsConv (Tuple1 typeNames) = typeNames
      <&> makeWrappedLexeme AST.TypeVariableName
      & toList

    typeAnnotationConv :: TypeAnnotation -> LIGO Info
    typeAnnotationConv = typeExprConv . unTuple1

    patConv :: Pattern -> LIGO Info
    patConv = \case
      PApp (unpackReg -> (r, (ctor, arg))) -> fastMake r (AST.IsConstr (patConv ctor) (patConv <$> maybeToList arg))
      PAttr (Tuple1 pat) -> patConv pat
      PBytes (unpackWrap -> (r, Tuple1 bts)) -> makeConstantPat r (AST.CBytes bts)
      PCons (unpackReg -> (r, (x, xs))) -> fastMake r (AST.IsCons (patConv x) (patConv xs))
      PCtor ctor -> makeWrappedLexeme AST.Ctor ctor
      PFalse (unpackWrap -> (r, _)) -> fastMake r AST.IsFalse
      PInt (unpackWrap -> (r, Tuple1 n)) -> makeConstantPat r (AST.CInt n)
      PList (unpackReg -> (r, Par' pats)) -> fastMake r (AST.IsList (patConv <$> pats))
      PModPath (unpackReg -> (r, modPath)) -> fastMake r (modPathConv $ patConv <$> modPath)
      PMutez (unpackWrap -> (r, Tuple1 tez)) -> makeConstantPat r (AST.CTez tez)
      PTez (unpackWrap -> (r, Tuple1 tez)) -> makeConstantPat r (AST.CTez tez)
      PNat (unpackWrap -> (r, Tuple1 n)) -> makeConstantPat r (AST.CNat n)
      PPar (unpackReg -> (r, Par' pat)) -> fastMake r (AST.IsParen (patConv pat))
      PRecord (unpackReg -> (r, Par' recFields)) -> fastMake r (AST.IsRecord (fieldPatConv <$> recFields))
      PString (unpackWrap -> (r, str)) -> makeConstantPat r (AST.CString $ escapeText str)
      PTrue (unpackWrap -> (r, _)) -> fastMake r AST.IsTrue
      PTuple (unpackReg -> (r, args)) -> fastMake r (AST.IsTuple (toList $ patConv <$> args))
      PTyped (unpackReg -> (r, (pat, ann))) -> fastMake r (AST.IsAnnot (patConv pat) (typeAnnotationConv ann))
      PVar var@(unpackWrap -> (r, v))
        | v == "_" -> fastMake r AST.IsWildcard
        | otherwise ->
          let nameDecl = makeWrappedLexeme AST.NameDecl var in
          fastMake r (AST.IsVar nameDecl)
      PVerbatim verb -> makeWrappedLexeme AST.Verbatim verb
      PUnit (unpackReg -> (r, _)) -> fastMake r (AST.Ctor "Unit")
      where
        makeConstantPat :: Range -> AST.Constant (LIGO Info) -> LIGO Info
        makeConstantPat r c = fastMake r $ AST.IsConstant (fastMake r c)

        fieldPatConv :: Field WrappedLexeme Pattern -> LIGO Info
        fieldPatConv = \case
          Punned (unpackReg -> (r, Pnd{..})) ->
            let fieldName = makeWrappedLexeme AST.FieldName pPun in
            fastMake r (AST.IsRecordCapture fieldName)
          Complete (unpackReg -> (r, FullField{..})) ->
            let
              fieldName = makeWrappedLexeme AST.FieldName ffFieldLhs
              rhsPat = patConv ffFieldRhs
            in fastMake r (AST.IsRecordField fieldName rhsPat)

    exprConv :: Expr -> LIGO Info
    exprConv = \case
      EAdd op -> makeBinOp op
      EAnd op -> makeBinOp op
      EApp (unpackReg -> (r, (f, xs))) ->
        fastMake r (AST.Apply (exprConv f) (toList $ exprConv <$> xs))
      EAssign (unpackReg -> (r, Assign{..})) ->
        let
          assign = makeWrappedLexeme AST.Op aAss
          binder = makeWrappedLexeme AST.Name aBinder
        in fastMake r (AST.AssignOp binder assign (exprConv aExpr))
      EAttr (Tuple1 expr) -> exprConv expr
      EBytes (unpackWrap -> (r, Tuple1 bts)) -> makeConstantExpr r (AST.CBytes bts)
      ECat op -> makeBinOp op
      ECodeInj (unpackReg -> (r, CodeInj{..})) ->
        let
          (langR, langName) = second (snd . unpackReg) $ unpackWrap ciLanguage
          lang = fastMake langR (AST.Attr langName)
        in fastMake r (AST.CodeInj lang (exprConv ciCode))
      ECond (unpackReg -> (r, CondExpr{..})) ->
        fastMake r (AST.If (exprConv ceTest) (exprConv ceIfSo) (exprConv . unTuple1 <$> ceIfNot))
      ECons op -> makeBinOp op
      EContractOf (unpackReg -> (r, path)) ->
        let
          lastPart = last path
          modPath = ModulePath (init path) (makeWrappedLexeme AST.ModuleName lastPart)
          modAccess = fastMake (head path `mergeRanges` lastPart) (modPathConv modPath)
        in fastMake r (AST.Contract modAccess)
      ECtor ctor -> makeWrappedLexeme AST.Ctor ctor
      EDiv op -> makeBinOp op
      EEqual op -> makeBinOp op
      EFalse (unpackWrap -> (r, _)) -> fastMake r AST.EFalse
      EFor (unpackReg -> (r, ForLoop{..})) ->
        let
          varName = makeWrappedLexeme AST.Name flIndex
          eqOp = makeWrappedLexeme AST.Op flEqual
          from = exprConv flBound1
          initExpr = fastMake (extract varName `merged` extract from) (AST.BinOp varName eqOp from)
          dir = case flDirection of
            Upto (unpackWrap -> (r', _)) -> fastMake r' AST.Upto
            Downto (unpackWrap -> (r', _)) -> fastMake r' AST.Downto
          to = exprConv flBound2
          body = makeLoopBody flBody
        in fastMake r (AST.ForLoop (Just initExpr) (Just dir) [to] (Just body))
      EForIn (unpackReg -> (r, ForInLoop{..})) ->
        let
          pat = patConv filPattern
          col = exprConv filCollection
          body = makeLoopBody filBody
        in fastMake r (AST.ForOfLoop pat col body)
      EFun (unpackReg -> (r, FunExpr{..})) ->
        let
          varDecls = toList $ patConv <$> feBinders
          tvNames = typeParamsConv . pInside . rValue <$> feTypeParams
            & maybeToList
            & concat
          typ = typeAnnotationConv <$> feRhsType
          expr = exprConv feBody
        in fastMake r (AST.Lambda varDecls tvNames typ expr)
      EGeq op -> makeBinOp op
      EGt op -> makeBinOp op
      EInt (unpackWrap -> (r, Tuple1 n)) -> makeConstantExpr r (AST.CInt n)
      ELand op -> makeBinOp op
      ELeq op -> makeBinOp op
      ELetIn (unpackReg -> (r, LetIn{..})) ->
        let
          start = fst $ unpackWrap liKwdLet
          (stop, binding) = unpackReg liBinding
          letDecl = letDeclConv (merged start stop) $ LetDecl liIsRec binding
          rhs = exprConv liBody
        in fastMake r (AST.Let letDecl rhs)
      ELetMutIn (unpackReg -> (r, LetMutIn{..})) ->
        let
          start = fst $ unpackWrap lmiKwdLet
          (stop, binding) = unpackReg lmiBinding
          letDecl = letDeclConv (merged start stop) $ LetDecl False binding
          rhs = exprConv lmiBody
        in fastMake r (AST.Let letDecl rhs)
      EList (unpackReg -> (r, Par' xs)) -> fastMake r (AST.List (exprConv <$> xs))
      ELor op -> makeBinOp op
      ELsl op -> makeBinOp op
      ELsr op -> makeBinOp op
      ELt op -> makeBinOp op
      ELxor op -> makeBinOp op
      EMatch (unpackReg -> (r, MatchExpr{..})) ->
        let
          subject = exprConv meSubject
          alts = meClauses
            & snd . unpackReg
            <&> do \(unpackReg -> (r', MatchClause{..})) ->
                    fastMake r' (AST.Alt (patConv mcPattern) (exprConv mcRhs))
            & toList
        in fastMake r (AST.Case subject alts)
      EMod op -> makeBinOp op
      EModIn (unpackReg -> (r, ModuleIn{..})) ->
        let
          moduleDecl = moduleDeclConv miModDecl
          body = exprConv miBody
        in fastMake r (AST.Let moduleDecl body)
      EModPath (unpackReg -> (r, modPath)) -> fastMake r (modPathConv $ exprConv <$> modPath)
      EMult op -> makeBinOp op
      EMutez (unpackWrap -> (r, Tuple1 tez)) -> makeConstantExpr r (AST.CTez tez)
      ETez (unpackWrap -> (r, Tuple1 tez)) -> makeConstantExpr r (AST.CTez tez)
      ENat (unpackWrap -> (r, Tuple1 nat)) -> makeConstantExpr r (AST.CNat nat)
      ENeg op -> makeUnOp op
      ENeq op -> makeBinOp op
      ENot op -> makeUnOp op
      EOr op -> makeBinOp op
      EPar (unpackReg -> (r, Par' expr)) -> fastMake r (AST.Paren (exprConv expr))
      EProj proj -> projConv proj
      ERecord (unpackReg -> (r, Par' recFields)) ->
        fastMake r (AST.Record (fieldExprConv (makeWrappedLexeme AST.FieldName) <$> recFields))
      EString (unpackWrap -> (r, str)) -> makeConstantExpr r (AST.CString $ escapeText str)
      ESub op -> makeBinOp op
      ETrue (unpackWrap -> (r, _)) -> fastMake r AST.ETrue
      ETuple (unpackReg -> (r, xs)) -> fastMake r (AST.Tuple (exprConv <$> toList xs))
      ETyped (unpackReg -> (r, Par' (expr, typ))) ->
        fastMake r (AST.Annot (exprConv expr) (typeAnnotationConv typ))
      ETypeIn (unpackReg -> (r, TypeIn{..})) ->
        let
          typeDecl = typeDeclConv tiTypeDecl
          body = exprConv tiBody
        in fastMake r (AST.Let typeDecl body)
      EUnit (unpackReg -> (r, _)) -> fastMake r (AST.Ctor "Unit")
      EUpdate (unpackReg -> (r, Par' UpdateExpr{..})) ->
        let
          name = exprConv ueRecord
          assignments = ueUpdates
            <&> do
                  fieldExprConv \case
                    Name fName -> makeWrappedLexeme AST.FieldName fName
                    Path proj -> projConv proj
            & toList
        in fastMake r (AST.RecordUpd name assignments)
      EVar v -> makeWrappedLexeme AST.Name v
      EVerbatim verb -> makeWrappedLexeme AST.Verbatim verb
      ESeq (unpackReg -> (r, SequenceExpr{..})) ->
        fastMake r (AST.Seq (exprConv <$> seElements))
      ERevApp op -> makeBinOp op
      EWhile (unpackReg -> (r, WhileLoop{..})) ->
        let
          cond = exprConv wlCond
          body = makeLoopBody wlBody
        in fastMake r (AST.WhileLoop cond body)
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

        makeLoopBody :: Reg LoopBody -> LIGO Info
        makeLoopBody (unpackReg -> (r, LoopBody{..})) =
          fastMake r (AST.Seq (exprConv <$> lbSeqExpr))

        projConv :: Reg Projection -> LIGO Info
        projConv (unpackReg -> (r, Projection{..})) =
          let
            name = exprConv pRecordOrTuple
            selection = pFieldPath
              <&> do \case
                      FieldName fName -> makeWrappedLexeme AST.FieldName fName
                      Component n -> makeWrappedLexeme AST.CInt (unTuple1 <$> n)
              & toList
          in fastMake r (AST.QualifiedName name selection)

        fieldExprConv :: (lhs -> LIGO Info) -> Field lhs Expr -> LIGO Info
        fieldExprConv f = \case
          Punned (unpackReg -> (r, Pnd{..})) ->
            let fieldName = f pPun in
            fastMake r (AST.Capture fieldName)
          Complete (unpackReg -> (r, FullField{..})) ->
            let
              fieldName = f ffFieldLhs
              rhsExpr = exprConv ffFieldRhs
            in fastMake r (AST.FieldAssignment [fieldName] rhsExpr)

    typeExprConv :: TypeExpr -> LIGO Info
    typeExprConv = \case
      TApp (unpackReg -> (r, (typ, args))) ->
        let
          f = typeExprConv typ
          xs = case args of
            TCSingle inner -> [typeExprConv inner]
            TCTuple (unpackReg -> (_, Par' typs)) -> typeExprConv <$> toList typs
        in fastMake r (AST.TApply f xs)
      TArg (unpackReg -> (r, Tuple1 name)) ->
        let
          typName = makeWrappedLexeme AST.TypeVariableName name
        in fastMake r (AST.TVariable typName)
      TAttr (Tuple1 typExpr) -> typeExprConv typExpr
      TCart (unpackReg -> (r, (x, xs))) ->
        let
          types = toList $ typeExprConv <$> x <| xs
        in fastMake r (AST.TProduct types)
      TForAll (unpackReg -> (r, (typeVars, typeExpr))) ->
        let
          vars = toList $ typeVarConv <$> typeVars
          tExpr = typeExprConv typeExpr
        in fastMake r (AST.TForAll vars tExpr)
      TFun (unpackReg -> (r, (dom, codom))) ->
        fastMake r (AST.TArrow (typeExprConv dom) (typeExprConv codom))
      TInt n@(unpackWrap -> (r, _)) ->
        let
          intVal = makeWrappedLexeme AST.CInt (unTuple1 <$> n)
        in fastMake r (AST.TInt intVal)
      TModPath (unpackReg -> (r, modAccess)) ->
        fastMake r (modPathConv $ typeExprConv <$> modAccess)
      TPar (unpackReg -> (r, Par' typ)) -> fastMake r (AST.TParen (typeExprConv typ))
      TRecord (unpackReg -> (r, Par' decls)) ->
        let
          fieldDecls = decls
            <&> do \(unpackReg -> (r', FieldDecl{..})) ->
                    let
                      name = makeWrappedLexeme AST.Name fdFieldName
                      typ = typeAnnotationConv <$> fdFieldType
                    in fastMake r' (AST.TField name typ)
        in fastMake r (AST.TRecord def fieldDecls)
      TString str@(unpackWrap -> (r, _)) ->
        let
          strVal = makeWrappedLexeme AST.CString (escapeText <$> str)
        in fastMake r (AST.TString strVal)
      TSum (unpackReg -> (r, SumType{..})) ->
        let
          variants = vtVariants
            <&> do \(unpackReg -> (r', Variant{..})) ->
                    let
                      name = makeWrappedLexeme AST.Name vCtor
                      arg = typeExprConv . unTuple1 <$> maybeToList vCtorArgs
                    in fastMake r' (AST.Variant name arg)
        in fastMake r (AST.TSum def variants)
      TVar var@(unpackWrap -> (r, v))
        | v == "_" -> fastMake r AST.TWildcard
        | otherwise -> makeWrappedLexeme AST.TypeName var
      TParameterOf (unpackReg -> (r, parts)) ->
        let
          lastPart = last parts
          modPath = ModulePath (init parts) (makeWrappedLexeme AST.ModuleName lastPart)
          modAccess = fastMake (head parts `mergeRanges` lastPart) (modPathConv modPath)
        in fastMake r (AST.TParameter modAccess)

    mergeRanges :: Wrap a -> Wrap b -> Range
    mergeRanges (unpackWrap -> (r1, _)) (unpackWrap -> (r2, _)) =
      r1 `merged` r2

    modPathConv :: ModulePath (LIGO Info) -> AST.ModuleAccess (LIGO Info)
    modPathConv ModulePath{..} =
      let
        path = mpModulePath
          <&> makeWrappedLexeme AST.ModuleName
      in AST.ModuleAccess path mpField

    moduleExprConv :: ModuleExpr -> LIGO Info
    moduleExprConv = \case
      MBody (unpackReg -> (r, ModuleBody{..})) ->
        fastMake r (AST.ModuleExpr $ declConv <$> mbDeclarations)
      MPath (unpackReg -> (r, moduleDecl)) ->
        moduleDecl
          <&> makeWrappedLexeme AST.ModuleName
          & fastMake r . modPathConv
      MVar name'@(unpackWrap -> (r, _)) ->
        ModulePath [] (makeWrappedLexeme AST.ModuleName name')
          & fastMake r . modPathConv

    moduleDeclConv :: Reg ModuleDecl -> LIGO Info
    moduleDeclConv (unpackReg -> (r, ModuleDecl{..})) =
      let
        annMb = signatureExprConv . unTuple1 <$> mdAnnotation
        name = makeWrappedLexeme AST.Name mdName
        modExpr = moduleExprConv mdModuleExpr
      in
        case mdModuleExpr of
          MBody{} -> fastMake r (AST.BModuleDecl name (one <$> annMb) modExpr)
          _ -> fastMake r (AST.BModuleAlias name annMb modExpr)

    signatureExprConv :: SignatureExpr -> LIGO Info
    signatureExprConv = \case
      SPath (unpackReg -> (r, modPath)) ->
        fastMake r (modPathConv $ makeWrappedLexeme AST.ModuleName <$> modPath)
      SVar name -> makeWrappedLexeme AST.ModuleName name
      SSig (unpackReg -> (r, SignatureBody{..})) -> fastMake r (AST.Signature $ sigItemConv <$> sbSigItems)
        where
          sigItemConv :: SigItem -> LIGO Info
          sigItemConv = \case
            SValue (unpackReg -> (r', SigValue{..})) ->
              let
                name' = makeWrappedLexeme AST.Name svVar
                typ' = typeExprConv svValType
              in fastMake r' (AST.SValue name' typ')
            SType (unpackReg -> (r', SigType{..})) ->
              let
                typName = makeWrappedLexeme AST.TypeName stTypeName
                typ' = typeExprConv . unTuple1 <$> stTypeRhs
              in fastMake r' (AST.SType typName typ')
            SInclude (unpackReg -> (r', (Tuple1 sigExpr))) ->
              fastMake r' (AST.SInclude $ signatureExprConv sigExpr)
            SAttr (unpackReg -> (_, (Tuple1 sigItem))) -> sigItemConv sigItem

    typeVarConv :: TypeVar -> LIGO Info
    typeVarConv (unpackReg -> (_, Tuple1 typVar)) =
      makeWrappedLexeme AST.TypeVariableName typVar

    typeDeclConv :: Reg TypeDecl -> LIGO Info
    typeDeclConv (unpackReg -> (r, TypeDecl{..})) =
      let
        name = makeWrappedLexeme AST.Name tdName
        params = tdParams
          <&> do \case
                  TVSingle typVar@(unpackReg -> (r', _)) ->
                    fastMake r' (AST.QuotedTypeParam (typeVarConv typVar))
                  TVTuple (unpackReg -> (r', Par' typVars)) ->
                    let
                      varNames = typeVarConv <$> toList typVars
                    in fastMake r' (AST.QuotedTypeParams varNames)
        typ = typeExprConv tdTypeExpr
      in fastMake r (AST.BTypeDecl name params typ)
