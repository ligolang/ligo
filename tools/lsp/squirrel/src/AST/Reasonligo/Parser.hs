-- | Parser for ReasonLigo contract

module AST.Reasonligo.Parser where

import           Duplo.Error
import           Duplo.Pretty
import           Duplo.Tree

import           AST.Skeleton

import           Parser
import           ParseTree
import           Product

example :: FilePath
-- example = "../../../src/test/contracts/counter.religo"
-- example = "./contracts/variant.religo"
-- example = "./contracts/amount.religo"
example = "./contracts/multisig.religo"
-- example = "../../../src/test/contracts/FA1.2.religo"
-- example = "../../../src/test/contracts/multisig.religo"
-- example = "../../../src/test/contracts/lambda.religo"
-- example = "../../../src/test/contracts/record.religo"
-- example = "../../../src/test/contracts/tuple_type.religo"
-- example = "../../../src/test/contracts/empty_case.religo"
-- example = "./contracts/empty_case.religo"
-- example = "./contracts/tuple_type.religo"
-- example = "./contracts/assert.religo"
-- example = "./contracts/tuples_no_annotation.religo"
-- example = "./contracts/match.religo"
-- example = "./contracts/let_multiple.religo"
-- example = "./contracts/attributes.religo"
-- example = "./contracts/lambda.religo"
-- example = "./contracts/arithmetic.religo"
-- example = "./contracts/letin.religo"

raw :: IO ()
raw = mkRawTreeReason (Path example)
    >>= print . pp

sample :: IO ()
sample = mkRawTreeReason (Path example)
    >>= runParserM . recognise
    >>= print . pp . fst

recognise :: RawTree -> ParserM (LIGO Info)
recognise = descent (\_ -> error . show . pp) $ map usingScope
  [ -- Contract
    Descent do
      boilerplate $ \case
        "contract" -> RawContract <$> fields "declaration"
        _ -> fallthrough

  -- ReasonExpr
  , Descent do
      boilerplate $ \case
        "bracket_block" -> Block <$> fields "statement" <*> fieldOpt "return"
        _ -> fallthrough


    -- Expr
  , Descent do
      boilerplate $ \case
        "fun_call"          -> Apply     <$> field  "f"         <*> field "arguments"
        "lambda_call" -> Apply <$> field "lambda" <*> field "arguments" -- TODO: maybe a separate apply?
        "arguments"         -> Tuple     <$> fields "argument"
        "unary_call"       -> UnOp      <$> field  "negate"    <*> field "arg"
        "binary_call"             -> BinOp     <$> field  "left"      <*> field "op"   <*> field "right"
        "constructor_call" -> Apply <$> field "constructor" <*> field "parameters"
        "block"             -> Seq       <$> fields "statement"
        "list_expr"         -> List      <$> fields "element"
        "list_access"         -> ListAccess <$> field "name" <*> fields "indexes"
        "annot_expr"        -> Annot     <$> field  "subject"   <*> field "type"
        "conditional"       -> If        <$> field  "selector"  <*> field "then" <*> field "else"
        "record_expr"       -> Record    <$> fields "assignment"
        "tuple_expr"        -> Tuple     <$> fields "element"

        "switch_instr"        -> Case      <$> field  "subject"    <*> fields   "case"
        "lambda"          -> Lambda    <$> field  "arguments" <*> fieldOpt    "lambda_type"  <*> field "lambda_body"
        _                   -> fallthrough

    -- Pattern
  , Descent do
      boilerplate $ \case
        "constr_pattern" -> IsConstr <$> field  "constr" <*> fieldOpt "arguments"
        "tuple_pattern"       -> IsTuple  <$> fields "element"
        "cons_pattern"        -> IsCons   <$> field  "head"   <*> field "tail"
        "annot_pattern"       -> IsAnnot <$> field "subject" <*> field "type"
        _                     -> fallthrough

    -- Alt
  , Descent do
      boilerplate $ \case
        "alt"  -> Alt <$> field "pattern" <*> field  "body"
        _                   -> fallthrough

    -- Record fields
  , Descent do
      boilerplate $ \case
        "record_field"      -> FieldAssignment <$> field "name" <*> field "value"
        "spread" -> Spread <$> field "name"
        _                  -> fallthrough

    -- MapBinding
  , Descent do
      boilerplate $ \case
        "binding" -> MapBinding <$> field "key" <*> field "value"
        _         -> fallthrough

  , Descent do
      boilerplate' $ \case
        ("+", _) -> return $ Op "+"
        ("-", _) -> return $ Op "-"
        ("mod", _) -> return $ Op "mod"
        ("/", _) -> return $ Op "/"
        ("*", _) -> return $ Op "*"
        (">", _) -> return $ Op ">"
        ("<", _) -> return $ Op "<"
        (">=", _) -> return $ Op ">="
        ("<=", _) -> return $ Op "<="
        ("==", _) -> return $ Op "=="
        ("!=", _) -> return $ Op "!="
        ("||", _) -> return $ Op "||"
        ("&&", _) -> return $ Op "&&"
        ("negate", n) -> return $ Op n
        _         -> fallthrough

  , Descent do
      boilerplate $ \case
        "module_qualified"    -> QualifiedName <$> field "module"    <*> fields "method"
        "struct_qualified"    -> QualifiedName <$> field "struct" <*> fields "method"
        _                 -> fallthrough

    -- Literal
  , Descent do
      boilerplate' $ \case
        ("Int",    i) -> return $ Int i
        ("Nat",    i) -> return $ Nat i
        ("Bytes",  i) -> return $ Bytes i
        ("String", i) -> return $ String i
        ("Tez",    i) -> return $ Tez i
        _             -> fallthrough

    -- Declaration
  , Descent do
      boilerplate $ \case
        -- TODO: Current `Let` in ast is untyped
        "let_declaration"   -> Var      <$>             field    "binding"       <*> fieldOpt "let_type" <*> field "let_value"
        "type_decl"  -> TypeDecl <$>             field    "type_name"   <*> field "type_value"
        "attr_decl" -> Attribute <$> field "name"
        _            -> fallthrough

    -- Parameters
  , Descent do
      boilerplate $ \case
        "parameters" -> Parameters <$> fields "parameter"
        _            -> fallthrough

    -- VarDecl
  , Descent do
      boilerplate $ \case
        "param_decl" -> Decl <$> field "access" <*> field "name" <*> field "type"
        _            -> fallthrough

    -- Name
  , Descent do
      boilerplate' $ \case
        ("Name", n) -> return $ Name n
        ("and", _)  -> return $ Name "and"
        ("or", _)   -> return $ Name "or"
        _           -> fallthrough

    -- Type
  , Descent do
      boilerplate $ \case
        "fun_type"         -> TArrow   <$> field  "domain"     <*> field "codomain"
        -- TODO: maybe only one argument of parameter list is considered
        "type_application"      -> TApply   <$> field  "functor" <*> field "parameter"
        "type_tuple"       -> TTuple   <$> fields "element"
        "record_type"      -> TRecord  <$> fields "field"
        "sum_type"         -> TSum     <$> fields "variant"
        _                 -> fallthrough

    -- Variant
  , Descent do
      boilerplate $ \case
        "variant" -> Variant <$> field "constructor" <*> fieldOpt "arguments"
        _         -> fallthrough

    -- TField
  , Descent do
      boilerplate $ \case
        "field_decl" -> TField <$> field "field_name" <*> field "field_type"
        _            -> fallthrough

    -- TypeName
  , Descent do
      boilerplate' $ \case
        ("TypeName", name) -> return $ TypeName name
        _                  -> fallthrough

    -- Ctor
  , Descent do
      boilerplate' $ \case
        ("Name_Capital", name) -> return $ Ctor name
        ("Some", _)            -> return $ Ctor "Some"
        ("None", _)            -> return $ Ctor "None"
        ("Bool", b)            -> return $ Ctor b
        ("Unit", _)            -> return $ Ctor "Unit"
        ("Nil", _)             -> return $ Ctor "Nil"
        _                      -> fallthrough

  -- Err
  , Descent do
      \(r :> _, ParseTree _ _ msg) -> do
        withComments do
          return (r :> N :> Nil, Err msg)

  , Descent do
      \case
        (r :> _, ParseTree "ERROR" _ msg) -> do
          return ([] :> r :> Y :> Nil, Err msg)

        _ -> fallthrough
  ]
