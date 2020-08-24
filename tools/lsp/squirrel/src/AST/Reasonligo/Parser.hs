-- | Parser for ReasonLigo contract

module AST.Reasonligo.Parser where

import           Duplo.Error
import           Duplo.Pretty
import           Duplo.Tree

import           AST.Skeleton

import           Parser
import           ParseTree
import           Product

-- example :: FilePath
-- example = "../../../src/test/contracts/counter.religo"
-- example = "./contracts/variant.religo"
-- example = "./contracts/amount.religo"
-- example = "./contracts/multisig.religo"
-- example = "./contracts/arithmetic.religo"
-- example = "./contracts/lambda.religo"
-- example = "./contracts/id.religo"
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
-- example = "./contracts/FA2.religo"

recognise :: RawTree -> ParserM (LIGO Info)
recognise = descent (error "Reasonligo.recognise") $ map usingScope
  [ -- Contract
    Descent do
      boilerplate $ \case
        "contract" -> RawContract <$> fields "declaration"
        _ -> fallthrough

    -- Expr
  , Descent do
      boilerplate $ \case
        "unary_call"  -> UnOp       <$> field  "negate"    <*> field "arg"
        "binary_call" -> BinOp      <$> field  "left"      <*> field "op"        <*> field "right"
        "Some_call"   -> Apply      <$> field  "some"      <*> fields "argument"
        "apply"       -> Apply      <$> field  "function"  <*> fields "argument"
        "block"       -> Seq        <$> fields "statement"
        "list"        -> List       <$> fields "element"
        "indexing"    -> ListAccess <$> field  "box"       <*> fields "index"
        "annot_expr"  -> Annot      <$> field  "subject"   <*> field "type"
        "if"          -> If         <$> field  "selector"  <*> field "then"      <*> fieldOpt "else"
        "record"      -> Record     <$> fields "assignment"
        "tuple"       -> Tuple      <$> fields "item"
        "switch"      -> Case       <$> field  "subject"   <*> fields   "alt"
        "lambda"      -> Lambda     <$> fields "argument"  <*> fieldOpt "type"   <*> field "body"
        _             -> fallthrough

    -- Pattern
  , Descent do
      boilerplate $ \case
        "tuple_pattern"          -> IsTuple <$> fields "pattern"
        "annot_pattern"          -> IsAnnot <$> field "subject" <*> field "type"
        "list_pattern"           -> IsList  <$> fields "pattern"
        "var_pattern"            -> IsVar   <$> field "var"
        "wildcard"               -> return IsWildcard
        "nullary_constr_pattern" -> IsConstr <$> field "constructor" <*> return Nothing
        "unary_constr_pattern"   -> IsConstr <$> field "constructor" <*> fieldOpt "arg"
        "spread_pattern"         -> IsSpread <$> field "expr"
        _                        -> fallthrough

    -- Alt
  , Descent do
      boilerplate $ \case
        "alt"  -> Alt <$> field "pattern" <*> field "expr"
        _                   -> fallthrough

    -- Record fields
    -- TODO: capture and record
  , Descent do
      boilerplate $ \case
        "record_field"      -> FieldAssignment <$> field "name" <*> field "value"
        "spread" -> Spread <$> field "name"
        _                   -> fallthrough

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
        "qualified_name"    -> QualifiedName <$> field "expr" <*> fields "name"
        "lhs" -> QualifiedName <$> field "callee" <*> fields "name"
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
        -- TODO: We forget "rec" field in let
        "let_declaration"   -> Var      <$>             field    "binding"       <*> fieldOpt "type" <*> field "value"
        "type_decl"  -> TypeDecl <$>             field    "type_name"   <*> field "type_value"
        "attr_decl" -> Attribute <$> field "name"
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
        "type_application" -> TApply   <$> field  "functor" <*> fields "argument"
        "type_tuple"       -> TTuple   <$> fields "element"
        "record_type"      -> TRecord  <$> fields "field"
        "sum_type"         -> TSum     <$> fields "variant"
        _                  -> fallthrough

   -- Michelson pair types
  , Descent do
      boilerplate' $ \case
        ("type_string", i) -> return $ TString i
        _                  -> fallthrough


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
