-- TODO: recogniser does not recognize maps and bigmaps properly

module AST.Parser.Camligo where

import AST.Skeleton

import Duplo.Tree

import ParseTree
import Parser
import Product

-- example :: FilePath
-- example = "../../../src/test/contracts/address.mligo"
-- example = "../../../src/test/contracts/amount_lambda.mligo"
-- example = "../../../src/test/contracts/attributes.mligo"
-- example = "../../../src/test/contracts/assert.mligo"
-- example = "../../../src/test/contracts/amount.mligo"
-- example = "../../../src/test/contracts/arithmetic.mligo"
-- example = "../../../src/test/contracts/basic.mligo"
-- example = "../../../src/test/contracts/bytes_arithmetic.mligo"
-- example = "../../../src/test/contracts/bitwise_arithmetic.mligo"
-- example = "../../../src/test/contracts/big_map.mligo"
-- example = "../../../src/test/contracts/boolean_operators.mligo"
-- example = "../../../src/test/contracts/balance_constant.mligo"
-- example = "../../../src/test/contracts/closure.mligo"
-- example = "../../../src/test/contracts/counter.mligo"
-- example = "../../../src/test/contracts/condition.mligo"
-- example = "../../../src/test/contracts/crypto.mligo"
-- example = "../../../src/test/contracts/condition-annot.mligo"
-- example = "../../../src/test/contracts/curry.mligo"
-- example = "../../../src/test/contracts/condition-shadowing.mligo"
-- example = "../../../src/test/contracts/create_contract.mligo"
-- example = "../../../src/test/contracts/comparable.mligo"
-- example = "../../../src/test/contracts/check_signature.mligo"
-- example = "../../../src/test/contracts/double_michelson_or.mligo"
-- example = "../../../src/test/contracts/eq_bool.mligo"
-- example = "../../../src/test/contracts/empty_case.mligo"
-- example = "../../../src/test/contracts/fibo4.mligo"
-- example = "../../../src/test/contracts/fibo3.mligo"
-- example = "../../../src/test/contracts/fibo2.mligo"
-- example = "../../../src/test/contracts/fibo.mligo"
-- example = "../../../src/test/contracts/function-shared.mligo"
-- example = "../../../src/test/contracts/failwith.mligo"
-- example = "../../../src/test/contracts/guess_string.mligo"
-- example = "../../../src/test/contracts/high-order.mligo"
-- example = "../../../src/test/contracts/hashlock.mligo"
-- example = "../../../src/test/contracts/includer.mligo"
-- example = "../../../src/test/contracts/incr_decr.mligo"
-- example = "../../../src/test/contracts/issue-184-combs.mligo"
-- example = "../../../src/test/contracts/implicit.mligo"
-- example = "../../../src/test/contracts/included.mligo"
-- example = "../../../src/test/contracts/implicit_account.mligo"
-- example = "../../../src/test/contracts/interpret_test.mligo"
-- example = "../../../src/test/contracts/isnat.mligo"
-- example = "../../../src/test/contracts/id.mligo"
-- example = "../../../src/test/contracts/key_hash.mligo"
-- example = "../../../src/test/contracts/letin.mligo"
-- example = "../../../src/test/contracts/lambda.mligo"
-- example = "../../../src/test/contracts/let_multiple.mligo"
-- example = "../../../src/test/contracts/lambda2.mligo"
-- example = "../../../src/test/contracts/loop.mligo"
-- example = "../../../src/test/contracts/let_in_multi_bind.mligo"
-- example = "../../../src/test/contracts/fibo2.mligo"

recognise :: SomeRawTree -> ParserM (SomeLIGO Info)
recognise (SomeRawTree dialect rawTree)
  = fmap (SomeLIGO dialect)
  $ flip (descent (error "Camligo.recognise")) rawTree
  $ map usingScope
  [ -- Contract
    Descent do
      boilerplate $ \case
        "contract" -> RawContract <$> fields "declaration"
        _ -> fallthrough

  , Descent do
      boilerplate $ \case
        "fun_decl"  -> BFunction <$> flag "recursive" <*> field "name" <*> fields "arg" <*> fieldOpt "type" <*> field "body"
        "let_decl"  -> BConst    <$>                      field "name"                  <*> fieldOpt "type" <*> fieldOpt "body"
        "include"   -> BInclude  <$>                      field "filename"
        "type_decl" -> BTypeDecl <$> field "name" <*> field "type"
        _ -> fallthrough

  , Descent do
      boilerplate $ \case
        "let_expr1" -> Let <$> field "decl" <*> field "body"
        _ -> fallthrough

  --   -- Expr
  , Descent do
      boilerplate $ \case
        "fun_app"           -> Apply      <$> field  "f"         <*> fields "x"
        "rec_expr"          -> RecordUpd  <$> field  "subject"   <*> fields "field"
        "rec_literal"       -> Record     <$> fields "field"
        "if_expr"           -> If         <$> field  "condition" <*> field "then"  <*> fieldOpt "else"
        "match_expr"        -> Case       <$> field  "subject"   <*> fields "alt"
        "lambda_expr"       -> Lambda     <$> fields "arg"       <*> pure Nothing  <*> field "body"
        "list_expr"         -> List       <$> fields "item"
        "tup_expr"          -> Tuple      <$> fields "x"
        "paren_expr"        -> Tuple      <$> fields "expr"
        "block_expr"        -> Seq        <$> fields "item"
        "annot_expr"        -> Annot      <$> field  "expr"      <*> field "type"
        "binary_op_app"     -> BinOp      <$> field  "left"      <*> field "op"    <*> field "right"
        "unary_op_app"      -> UnOp       <$> field  "negate"    <*> field "arg"
        _                   -> fallthrough

    -- QualifiedName
  , Descent do
      boilerplate $ \case
        "data_projection" -> QualifiedName <$> field "box" <*> fields "selector"
        _ -> fallthrough


    -- Pattern
  , Descent do
      boilerplate $ \case
        "list_pattern"     -> IsList   <$> fields "item"
        "list_con_pattern" -> IsCons   <$> field  "x"    <*> field "xs"
        "tup_pattern"      -> IsTuple  <$> fields "item"
        "con_pattern"      -> IsConstr <$> field  "ctor" <*> fieldOpt "args"
        "annot_pattern"    -> IsAnnot  <$> field  "pat"  <*> field "type"
        "paren_pattern"    -> IsTuple  <$> fields "pat"
        "var_pattern"      -> IsVar    <$> field  "var"
        "_"                -> pure IsWildcard
        _                  -> fallthrough

    -- Alt
  , Descent do
      boilerplate $ \case
        "matching"  -> Alt <$> field "pattern" <*> field  "body"
        _           -> fallthrough

    -- Record fields
  , Descent do
      boilerplate $ \case
        "rec_assignment" -> FieldAssignment <$> field "field" <*> field "value"
        _                -> fallthrough

  , Descent do
      boilerplate' $ \case
        ("+", _)      -> return $ Op "+"
        ("-", _)      -> return $ Op "-"
        ("mod", _)    -> return $ Op "mod"
        ("/", _)      -> return $ Op "/"
        ("*", _)      -> return $ Op "*"
        ("^", _)      -> return $ Op "^"
        ("::", _)     -> return $ Op "::"
        (">", _)      -> return $ Op ">"
        ("<", _)      -> return $ Op "<"
        (">=", _)     -> return $ Op ">="
        ("<=", _)     -> return $ Op "<="
        ("=", _)      -> return $ Op "=="
        ("!=", _)     -> return $ Op "!="
        ("<>", _)     -> return $ Op "!="
        ("||", _)     -> return $ Op "||"
        ("&&", _)     -> return $ Op "&&"
        ("negate", n) -> return $ Op n
        _         -> fallthrough

    -- Literal
  , Descent do
      boilerplate' $ \case
        ("Int",    i) -> return $ Int i
        ("Nat",    i) -> return $ Nat i
        ("Bytes",  i) -> return $ Bytes i
        ("String", i) -> return $ String i
        ("Tez",    i) -> return $ Tez i
        _             -> fallthrough

    -- Name
  , Descent do
      boilerplate' $ \case
        ("Name", n) -> return $ Name n
        _           -> fallthrough

    -- NameDecl
  , Descent do
      boilerplate' $ \case
        ("NameDecl", n) -> return $ NameDecl n
        _               -> fallthrough

    -- FieldName
  , Descent do
      boilerplate' $ \case
        ("FieldName", n) -> return $ FieldName n
        _                -> fallthrough

    -- Type
  , Descent do
      boilerplate $ \case
        "type_fun"           -> TArrow   <$> field  "domain" <*> field "codomain"
        "type_app"           -> TApply   <$> field  "f"      <*> field "x"
        "type_product"       -> TProduct <$> fields "x"
        "type_tuple"         -> TProduct <$> fields "x"
        "type_rec"           -> TRecord  <$> fields "field"
        "type_sum"           -> TSum     <$> fields "variant"
        _                 -> fallthrough

    -- Variant
  , Descent do
      boilerplate $ \case
        "variant" -> Variant <$> field "constructor" <*> fieldOpt "type"
        _         -> fallthrough

    -- TField
  , Descent do
      boilerplate $ \case
        "type_rec_field" -> TField <$> field "field" <*> field "type"
        _                -> fallthrough

    -- TypeName
  , Descent do
      boilerplate' $ \case
        ("type_con", name) -> return $ TypeName name
        _                  -> fallthrough

    -- Ctor
  , Descent do
      boilerplate' $ \case
        ("Name_Capital", name) -> return $ Ctor name
        ("data_con",     name) -> return $ Ctor name
        ("False", _)           -> return $ Ctor "False"
        ("True", _)            -> return $ Ctor "True"
        ("Unit", _)            -> return $ Ctor "Unit"
        _                      -> fallthrough

  -- Err
  , Descent do
      \(r :> _, ParseTree _ children source) -> do
        withComments do
          return (r :> N :> CodeSource source :> Nil, Error source children)
  ]
