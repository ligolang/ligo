
module AST.Parser (example, contract) where

import Data.Text (Text)

import AST.Types hiding (tuple)

import Parser
import Range

import Debug.Trace

name :: Parser (Name ASTInfo)
name = ctor Name <*> token "Name"

capitalName :: Parser (Name ASTInfo)
capitalName = ctor Name <*> token "Name_Capital"

contract :: Parser (Contract ASTInfo)
contract =
  ctor Contract
  <*> subtree "contract" do
        many "declaration" do
          inside "declaration:" do
            declaration

declaration :: Parser (Declaration ASTInfo)
declaration
  =   do ctor ValueDecl <*> binding
  <|> do ctor ValueDecl <*> vardecl
  <|> do ctor ValueDecl <*> constdecl
  <|> typedecl
  <|> do ctor Action <*> attributes

typedecl :: Parser (Declaration ASTInfo)
typedecl = do
  subtree "type_decl" do
    ctor TypeDecl
      <*> inside "typeName:"  name
      <*> inside "typeValue:" newtype_

vardecl :: Parser (Binding ASTInfo)
vardecl = do
  subtree "var_decl" do
    ctor Var
      <*> inside "name:"  name
      <*> inside "type:"  type_
      <*> inside "value:" expr

constdecl :: Parser (Binding ASTInfo)
constdecl = do
  subtree "const_decl" do
    ctor Const
      <*> inside "name" name
      <*> inside "type" type_
      <*> inside "value" expr

binding :: Parser (Binding ASTInfo)
binding = do
  inside ":fun_decl" do
    ctor Function
      <*> recursive
      <*> inside "name:"  name
      <*> inside "parameters:parameters" do
            many "param" do
              notFollowedBy do
                consumeOrDie ")"

              stubbed "parameters" paramDecl
      <*> inside "type:" type_
      <*> inside "body:" letExpr

recursive = do
  mr <- optional do
    inside "recursive" do
      token "recursie"

  return $ maybe False (== "recursive") mr

expr :: Parser (Expr ASTInfo)
expr = stubbed "expr" do
  select
    [ ctor Ident <*> do
        ctor QualifiedName
          <*> name
          <*> pure []
    , opCall
    , fun_call
    , record_expr
    , int_literal
    , tez_literal
    , par_call
    , method_call
    , if_expr
    , assign
    , list_expr
    , has_type
    , string_literal
    , attributes
    , tuple_expr
    , moduleQualified
    , big_map_expr
    , map_expr
    , map_remove
    -- , constant
    ]
  where
  -- $.case_expr,
  -- $.cond_expr,
  -- $.disj_expr,
  -- $.fun_expr,

map_remove :: Parser (Expr ASTInfo)
map_remove = do
  subtree "map_remove" do
    ctor MapRemove
      <*> inside "key" expr
      <*> inside "container" do
        inside ":path" do
          qname

big_map_expr :: Parser (Expr ASTInfo)
big_map_expr = do
  subtree "big_map_injection" do
    ctor BigMap <*> do
      many "binding" do
        inside "binding" do
          map_binding

map_expr :: Parser (Expr ASTInfo)
map_expr = do
  subtree "map_injection" do
    ctor Map <*> do
      many "binding" do
        inside "binding" do
          map_binding

map_binding :: Parser (MapBinding ASTInfo)
map_binding = do
  subtree "binding" do
    ctor MapBinding
      <*> inside "key"   expr
      <*> inside "value" expr

moduleQualified :: Parser (Expr ASTInfo)
moduleQualified = do
  subtree "module_field" do
    ctor Ident <*> do
      ctor QualifiedName
        <*> inside "module" capitalName
        <*> do pure <$> do ctor At <*> inside "method" name

tuple_expr :: Parser (Expr ASTInfo)
tuple_expr = do
  subtree "tuple_expr" do
    ctor Tuple <*> do
      many "tuple element" do
        inside "element" expr

attributes :: Parser (Expr ASTInfo)
attributes = do
  subtree "attr_decl" do
    ctor Attrs <*> do
      many "attribute" do
        inside "attribute" do
          token "String"

string_literal :: Parser (Expr ASTInfo)
string_literal = do
  ctor Constant <*> do
    ctor String <*>
      token "String"

has_type :: Parser (Expr ASTInfo)
has_type = do
  subtree "annot_expr" do
    ctor Annot
      <*> inside "subject" expr
      <*> inside "type"    type_

list_expr :: Parser (Expr ASTInfo)
list_expr = do
  subtree "list_expr" do
    ctor List <*> do
      many "list elem" do
        inside "element" expr

qname :: Parser (QualifiedName ASTInfo)
qname = do
  ctor QualifiedName
    <*> name
    <*> pure []

assign :: Parser (Expr ASTInfo)
assign = do
  subtree "assignment" do
    ctor Assign
      <*> inside "LHS" lhs
      <*> inside "RHS" expr

lhs :: Parser (LHS ASTInfo)
lhs =
  do ctor LHS
       <*> inside "container:path" do
             qname <|> projection
       <*> pure Nothing
  <|>
  do ctor LHS
       <*> subtree "path" do
             qname <|> projection
       <*> pure Nothing
  <|>
  do subtree "map_lookup" do
       ctor LHS
         <*> inside "container:path" do
               qname <|> projection
         <*> inside "index" do
               Just <$> expr


tez_literal :: Parser (Expr ASTInfo)
tez_literal = do
  ctor Constant <*> do
    ctor Tez <*> token "Tez"

if_expr :: Parser (Expr ASTInfo)
if_expr = do
  subtree "conditional" do
    ctor If
      <*> inside "selector"       expr
      <*> inside "then:if_clause" expr
      <*> inside "else:if_clause" expr

method_call :: Parser (Expr ASTInfo)
method_call = do
  subtree "projection_call" do
    ctor Apply
      <*> do ctor Ident <*> field "f" projection
      <*> inside "arguments" arguments

projection :: Parser (QualifiedName ASTInfo)
projection = do
  gets pfGrove >>= traceShowM
  subtree "data_projection" do
    ctor QualifiedName
      <*> inside "struct" name
      <*> many "selection" selection

selection :: Parser (Path ASTInfo)
selection = do
    inside "index:selection"
      $   do ctor At <*> name
      <|> do ctor Ix <*> token "Int"
  <|>
    inside "index" do
      do ctor Ix <*> token "Int"

par_call :: Parser (Expr ASTInfo)
par_call = do
  subtree "par_call" do
    ctor Apply
      <*> inside "f" expr
      <*> inside "arguments" arguments

int_literal :: Parser (Expr ASTInfo)
int_literal = do
  ctor Constant
    <*> do ctor Int <*> token "Int"

record_expr :: Parser (Expr ASTInfo)
record_expr = do
  subtree "record_expr" do
    ctor Record <*> do
      many "assignment" do
        inside "assignment:field_assignment" do
          ctor Assignment
            <*> inside "name" name
            <*> inside "_rhs" expr

fun_call :: Parser (Expr ASTInfo)
fun_call = do
  subtree "fun_call" do
    ctor Apply
      <*> do ctor Ident <*> inside "f" function_id
      <*> inside "arguments" arguments

arguments =
  subtree "arguments" do
    many "argument" do
      inside "argument" expr

function_id :: Parser (QualifiedName ASTInfo)
function_id = select
  [ qname
  , do
      subtree "module_field" do
        ctor QualifiedName
          <*> inside "module" capitalName
          <*> do pure <$> do ctor At <*> inside "method" name
  ]

opCall :: Parser (Expr ASTInfo)
opCall = do
  subtree "op_expr"
    $   do inside "the" expr
    <|> do ctor BinOp
             <*> inside "arg1" expr
             <*> inside "op"   anything
             <*> inside "arg2" expr
    <|> do ctor UnOp
             <*> inside "negate" anything
             <*> inside "arg"    expr

letExpr = do
  subtree "let_expr" do
    ctor let'
      <*> optional do
        inside "locals:block" do
          many "decl" do
            inside "statement" do
              declaration <|> statement
      <*> inside "body"expr

  where
    let' r decls body = case decls of
      Just them -> Let r them body
      Nothing   -> body

statement :: Parser (Declaration ASTInfo)
statement = ctor Action <*> expr

paramDecl :: Parser (VarDecl ASTInfo)
paramDecl = do
  info <- getRange
  inside "parameter:param_decl" do
    ctor Decl
      <*> do inside ":access" do
              select
                [ ctor Mutable   <* consumeOrDie "var"
                , ctor Immutable <* consumeOrDie "const"
                ]
     <*> inside "name" name
     <*> inside "type" type_

newtype_ = select
  [ record_type
  , type_
  -- , sum_type
  ]

record_type = do
  subtree "record_type" do
    ctor TRecord
      <*> many "field" do
        inside "field" do
          field_decl

field_decl = do
  subtree "field_decl" do
    ctor TField
      <*> inside "fieldName" name
      <*> inside "fieldType" type_

type_ :: Parser (Type ASTInfo)
type_ =
    fun_type
  where
    fun_type :: Parser (Type ASTInfo)
    fun_type = do
      inside ":fun_type" do
        ctor tarrow
          <*>             inside "domain"  cartesian
          <*> optional do inside "codomain" fun_type

      where
        tarrow info domain codomain =
          case codomain of
            Just co -> TArrow info domain co
            Nothing -> domain

    cartesian = do
      inside ":cartesian" do
        ctor TProduct <*> some "corety" do
          inside "element" do
            core_type

    core_type = do
      select
        [ ctor TVar <*> name
        , subtree "invokeBinary" do
            ctor TApply
              <*> inside "typeConstr" name'
              <*> inside "arguments"  typeTuple
        , subtree "invokeUnary" do
            ctor TApply
              <*> inside "typeConstr" name'
              <*> do pure <$> inside "arguments" type_
        ]

name' :: Parser (Name ASTInfo)
name' = do
  ctor Name <*> anything

typeTuple :: Parser [Type ASTInfo]
typeTuple = do
  subtree "type_tuple" do
    many "type tuple element" do
      inside "element" type_

-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/address.ligo"
-- example = "../../../src/test/contracts/amount.ligo"
-- example = "../../../src/test/contracts/annotation.ligo"
-- example = "../../../src/test/contracts/arithmetic.ligo"
-- example = "../../../src/test/contracts/assign.ligo"
-- example = "../../../src/test/contracts/attributes.ligo"
-- example = "../../../src/test/contracts/bad_timestamp.ligo"
-- example = "../../../src/test/contracts/bad_type_operator.ligo"
-- example = "../../../src/test/contracts/balance_constant.ligo"
example = "../../../src/test/contracts/big_map.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
