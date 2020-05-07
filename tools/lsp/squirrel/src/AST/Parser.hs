
{-
  Parser for a contract. The `example` is exported to run on current debug target.

  TODO: prune some "path" and alike stuff from grammar, refactor common things.

  TODO: break <*>/do ladders onto separate named parsers.
-}

module AST.Parser (example, contract) where

import Data.Text (Text)

import AST.Types hiding (tuple)

import Parser
import Range

import Debug.Trace

contract :: Parser (Contract ASTInfo)
contract =
  ctor Contract
  <*> subtree "contract" do
        many "declaration" do
          inside "declaration:" do
            declaration

name :: Parser (Name ASTInfo)
name = ctor Name <*> token "Name"

capitalName :: Parser (Name ASTInfo)
capitalName = ctor Name <*> token "Name_Capital"

declaration :: Parser (Declaration ASTInfo)
declaration
  =   do ctor ValueDecl <*> binding
  <|> do ctor ValueDecl <*> vardecl
  <|> do ctor ValueDecl <*> constdecl
  <|> do typedecl
  <|> do ctor Action <*> attributes
  <|> do include

include = do
  subtree "include" do
    ctor Include
      <*> inside "filename" do token "String"

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
      <*> inside "name"  name
      <*> inside "type"  type_
      <*> inside "value" expr

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
              inside "parameter" paramDecl
      <*> inside "type:" type_
      <*> inside "body:" letExpr

recursive = do
  mr <- optional do
    inside "recursive" do
      token "recursive"

  return $ maybe False (== "recursive") mr

expr :: Parser (Expr ASTInfo)
expr = stubbed "expr" do
  select
    [ -- Wait, isn't it `qname`? TODO: replace.
      ctor Ident <*> do
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
    , indexing
    , constr_call
    , nat_literal
    , nullary_ctor
    , bytes_literal
    , case_expr
    , skip
    , case_action
    , clause_block
    , loop
    , seq_expr
    , lambda_expr
    , set_expr
    , map_patch
    , record_update
    , set_patch
    , set_remove
    ]

set_remove :: Parser (Expr ASTInfo)
set_remove = do
  subtree "set_remove" do
    ctor SetRemove
      <*> inside "key" expr
      <*> inside "container" do
        inside ":path" do
          qname <|> projection

set_patch = do
  subtree "set_patch" do
    ctor SetPatch
      <*> inside "container:path" (qname <|> projection)
      <*> many "key" do
            inside "key" expr

record_update = do
  subtree "update_record" do
    ctor RecordUpd
      <*> inside "record:path" do qname <|> projection
      <*> many "field" do
            inside "assignment" field_path_assignment

field_path_assignment = do
  subtree "field_path_assignment" do
    ctor FieldAssignment
      <*> inside "lhs:path" do qname <|> projection
      <*> inside "_rhs" expr

map_patch = do
  subtree "map_patch" do
    ctor MapPatch
      <*> inside "container:path" (qname <|> projection)
      <*> many "binding" do
            inside "binding" map_binding

set_expr :: Parser (Expr ASTInfo)
set_expr = do
  subtree "set_expr" do
    ctor List <*> do
      many "list elem" do
        inside "element" expr

lambda_expr = do
  subtree "fun_expr" do
    ctor Lambda
      <*> inside "parameters:parameters" do
            many "param" do
              inside "parameter" paramDecl
      <*> inside "type" newtype_
      <*> inside "body" expr

seq_expr = do
  subtree "block" do
    ctor Seq <*> do
      many "statement" do
        inside "statement" do
          declaration <|> statement

loop = do
  subtree "loop" do
    for_loop <|> while_loop <|> for_container

for_container = do
  subtree "for_loop" do
    ctor ForBox
      <*> inside "key" name
      <*> optional do inside "value" name
      <*> inside "kind" anything
      <*> inside "collection" expr
      <*> inside "body" (expr <|> seq_expr)

while_loop = do
  subtree "while_loop" do
    ctor WhileLoop
      <*> inside "breaker" expr
      <*> inside "body"    expr

for_loop = do
  subtree "for_loop" do
    ctor ForLoop
      <*> inside "name"  name
      <*> inside "begin" expr
      <*> inside "end"   expr
      <*> inside "body"  expr

clause_block = do
    subtree "clause_block" do
      inside "block:block" do
        ctor Seq <*> many "statement" do
          inside "statement" (declaration <|> statement)
  <|> do
    subtree "clause_block" do
      ctor Seq <*> many "statement" do
        inside "statement" (declaration <|> statement)

skip :: Parser (Expr ASTInfo)
skip = do
  ctor Skip <* token "skip"

case_action :: Parser (Expr ASTInfo)
case_action = do
  subtree "case_instr" do
    ctor Case
      <*> inside "subject" expr
      <*> many "case" do
            inside "case" alt_action

alt_action :: Parser (Alt ASTInfo)
alt_action = do
  subtree "case_clause_instr" do
    ctor Alt
      <*> inside "pattern"        pattern
      <*> inside "body:if_clause" expr

case_expr :: Parser (Expr ASTInfo)
case_expr = do
  subtree "case_expr" do
    ctor Case
      <*> inside "subject" expr
      <*> many "case" do
            inside "case" alt

alt :: Parser (Alt ASTInfo)
alt = do
  subtree "case_clause_expr" do
    ctor Alt
      <*> inside "pattern" pattern
      <*> inside "body"    expr

pattern :: Parser (Pattern ASTInfo)
pattern = do
  subtree "pattern" $ do
      inside "the" core_pattern
    <|>
      do ctor IsCons
           <*> inside "head" core_pattern
           <*> inside "tail" pattern

core_pattern :: Parser (Pattern ASTInfo)
core_pattern
  =   constr_pattern
  <|> string_pattern
  <|> int_pattern
  <|> nat_pattern
  <|> tuple_pattern
  <|> list_pattern
  <|> some_pattern
  <|> var_pattern

var_pattern :: Parser (Pattern ASTInfo)
var_pattern =
  ctor IsVar <*> name

some_pattern :: Parser (Pattern ASTInfo)
some_pattern = do
  subtree "Some_pattern" do
    ctor IsConstr
      <*> do inside "constr" do ctor Name <*> token "Some"
      <*> do Just <$> inside "arg" pattern

string_pattern :: Parser (Pattern ASTInfo)
string_pattern =
  ctor IsConstant <*> do
    ctor String <*> token "String"

nat_pattern :: Parser (Pattern ASTInfo)
nat_pattern =
  ctor IsConstant <*> do
    ctor Nat <*> token "Nat"

int_pattern :: Parser (Pattern ASTInfo)
int_pattern =
  ctor IsConstant <*> do
    ctor Int <*> token "Int"

constr_pattern :: Parser (Pattern ASTInfo)
constr_pattern =
    do
      subtree "user_constr_pattern" do
        ctor IsConstr
          <*> inside "constr:constr" capitalName
          <*> optional do
                inside "arguments" tuple_pattern
  <|>
    do
      ctor IsConstr
         <*> do ctor Name <*> do token "True" <|> token "False" <|> token "None" <|> token "Unit"
         <*> pure Nothing

tuple_pattern :: Parser (Pattern ASTInfo)
tuple_pattern = do
  subtree "tuple_pattern" do
    ctor IsTuple <*> do
      many "element" do
        inside "element" pattern

list_pattern :: Parser (Pattern ASTInfo)
list_pattern = do
  subtree "list_pattern" do
    ctor IsList <*> do
      many "element" do
        inside "element" pattern

nullary_ctor :: Parser (Expr ASTInfo)
nullary_ctor = do
  ctor Ident <*> do
    ctor QualifiedName
      <*> do ctor Name <*> do
               true <|> false <|> none <|> unit
      <*> pure []
  where
    true  = token "True"
    false = token "False"
    none  = token "None"
    unit  = token "Unit"

nat_literal :: Parser (Expr ASTInfo)
nat_literal = do
  ctor Constant <*> do
    ctor Nat <*> token "Nat"

bytes_literal :: Parser (Expr ASTInfo)
bytes_literal = do
  ctor Constant <*> do
    ctor Bytes <*> token "Bytes"

constr_call :: Parser (Expr ASTInfo)
constr_call = do
  some_call <|> user_constr_call
  where
    some_call = do
      subtree "Some_call" do
        ctor Apply
          <*> do ctor Ident <*> inside "constr" qname'
          <*> inside "arguments:arguments" do
            many "argument" do
              inside "argument" expr

    user_constr_call = do
      subtree "constr_call" do
        ctor Apply
          <*> inside "constr:constr" do
                ctor Ident <*> do
                   ctor QualifiedName
                     <*> capitalName
                     <*> pure []
          <*> inside "arguments:arguments" do
            many "argument" do
              inside "argument" expr

indexing :: Parser (Expr ASTInfo)
indexing = do
  subtree "map_lookup" do
    ctor Indexing
      <*> inside "container:path" do
            qname <|> projection
      <*> inside "index" expr

map_remove :: Parser (Expr ASTInfo)
map_remove = do
  subtree "map_remove" do
    ctor MapRemove
      <*> inside "key" expr
      <*> inside "container" do
        inside ":path" do
          qname <|> projection

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
        <*> do pure <$> do ctor At <*> inside "method" do name <|> name'

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

qname' :: Parser (QualifiedName ASTInfo)
qname' = do
  ctor QualifiedName
    <*> name'
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
  <|> do
    subtree "cond_expr" do
      ctor If
        <*> inside "selector"       expr
        <*> inside "then" expr
        <*> inside "else" expr

method_call :: Parser (Expr ASTInfo)
method_call = do
  subtree "projection_call" do
    ctor apply'
      <*> field "f" projection
      <*> optional do inside "arguments" arguments
  where
    apply' r f (Just xs) = Apply r (Ident r f) xs
    apply' r f _         = Ident r f

projection :: Parser (QualifiedName ASTInfo)
projection = do
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
    ctor apply'
      <*> inside "f" expr
      <*> optional do inside "arguments" arguments
  where
    apply' r f (Just xs) = Apply r f xs
    apply' _ f  _        = f

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
          <*> do pure <$> do ctor At <*> inside "method" do name <|> name'
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
  subtree "param_decl" do
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
  , sum_type
  ]

sum_type = do
  subtree "sum_type" do
    ctor TSum <*> do
      many "variant" do
        inside "variant" variant

variant = do
  subtree "variant" do
    ctor Variant
      <*> inside "constructor:constr" capitalName
      <*> optional do inside "arguments" type_

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
      <*> inside "fieldType" newtype_

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

        , subtree "type_expr" newtype_
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
-- example = "../../../src/test/contracts/big_map.ligo"
-- example = "../../../src/test/contracts/bitwise_arithmetic.ligo"
-- example = "../../../src/test/contracts/blockless.ligo"
-- example = "../../../src/test/contracts/boolean_operators.ligo"
-- example = "../../../src/test/contracts/bytes_arithmetic.ligo"
-- example = "../../../src/test/contracts/bytes_unpack.ligo"
-- example = "../../../src/test/contracts/chain_id.ligo"
-- example = "../../../src/test/contracts/coase.ligo"
example = "../../../src/test/contracts/failwith.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
