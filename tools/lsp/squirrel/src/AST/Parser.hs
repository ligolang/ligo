
{- | Parser for a contract.
-}

module AST.Parser (example, contract) where

import Data.Text (Text)
import Data.Sum

import AST.Types

import Parser
import Tree hiding (skip)

-- import Debug.Trace

ranged
  :: ( Functor f
     , Element f fs
     )
  => Parser (f (Tree fs ASTInfo))
  -> Parser    (Tree fs ASTInfo)
ranged p = do
  r <- getInfo
  a <- p
  return $ mk r a

-- | The entrypoint.
contract :: Parser (Pascal ASTInfo)
contract =
  ranged do
    pure Contract
      <*> subtree "contract" do
            many do
              inside "declaration:" do
                declaration

name :: Parser (Pascal ASTInfo)
name = ranged do pure Name <*> token "Name"

capitalName :: Parser (Pascal ASTInfo)
capitalName = ranged do pure Name <*> token "Name_Capital"

declaration :: Parser (Pascal ASTInfo)
declaration
  =   do ranged do pure ValueDecl <*> binding
  <|> do ranged do pure ValueDecl <*> vardecl
  <|> do ranged do pure ValueDecl <*> constdecl
  <|> do ranged do pure Action    <*> attributes
  <|> do typedecl
  <|> do include

include :: Parser (Pascal ASTInfo)
include = do
  subtree "include" do
    ranged do
      pure Include
        <*> inside "filename" do
              token "String"

typedecl :: Parser (Pascal ASTInfo)
typedecl = do
  subtree "type_decl" do
    ranged do
      pure TypeDecl
        <*> inside "typeName:"  name
        <*> inside "typeValue:" newtype_

vardecl :: Parser (Pascal ASTInfo)
vardecl = do
  subtree "var_decl" do
    ranged do
      pure Var
        <*> inside "name"  name
        <*> inside "type"  type_
        <*> inside "value" expr

constdecl :: Parser (Pascal ASTInfo)
constdecl = do
  subtree "const_decl" do
    ranged do
      pure Const
        <*> inside "name" name
        <*> inside "type" type_
        <*> inside "value" expr

binding :: Parser (Pascal ASTInfo)
binding = do
  inside ":fun_decl" do
    ranged do
      pure Function
        <*> recursive
        <*> inside "name:"  name
        <*> inside "parameters:parameters" do
              many do
                inside "parameter" paramDecl
        <*> inside "type:" type_
        <*> inside "body:" letExpr

recursive :: Parser Bool
recursive = do
  mr <- optional do
    inside "recursive" do
      token "recursive"

  return $ maybe False (== "recursive") mr

expr :: Parser (Pascal ASTInfo)
expr = stubbed "expr" do
  select
    [ -- Wait, isn't it `qname`? TODO: replace.
      ranged do
        pure Ident <*> do
          ranged do
            pure QualifiedName
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

set_remove :: Parser (Pascal ASTInfo)
set_remove = do
  subtree "set_remove" do
    ranged do
      pure SetRemove
        <*> inside "key" expr
        <*> inside "container" do
          inside ":path" do
            qname <|> projection

set_patch :: Parser (Pascal ASTInfo)
set_patch = do
  subtree "set_patch" do
    ranged do
      pure SetPatch
        <*> inside "container:path" (qname <|> projection)
        <*> many do inside "key" expr

record_update :: Parser (Pascal ASTInfo)
record_update = do
  subtree "update_record" do
    ranged do
      pure RecordUpd
        <*> inside "record:path" do qname <|> projection
        <*> many do inside "assignment" field_path_assignment

field_path_assignment :: Parser (Pascal ASTInfo)
field_path_assignment = do
  subtree "field_path_assignment" do
    ranged do
      pure FieldAssignment
        <*> inside "lhs:path" do qname <|> projection
        <*> inside "_rhs" expr

map_patch :: Parser (Pascal ASTInfo)
map_patch = do
  subtree "map_patch" do
    ranged do
      pure MapPatch
        <*> inside "container:path" (qname <|> projection)
        <*> many do inside "binding" map_binding

set_expr :: Parser (Pascal ASTInfo)
set_expr = do
  subtree "set_expr" do
    ranged do
      pure List <*> many do
        inside "element" expr

lambda_expr :: Parser (Pascal ASTInfo)
lambda_expr = do
  subtree "fun_expr" do
    ranged do
      pure Lambda
        <*> inside "parameters:parameters" do
              many do inside "parameter" paramDecl
        <*> inside "type" newtype_
        <*> inside "body" expr

seq_expr :: Parser (Pascal ASTInfo)
seq_expr = do
  subtree "block" do
    ranged do
      pure Seq <*> many do
        inside "statement" do
          declaration <|> statement

loop :: Parser (Pascal ASTInfo)
loop = do
  subtree "loop" do
    for_loop <|> while_loop <|> for_container

for_container :: Parser (Pascal ASTInfo)
for_container = do
  subtree "for_loop" do
    ranged do
      pure ForBox
        <*> inside "key" name
        <*> optional do inside "value" name
        <*> inside "kind" anything
        <*> inside "collection" expr
        <*> inside "body" (expr <|> seq_expr)

while_loop :: Parser (Pascal ASTInfo)
while_loop = do
  subtree "while_loop" do
    ranged do
      pure WhileLoop
        <*> inside "breaker" expr
        <*> inside "body"    expr

for_loop :: Parser (Pascal ASTInfo)
for_loop = do
  subtree "for_loop" do
    ranged do
      pure ForLoop
        <*> inside "name"  name
        <*> inside "begin" expr
        <*> inside "end"   expr
        <*> inside "body"  expr

clause_block :: Parser (Pascal ASTInfo)
clause_block = do
    subtree "clause_block" do
      inside "block:block" do
        ranged do
          pure Seq <*> many do
            inside "statement" (declaration <|> statement)
  <|> do
    subtree "clause_block" do
      ranged do
        pure Seq <*> many do
          inside "statement" (declaration <|> statement)

skip :: Parser (Pascal ASTInfo)
skip = do
  ranged do
    pure Skip
      <* token "skip"

case_action :: Parser (Pascal ASTInfo)
case_action = do
  subtree "case_instr" do
    ranged do
      pure Case
        <*> inside "subject" expr
        <*> many do
              inside "case" alt_action

alt_action :: Parser (Pascal ASTInfo)
alt_action = do
  subtree "case_clause_instr" do
    ranged do
      pure Alt
        <*> inside "pattern"        pattern
        <*> inside "body:if_clause" expr

case_expr :: Parser (Pascal ASTInfo)
case_expr = do
  subtree "case_expr" do
    ranged do
      pure Case
        <*> inside "subject" expr
        <*> many do
              inside "case" alt

alt :: Parser (Pascal ASTInfo)
alt = do
  subtree "case_clause_expr" do
    ranged do
      pure Alt
        <*> inside "pattern" pattern
        <*> inside "body"    expr

pattern :: Parser (Pascal ASTInfo)
pattern = do
  subtree "pattern" $ do
      inside "the" core_pattern
    <|>
      do ranged do
           pure IsCons
             <*> inside "head" core_pattern
             <*> inside "tail" pattern

core_pattern :: Parser (Pascal ASTInfo)
core_pattern
  =   constr_pattern
  <|> string_pattern
  <|> int_pattern
  <|> nat_pattern
  <|> tuple_pattern
  <|> list_pattern
  <|> some_pattern
  <|> var_pattern

var_pattern :: Parser (Pascal ASTInfo)
var_pattern =
  ranged do
    pure IsVar <*> name

some_pattern :: Parser (Pascal ASTInfo)
some_pattern = do
  subtree "Some_pattern" do
    ranged do
      pure IsConstr
        <*> inside "constr" do
              ranged do
                pure Name <*> token "Some"

        <*> do Just <$> inside "arg" pattern

string_pattern :: Parser (Pascal ASTInfo)
string_pattern =
  ranged do
    pure IsConstant <*> do
      ranged do
        pure String <*> token "String"

nat_pattern :: Parser (Pascal ASTInfo)
nat_pattern =
  ranged do
      pure IsConstant <*> do
        ranged do
          pure Nat <*> token "Nat"

int_pattern :: Parser (Pascal ASTInfo)
int_pattern =
  ranged do
    pure IsConstant <*> do
      ranged do
        pure Int <*> token "Int"

constr_pattern :: Parser (Pascal ASTInfo)
constr_pattern =
    do
      subtree "user_constr_pattern" do
        ranged do
          pure IsConstr
            <*> inside "constr:constr" capitalName
            <*> optional do
                  inside "arguments" tuple_pattern
  <|>
    do
      ranged do
        pure IsConstr
          <*> ranged do
                pure Name <*> do
                  true <|> false <|> none <|> unit
          <*> pure Nothing

tuple_pattern :: Parser (Pascal ASTInfo)
tuple_pattern = do
  subtree "tuple_pattern" do
    ranged do
      pure IsTuple <*> many do
        inside "element" pattern

list_pattern :: Parser (Pascal ASTInfo)
list_pattern = do
  subtree "list_pattern" do
    ranged do
      pure IsList <*> many do
        inside "element" pattern

nullary_ctor :: Parser (Pascal ASTInfo)
nullary_ctor = do
  ranged do
    pure Ident <*> do
      ranged do
        pure QualifiedName
          <*> ranged do
                pure Name <*> do
                   true <|> false <|> none <|> unit
          <*> pure []

true, false, none, unit :: Parser Text
true  = token "True"
false = token "False"
none  = token "None"
unit  = token "Unit"

nat_literal :: Parser (Pascal ASTInfo)
nat_literal = do
  ranged do
    pure Constant <*> do
      ranged do
        pure Nat <*> token "Nat"

bytes_literal :: Parser (Pascal ASTInfo)
bytes_literal = do
  ranged do
    pure Constant <*> do
      ranged do
        pure Bytes <*> token "Bytes"

constr_call :: Parser (Pascal ASTInfo)
constr_call = do
  some_call <|> user_constr_call
  where
    some_call = do
      subtree "Some_call" do
        ranged do
          pure Apply
            <*> ranged do
                  pure Ident <*> inside "constr" qname'
            <*> inside "arguments:arguments" do
              many do inside "argument" expr

    user_constr_call = do
      subtree "constr_call" do
        ranged do
          pure Apply
            <*> inside "constr:constr" do
                  ranged do
                    pure Ident <*> do
                      ranged do
                        pure QualifiedName
                          <*> capitalName
                          <*> pure []
            <*> inside "arguments:arguments" do
                  many do
                    inside "argument" expr

indexing :: Parser (Pascal ASTInfo)
indexing = do
  subtree "map_lookup" do
    ranged do
      pure Indexing
        <*> inside "container:path" do
              qname <|> projection
        <*> inside "index" expr

map_remove :: Parser (Pascal ASTInfo)
map_remove = do
  subtree "map_remove" do
    ranged do
      pure MapRemove
        <*> inside "key" expr
        <*> inside "container" do
          inside ":path" do
            qname <|> projection

big_map_expr :: Parser (Pascal ASTInfo)
big_map_expr = do
  subtree "big_map_injection" do
    ranged do
      pure BigMap <*> many do
        inside "binding" do
          map_binding

map_expr :: Parser (Pascal ASTInfo)
map_expr = do
  subtree "map_injection" do
    ranged do
      pure Map <*> many do
        inside "binding" do
          map_binding

map_binding :: Parser (Pascal ASTInfo)
map_binding = do
  subtree "binding" do
    ranged do
      pure MapBinding
        <*> inside "key"   expr
        <*> inside "value" expr

moduleQualified :: Parser (Pascal ASTInfo)
moduleQualified = do
  subtree "module_field" do
    ranged do
      pure Ident <*> do
        ranged do
          pure QualifiedName
            <*> inside "module" capitalName
            <*> do pure <$> ranged do
                    pure At <*> inside "method" do name <|> name'

tuple_expr :: Parser (Pascal ASTInfo)
tuple_expr = do
  subtree "tuple_expr" do
    ranged do
      pure Tuple <*> many do
        inside "element" expr

attributes :: Parser (Pascal ASTInfo)
attributes = do
  subtree "attr_decl" do
    ranged do
      pure Attrs <*> many do
        inside "attribute" do
          token "String"

string_literal :: Parser (Pascal ASTInfo)
string_literal = do
  ranged do
    pure Constant <*> do
      ranged do
        pure String <*> do
          token "String"

has_type :: Parser (Pascal ASTInfo)
has_type = do
  subtree "annot_expr" do
    ranged do
      pure Annot
        <*> inside "subject" expr
        <*> inside "type"    type_

list_expr :: Parser (Pascal ASTInfo)
list_expr = do
  subtree "list_expr" do
    ranged do
      pure List <*> many do
        inside "element" expr

qname :: Parser (Pascal ASTInfo)
qname = do
  ranged do
    pure QualifiedName
      <*> name
      <*> pure []

qname' :: Parser (Pascal ASTInfo)
qname' = do
  ranged do
    pure QualifiedName
      <*> name'
      <*> pure []

assign :: Parser (Pascal ASTInfo)
assign = do
  subtree "assignment" do
    ranged do
      pure Assign
        <*> inside "LHS" lhs
        <*> inside "RHS" expr

lhs :: Parser (Pascal ASTInfo)
lhs =
  ranged do
    pure LHS
      <*> inside "container:path" do
            qname <|> projection
      <*> pure Nothing
  <|>
  ranged do
    pure LHS
      <*> subtree "path" do
            qname <|> projection
      <*> pure Nothing
  <|>
  subtree "map_lookup" do
    ranged do
      pure LHS
        <*> inside "container:path" do
              qname <|> projection
        <*> inside "index" do
              Just <$> expr


tez_literal :: Parser (Pascal ASTInfo)
tez_literal = do
  ranged do
    pure Constant <*> do
      ranged do
        pure Tez <*> token "Tez"

if_expr :: Parser (Pascal ASTInfo)
if_expr = do
    subtree "conditional" do
      ranged do
        pure If
          <*> inside "selector"       expr
          <*> inside "then:if_clause" expr
          <*> inside "else:if_clause" expr
  <|> do
    subtree "cond_expr" do
      ranged do
        pure If
          <*> inside "selector"       expr
          <*> inside "then" expr
          <*> inside "else" expr

method_call :: Parser (Pascal ASTInfo)
method_call = do
  subtree "projection_call" do
    ranged do
      pure apply'
        <*> getInfo
        <*> inside "f" projection
        <*> optional do inside "arguments" arguments
  where
    apply' i f (Just xs) = Apply (mk i $ Ident f) xs
    apply' _ f _         = Ident f

projection :: Parser (Pascal ASTInfo)
projection = do
  subtree "data_projection" do
    ranged do
      pure QualifiedName
        <*> inside "struct" name
        <*> many selection

selection :: Parser (Pascal ASTInfo)
selection = do
    inside "index:selection"
      $   ranged do pure At <*> name
      <|> ranged do pure Ix <*> token "Int"
  <|>
    inside "index" do
      ranged do pure Ix <*> token "Int"

par_call :: Parser (Pascal ASTInfo)
par_call = do
  subtree "par_call" do
    pure apply'
      <*> getInfo
      <*> inside "f" expr
      <*> optional do inside "arguments" arguments
  where
    apply'
      :: ASTInfo
      -> Pascal ASTInfo
      -> Maybe [Pascal ASTInfo]
      -> Pascal ASTInfo
    apply' i f (Just xs) = mk i $ Apply f xs
    apply' _ f  _        = f

int_literal :: Parser (Pascal ASTInfo)
int_literal = do
  ranged do
    pure Constant
      <*> ranged do
        pure Int <*> token "Int"

record_expr :: Parser (Pascal ASTInfo)
record_expr = do
  subtree "record_expr" do
    ranged do
      pure Record <*> many do
        inside "assignment:field_assignment" do
          ranged do
            pure Assignment
              <*> inside "name" name
              <*> inside "_rhs" expr

fun_call :: Parser (Pascal ASTInfo)
fun_call = do
  subtree "fun_call" do
    ranged do
      pure Apply
        <*> ranged do pure Ident <*> inside "f" function_id
        <*> inside "arguments" arguments

arguments :: Parser [Pascal ASTInfo]
arguments =
  subtree "arguments" do
    many do inside "argument" expr

function_id :: Parser (Pascal ASTInfo)
function_id = select
  [ qname
  , do
      subtree "module_field" do
        ranged do
          pure QualifiedName
            <*> inside "module" capitalName
            <*> do pure <$> ranged do
                    pure At <*> inside "method" do name <|> name'
  ]

opCall :: Parser (Pascal ASTInfo)
opCall = do
  subtree "op_expr"
    $   do inside "the" expr
    <|> ranged do
          pure BinOp
            <*> inside "arg1" expr
            <*> inside "op"   anything
            <*> inside "arg2" expr
    <|> ranged do
          pure UnOp
            <*> inside "negate" anything
            <*> inside "arg"    expr

letExpr :: Parser (Pascal ASTInfo)
letExpr = do
  subtree "let_expr" do
    pure let'
      <*> getInfo
      <*> optional do
        inside "locals:block" do
          many do
            inside "statement" do
              declaration <|> statement
      <*> inside "body"expr
  where
    let' r decls body = case decls of
      Just them -> mk r $ Let them body
      Nothing   -> body

statement :: Parser (Pascal ASTInfo)
statement = ranged do pure Action <*> expr

paramDecl :: Parser (Pascal ASTInfo)
paramDecl = do
  subtree "param_decl" do
    ranged do
      pure Decl
        <*> inside "access" do
              ranged do
                access' =<< anything
        <*> inside "name" name
        <*> inside "type" type_
  where
    access' "var"   = pure Mutable
    access' "const" = pure Immutable
    access' _       = die  "`var` or `const`"

newtype_ :: Parser (Pascal ASTInfo)
newtype_ = select
  [ record_type
  , type_
  , sum_type
  ]

sum_type :: Parser (Pascal ASTInfo)
sum_type = do
  subtree "sum_type" do
    ranged do
      pure TSum <*> many do
        inside "variant" variant

variant :: Parser (Pascal ASTInfo)
variant = do
  subtree "variant" do
    ranged do
      pure Variant
        <*> inside "constructor:constr" capitalName
        <*> optional do inside "arguments" type_

record_type :: Parser (Pascal ASTInfo)
record_type = do
  subtree "record_type" do
    ranged do
      pure TRecord <*> many do
        inside "field" do
          field_decl

field_decl :: Parser (Pascal ASTInfo)
field_decl = do
  subtree "field_decl" do
    ranged do
      pure TField
        <*> inside "fieldName" name
        <*> inside "fieldType" newtype_

type_ :: Parser (Pascal ASTInfo)
type_ =
    fun_type
  where
    fun_type :: Parser (Pascal ASTInfo)
    fun_type = do
      inside ":fun_type" do
        pure tarrow
          <*> getInfo
          <*>             inside "domain"  cartesian
          <*> optional do inside "codomain" fun_type

      where
        tarrow i domain codomain =
          case codomain of
            Just co -> mk i $ TArrow domain co
            Nothing -> domain

    cartesian = do
      inside ":cartesian" do
        ranged do
          pure TProduct <*> some do
            inside "element" do
              core_type

    core_type = do
      select
        [ ranged do pure TVar <*> name
        , subtree "invokeBinary" do
            ranged do
              pure TApply
                <*> inside "typeConstr" name'
                <*> inside "arguments"  typeTuple
        , subtree "invokeUnary" do
            ranged do
              pure TApply
                <*> inside "typeConstr" name'
                <*> do pure <$> inside "arguments" type_

        , subtree "type_expr" newtype_
        ]

name' :: Parser (Pascal ASTInfo)
name' = do
  ranged do pure Name <*> anything

typeTuple :: Parser [Pascal ASTInfo]
typeTuple = do
  subtree "type_tuple" do
    many do inside "element" type_

-- example :: Text
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/address.ligo"
-- example = "../../../src/test/contracts/amount.ligo"
-- example = "../../../src/test/contracts/annotation.ligo"
example = "../../../src/test/contracts/arithmetic.ligo"
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
-- example = "../../../src/test/contracts/failwith.ligo"
-- example = "../../../src/test/contracts/loop.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
