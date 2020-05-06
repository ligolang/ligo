
module AST.Parser (example, contract) where

import Data.Text (Text)

import AST.Types hiding (tuple)

import Parser
import Range

import Debug.Trace

name :: Parser (Name Range)
name = do
  (raw, info) <- range (token "Name")
  return Name {info, raw}

capitalName :: Parser (Name Range)
capitalName = do
  (raw, info) <- range (token "Name_Capital")
  return Name {info, raw}

contract :: Parser (Contract Range)
contract = subtree "contract" do
  (decls, info) <- range do
    many "declaration" do
      inside "declaration:" do
        declaration

  return (Contract info decls)

declaration :: Parser (Declaration Range)
declaration = do
    (b, info) <- range binding
    return (ValueDecl info b)
  <|> do
    (b, info) <- range vardecl
    return (ValueDecl info b)
  <|> do
    (b, info) <- range constdecl
    return (ValueDecl info b)
  <|>
    typedecl

typedecl :: Parser (Declaration Range)
typedecl = do
  subtree "type_decl" do
    r <- getRange
    n <- inside "typeName:"  name
    t <- inside "typeValue:" newtype_
    return $ TypeDecl r n t

vardecl :: Parser (Binding Range)
vardecl = do
  subtree "var_decl" do
    r  <- getRange
    n  <- inside "name:"  name
    ty <- inside "type:"  type_
    b  <- inside "value:" expr
    return (Var r n ty b)

constdecl :: Parser (Binding Range)
constdecl = do
  subtree "const_decl" do
    r  <- getRange
    n  <- inside "name" name
    ty <- inside "type" type_
    b  <- inside "value" expr
    return (Const r n ty b)

par x = do
  consume "("
  a <- x
  consume ")"
  return a

binding :: Parser (Binding Range)
binding = do
  info <- getRange
  inside ":fun_decl" do
    recur <- optional $ inside "recursive" $ token "recursive"
    name  <- inside "name:"  name
    params <-
      inside "parameters:parameters" do
        many "param" do
          notFollowedBy do
            consumeOrDie ")"

          stubbed "parameters" paramDecl
    ty  <- inside "type:" type_
    exp <- inside "body:" letExpr
    return (Function info (recur == Just "recursive") name params ty exp)

expr :: Parser (Expr Range)
expr = stubbed "expr" do
  select
    [ Ident <$> getRange <*> do
        r <- getRange
        n <- name
        return $ QualifiedName r n []
    , opCall
    , fun_call
    , record_expr
    , int_literal
    , par_call
    , method_call
    -- , if_expr
    -- , constant
    ]
  where
  -- $.case_expr,
  -- $.cond_expr,
  -- $.disj_expr,
  -- $.fun_expr,

method_call :: Parser (Expr Range)
method_call = do
  subtree "projection_call" do
    r <- getRange
    (f, r') <- field "f" $ range projection
    xs <- inside "arguments" arguments
    return $ Apply r (Ident r' f) xs

projection :: Parser (QualifiedName Range)
projection = do
  subtree "data_projection" do
    r  <- getRange
    s  <- inside "struct" name
    is <- many "selection" selection
    return $ QualifiedName r s is

selection :: Parser (Path Range)
selection = do
  inside "index:selection" $ do
      r <- getRange
      n <- name
      return $ At r n
    <|> do
      r <- getRange
      n <- token "Int"
      return $ Ix r n

par_call :: Parser (Expr Range)
par_call = do
  subtree "par_call" do
    r  <- getRange
    f  <- inside "f" expr
    az <- inside "arguments" arguments
    return $ Apply r f az

int_literal :: Parser (Expr Range)
int_literal = do
  r <- getRange
  i <- token "Int"
  return $ Constant r (Int r i)

record_expr :: Parser (Expr Range)
record_expr = do
  subtree "record_expr" do
    r  <- getRange
    az <- many "assignment" do
      inside "assignment:field_assignment" do
        r <- getRange
        n <- inside "name" name
        e <- inside "_rhs" expr
        return $ Assignment r n e
    return $ Record r az

fun_call :: Parser (Expr Range)
fun_call = do
  subtree "fun_call" do
    r <- getRange
    (f, r') <- range $ inside "f" function_id
    xs <- inside "arguments" do
      arguments
    return $ Apply r (Ident r' f) xs

arguments =
  subtree "arguments" do
    many "argument" do
      inside "argument" expr

function_id :: Parser (QualifiedName Range)
function_id = select
  [ do
      r <- getRange
      n <- name
      return $ QualifiedName r n []
  , do
      subtree "module_field" do
        r     <- getRange
        whole <- inside "module" capitalName
        path  <- inside "method" name
        return $ QualifiedName r whole [At r path]
  ]

opCall :: Parser (Expr Range)
opCall = do
  subtree "op_expr" $ do
      inside "the" do
        expr
    <|> do
      i <- getRange
      l <- inside "arg1" expr
      o <- inside "op"   anything
      r <- inside "arg2" expr
      return $ BinOp i l o r

letExpr = do
  subtree "let_expr" do
    r <- getRange
    decls <- optional do
      inside "locals:block" do
        many "decl" do
          inside "statement" do
            declaration <|> statement
    body <- inside "body"expr

    return case decls of
      Just them -> Let r them body
      Nothing   -> body

statement :: Parser (Declaration Range)
statement = do
  r <- getRange
  e <- expr
  return $ Action r e

paramDecl :: Parser (VarDecl Range)
paramDecl = do
  info <- getRange
  inside "parameter:param_decl" do
    info' <- getRange
    mutable <- do
      inside ":access" do
        select
          [ consume "var"   >> return (Mutable   info')
          , consume "const" >> return (Immutable info')
          ]
    name <- inside "name" name
    ty <- inside "type" type_
    return (Decl info mutable name ty)

newtype_ = select
  [ record_type
  , type_
  -- , sum_type
  ]

record_type = do
  subtree "record_type" do
    r  <- getRange
    fs <- many "field" do
      inside "field" do
        field_decl
    traceShowM fs
    return $ TRecord r fs

field_decl = do
  subtree "field_decl" do
    r <- getRange
    n <- inside "fieldName" name
    t <- inside "fieldType" type_
    return $ TField r n t

type_ :: Parser (Type Range)
type_ =
    fun_type
  where
    fun_type :: Parser (Type Range)
    fun_type = do
      inside ":fun_type" do
        info <- getRange
        domain <- inside "domain" cartesian
        codomain <- optional do
          consume "->"
          fun_type

        return case codomain of
          Just co -> TArrow info domain co
          Nothing -> domain

    cartesian = do
      inside ":cartesian" do
        info <- getRange
        TProduct info <$> some "corety" do
          inside "element" do
            core_type

    core_type = do
      info <- getRange
      select
        [ TVar info <$> typename
        , subtree "invokeBinary" do
            r  <- getRange
            f  <- inside "typeConstr" name
            xs <- inside "arguments"  typeTuple
            return $ TApply r f xs
        ]

    typename = name

typeTuple :: Parser [Type Range]
typeTuple = do
  subtree "type_tuple" do
    many "type tuple element" do
      inside "element" type_

tuple :: Text -> Parser a -> Parser [a]
tuple msg = par . some msg

-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/address.ligo"
example = "../../../src/test/contracts/amount.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/application.ligo"
