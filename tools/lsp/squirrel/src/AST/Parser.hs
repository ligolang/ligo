
module AST.Parser (example, contract) where

import Data.Text (Text)

import AST.Types

import Parser
import Range

import Debug.Trace

name :: Parser (Name Range)
name = do
  (raw, info) <- range (token "Name")
  return Name {info, raw}

contract :: Parser (Contract Range)
contract = subtree "contract" do
  (decls, info) <- range do
    gets (length . pfGrove) >>= traceShowM
    many "declaration" declaration <* (gets (length . pfGrove) >>= traceShowM)

  return (Contract info decls)

declaration :: Parser (Declaration Range)
declaration =
  stubbed "declaration" do
    field "declaration" do
        (b, info) <- range binding
        return (ValueDecl info b)

par x = do
  consume "("
  a <- x
  consume ")"
  return a

binding :: Parser (Binding Range)
binding = do
  info <- getRange
  "fun_decl" `subtree` do
    recur <- optional do
      field "recursive" do
        token "recursive"
    consume "function"
    name <- stubbed "name" do
      field "name" do
        name
    params <-
      field "parameters" do
        subtree "parameters" do
          par do
            many "param" do
              notFollowedBy do
                consumeOrDie ")"

              stubbed "parameters" do
                paramDecl
    consume ":"
    ty <-
      stubbed "type" do
        field "type" type_
    consume "is"
    exp <- stubbed "body" do
      field "body" letExpr
    return (Function info (recur == Just "recursive") name params ty exp)

expr :: Parser (Expr Range)
expr = select
  [ Ident <$> getRange <*> name
  -- , ident
  -- , constant
  ]
  where
  -- $.case_expr,
  -- $.cond_expr,
  -- $.disj_expr,
  -- $.fun_expr,

letExpr = do
  subtree "let_expr" do
    r <- getRange
    decls <- optional do
      field "locals" do
        subtree "block" do
          many "decl" do
            field "statement" do
              declaration
    body <- field "body" do
      -- gets pfGrove >>= traceShowM
      stubbed "expr" do
        expr

    return case decls of
      Just them -> Let r them body
      Nothing   -> body

paramDecl :: Parser (VarDecl Range)
paramDecl = do
  info <- getRange
  "parameter" `field` do
    subtree "param_decl" do
      info' <- getRange
      mutable <- do
        traceM "paramDecl"
        stubbed "access" do
          "access" `subtree` do
            traceM "paramDecl"
            select
              [ consume "var"   >> return (Mutable info')
              , consume "const" >> return (Immutable info')
              ]
      name <-
        stubbed "name" do
          field "name" name
      consume ":"
      ty <-
        stubbed "type" do
          field "type" type_
      return (Decl info mutable name ty)

newtype_ = do
  type_

type_ :: Parser (Type Range)
type_ =
    fun_type
  where
    fun_type :: Parser (Type Range)
    fun_type = do
      stubbed "type" do
        subtree "fun_type" do
          info <- getRange
          domain <- stubbed "domain" do
            field "domain" cartesian
          codomain <- optional do
            consume "->"
            fun_type
          return case codomain of
            Just co -> TArrow info domain co
            Nothing -> domain

    cartesian = do
      stubbed "cartesian" do
        subtree "cartesian" do
          info <- getRange
          Product info <$> some "corety" do
            field "element" do
              core_type

    core_type = do
      info <- getRange
      select
        [ TVar info <$> typename
        ]

    typename = name

tuple :: Text -> Parser a -> Parser [a]
tuple msg = par . some msg

example = "../../../src/test/contracts/address.ligo"
