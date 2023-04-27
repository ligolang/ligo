-- | Parser for a Jsligo contract.
module Language.LIGO.AST.Parser.Jsligo
  ( recognise
  ) where

import Data.Default (Default (def))
import Prelude hiding (Alt)

import Duplo.Tree

import Data.Text qualified as T

import Language.LIGO.AST.Skeleton
import Language.LIGO.ParseTree
import Language.LIGO.Parser

recognise :: SomeRawTree -> ParserM (SomeLIGO Info)
recognise (SomeRawTree dialect rawTree)
  = fmap (SomeLIGO dialect)
  $ flip (descent (error "Jsligo.recognise")) rawTree
  $ map usingScope
  [ -- Contract
    Descent do
      boilerplate \case
        "source_file" -> RawContract <$> fields "toplevel"
        _ -> fallthrough

    -- Expr
  , Descent do
      boilerplate \case
        "unary_call"          -> UnOp       <$> field  "negate"      <*> field    "arg"
        "binary_call"         -> BinOp      <$> field  "left"        <*> field    "op"        <*> field "right"
        "assignment_operator" -> AssignOp   <$> field  "lhs"         <*> field    "op"        <*> field "rhs"
        "apply"               -> Apply      <$> field  "function"    <*> fields   "argument"
        "block_statement"     -> Seq        <$> fields "statement"
        "list_literal"        -> List       <$> fields "element"
        "annot_expr"          -> Annot      <$> field  "subject"     <*> field    "type"
        "if_else_statement"   -> If         <$> field  "selector"    <*> field    "then_branch" <*> fieldOpt "else_branch"
        "ternary_expr"        -> Ternary    <$> field  "selector"    <*> field    "then_branch" <*> field "else_branch"
        "record"              -> Record     <$> fields "assignment"
        "record_update"       -> RecordUpd  <$> field  "subject"     <*> fields   "field"
        "paren_expr"          -> Paren      <$> field  "expr"
        "tuple"               -> Tuple      <$> fields "item"
        "lambda"              -> Lambda     <$> fields "argument"    <*> pure [] <*> fieldOpt "type" <*> field "body"
        "code_inj"            -> CodeInj    <$> field  "lang"        <*> field    "code"
        "pattern_match"       -> Case       <$> field  "subject"     <*> fields   "alt"
        "switch_statement"    -> SwitchStm  <$> field  "selector"    <*> fields   "case"
        "while_statement"     -> WhileLoop  <$> field  "breaker"     <*> field    "statement"
        "for_of_statement"    -> ForOfLoop  <$> field  "key"         <*> field    "collection" <*> field "statement"
        "break_statement"     -> pure Break
        "return_statement"    -> Return     <$> fieldOpt "expr"
        "indexing"            -> ListAccess <$> field  "box"         <*> fields   "index"
        "type_as_annotation"  -> Annot      <$> field  "subject"     <*> field    "type"
        _                     -> fallthrough

    -- Case & Default
  , Descent do
      boilerplate \case
        "case_statement"    -> CaseStm    <$> field    "selector_value" <*> fields "statement"
        "default_statement" -> DefaultStm <$> fields "statement"
        _                   -> fallthrough

    -- Pattern
  , Descent do
      boilerplate \case
        "tuple_pattern"          -> IsTuple  <$> fields "pattern"
        "list_pattern"           -> IsList   <$> fields "pattern"
        "spread_pattern"         -> IsSpread <$> field  "expr"
        "var_pattern"            -> IsVar    <$> field  "var"
        "wildcard"               -> pure IsWildcard
        "constr_pattern"         -> IsConstr <$> field  "constructor" <*> fieldOpt "arg"
        "ctor_params"            -> IsTuple  <$> fields "ctor_param"
        "ctor_param"             -> IsAnnot  <$> field  "subject"     <*> field    "type"
        "record_pattern"         -> IsRecord <$> fields "field"
        _                        -> fallthrough

    -- RecordFieldPattern
  , Descent do
      boilerplate \case
        "record_field_pattern"   -> IsRecordField   <$> field "name" <*> field "body"
        "record_capture_pattern" -> IsRecordCapture <$> field "name"
        "record_rest_pattern"    -> IsRecordCapture <$> field "name"
        _                        -> fallthrough

    -- Alt
  , Descent do
      boilerplate \case
        "list_case" -> Alt <$> field "pattern" <*> field "expr"
        "ctor_case" -> Alt <$> field "pattern" <*> field "expr"
        _           -> fallthrough

    -- Record fields
  , Descent do
      boilerplate \case
        "capture"           -> Capture         <$> field "accessor"
        "record_field"      -> FieldAssignment <$> fields "accessor" <*> field "value"
        "spread"            -> Spread          <$> field  "name"
        _                   -> fallthrough

    -- Preprocessor
  , Descent do
      boilerplate \case
        "preprocessor" -> Preprocessor <$> field "preprocessor_command"
        _              -> fallthrough

    -- ProcessorCommand
  , Descent do
      boilerplate' \case
        ("p_if"      , rest) -> pure $ PreprocessorCommand $ "#if "      <> rest
        ("p_error"   , rest) -> pure $ PreprocessorCommand $ "#error "   <> rest
        ("p_define"  , rest) -> pure $ PreprocessorCommand $ "#define "  <> rest
        _                    -> fallthrough

  , Descent do
      boilerplate' \case
        ("||", _)     -> pure $ Op "||"
        ("&&", _)     -> pure $ Op "&&"
        ("<", _)      -> pure $ Op "<"
        ("<=", _)     -> pure $ Op "<="
        (">", _)      -> pure $ Op ">"
        (">=", _)     -> pure $ Op ">="
        ("==", _)     -> pure $ Op "=="
        ("!=", _)     -> pure $ Op "!="
        ("+", _)      -> pure $ Op "+"
        ("-", _)      -> pure $ Op "-"
        ("*", _)      -> pure $ Op "*"
        ("/", _)      -> pure $ Op "/"
        ("%", _)      -> pure $ Op "%"
        ("=", _)      -> pure $ Op "="
        ("*=", _)     -> pure $ Op "*="
        ("/=", _)     -> pure $ Op "/="
        ("%=", _)     -> pure $ Op "%="
        ("+=", _)     -> pure $ Op "+="
        ("-=", _)     -> pure $ Op "-="
        ("negate", n) -> pure $ Op n
        _             -> fallthrough

  , Descent do
      boilerplate \case
        "data_projection" -> QualifiedName <$> field "expr" <*> fields "accessor"
        _                 -> fallthrough

    -- Literal
  , Descent do
      boilerplate' \case
        ("Int",    i) -> pure $ CInt i
        ("Nat",    i) -> pure $ CNat i
        ("Bytes",  i) -> pure $ CBytes i
        ("String", i) -> pure $ CString i
        ("Tez",    i) -> pure $ CTez i
        _             -> fallthrough

    -- Declaration
  , Descent do
      boilerplate \case
        "toplevel_binding"    -> BConst False <$> field "binding_pattern" <*> fields "param" <*> fieldOpt "type_annot" <*> fieldOpt "value"
        "const_binding"       -> BConst False <$> field "binding_pattern" <*> fields "param" <*> fieldOpt "type_annot" <*> fieldOpt "value"
        "let_binding"         -> BVar         <$> field "binding_pattern" <*> fields "param" <*> fieldOpt "type_annot" <*> fieldOpt "value"
        "type_decl"           -> BTypeDecl    <$> field "type_name"       <*> fieldOpt "params"     <*> field    "type_value"
        "p_include"           -> BInclude     <$> field "filename"
        "p_import"            -> BImport      <$> field "filename"        <*> field "alias"
        "fun_arg"             -> BParameter   <$> field "argument"        <*> fieldOpt "type"
        "namespace_statement" -> BModuleDecl  <$> field "moduleName"      <*> fields "declaration"
        "import_statement"    -> BModuleAlias <$> field "moduleName"      <*> fields "module"
        _                     -> fallthrough

    -- QuotedTypeParams
  , Descent do
      boilerplate \case
        "type_params" -> QuotedTypeParams <$> fields "param"
        _             -> fallthrough

    -- Verbatim
  , Descent do
      boilerplate' \case
        ("Verbatim", code) -> pure $ Verbatim code
        _                  -> fallthrough

    -- Name
  , Descent do
      boilerplate' \case
        ("Name", n) -> pure $ Name n
        _           -> fallthrough

    -- NameDecl
  , Descent do
      boilerplate' \case
        ("NameDecl", n) -> pure $ NameDecl n
        _               -> fallthrough

    -- ModuleName
  , Descent do
      boilerplate' \case
        ("ModuleName", n) -> pure $ ModuleName n
        _                 -> fallthrough

    -- Type
  , Descent do
      boilerplate \case
        "string_type"      -> TString        <$> field  "value"
        "fun_type"         -> TArrow         <$> field  "domain"     <*> field  "codomain"
        "app_type"         -> TApply         <$> field  "functor"    <*> fields "argument"
        "record_type"      -> TRecord def    <$> fields "field_decl"
        "tuple_type"       -> TProduct       <$> fields "element"
        "sum_type"         -> TSum def       <$> fields1 "variant"
        "disc_union_type"  -> TSum def       <$> fields1 "variant"
        "TypeWildcard"     -> pure TWildcard
        "var_type"         -> TVariable      <$> field  "name"
        "domain"           -> TProduct       <$> fields "type"
        "paren_type"       -> TParen         <$> field  "type"
        _                  -> fallthrough

    -- Module access:
  , Descent do
      boilerplate \case
        "module_access_t" -> ModuleAccess <$> fields "path" <*> field "type"
        "module_access"   -> ModuleAccess <$> fields "path" <*> field "field"
        _                 -> fallthrough

    -- Variant
  , Descent do
      boilerplate \case
        "variant"   -> Variant  <$> field "constructor" <*> fieldOpt "ctor_arguments"
        _           -> fallthrough

    -- Variant args
  , Descent do
      boilerplate \case
        "ctor_arguments" -> TProduct  <$> fields "ctor_argument"
        _                -> fallthrough

    -- TField
  , Descent do
      boilerplate \case
        "field_decl" -> TField <$> field "field_name" <*> fieldOpt "field_type"
        _            -> fallthrough

    -- TypeName
  , Descent do
      boilerplate' \case
        ("TypeName", name) -> pure $ TypeName name
        _                  -> fallthrough

    -- TypeVariableName
  , Descent do
      boilerplate' \case
        ("TypeVariableName", name) -> pure $ TypeVariableName name
        _                          -> fallthrough

    -- FieldName
  , Descent do
      boilerplate' \case
        ("FieldName", name) -> pure $ FieldName name
        _                   -> fallthrough

    -- Ctor
  , Descent do
      boilerplate' \case
        ("ConstrName", name)   -> pure $ Ctor name
        ("True_kwd", _)        -> pure $ Ctor "True"
        ("False_kwd", _)       -> pure $ Ctor "False"
        ("Unit_kwd", _)        -> pure $ Ctor "Unit"
        ("constructor", c)     -> pure $ Ctor c
        ("ConstrNameType", n)  -> pure $ Ctor (T.init $ T.tail n)
        _                      -> fallthrough

  -- Attr
  , Descent $
      boilerplate' \case
        ("Attr", attr) -> pure $ Attr attr
        _              -> fallthrough

  -- Lang
  , Descent $
      boilerplate' \case
        -- n.b.: Putting it in Attr for now, for consistency with other dialects.
        ("lang", lang) -> pure $ Attr lang
        _              -> fallthrough

  -- Err
  , Descent noMatch
  ]
