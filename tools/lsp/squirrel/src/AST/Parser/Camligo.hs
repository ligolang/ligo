-- TODO: recogniser does not recognize maps and bigmaps properly

module AST.Parser.Camligo where

import AST.Skeleton

import Duplo.Tree

import ParseTree
import Parser
import Product


recognise :: SomeRawTree -> ParserM (SomeLIGO Info)
recognise (SomeRawTree dialect rawTree)
  = fmap (SomeLIGO dialect)
  $ flip (descent (error "Camligo.recognise")) rawTree
  $ map usingScope
  [ -- Contract
    Descent do
      boilerplate $ \case
        "source_file" -> RawContract <$> fields "declaration"
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
        "let_in" -> Let <$> field "decl" <*> field "body"
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
        "paren_expr"        -> Paren      <$> field  "expr"
        "block_expr"        -> Seq        <$> fields "item"
        "annot_expr"        -> Annot      <$> field  "expr"      <*> field "type"
        "binary_op_app"     -> BinOp      <$> field  "left"      <*> field "op"    <*> field "right"
        "unary_op_app"      -> UnOp       <$> field  "negate"    <*> field "arg"
        "michelson_interop" -> Michelson <$> field  "code"       <*> field "type"  <*> fields "argument"
        _                   -> fallthrough

    -- QualifiedName
  , Descent do
      boilerplate $ \case
        "data_projection" -> QualifiedName <$> field "box" <*> fields "accessor"
        _ -> fallthrough


    -- Pattern
  , Descent do
      boilerplate $ \case
        "list_pattern"     -> IsList   <$> fields "item"
        "list_con_pattern" -> IsCons   <$> field  "x"    <*> field "xs"
        "tup_pattern"      -> IsTuple  <$> fields "item"
        "con_pattern"      -> IsConstr <$> field  "ctor" <*> fieldOpt "args"
        "par_annot_pattern"-> IsAnnot  <$> field  "pat"  <*> field "type"
        "paren_pattern"    -> IsTuple  <$> fields "pat"
        "var_pattern"      -> IsVar    <$> field  "var"
        "rec_pattern"      -> IsRecord <$> fields "field"
        "wildcard_pattern" -> pure IsWildcard
        _                  -> fallthrough

    -- Irrefutable
  , Descent do
      boilerplate $ \case
        "irrefutable_tuple"  -> IsTuple <$> fields "item"
        "annot_pattern"      -> IsAnnot <$> field  "pat"  <*> field "type"
        "closed_irrefutable" -> IsTuple <$> fields "pat"
        _                    -> fallthrough

   -- RecordFieldPattern
  , Descent do
      boilerplate $ \case
        "rec_field_pattern" -> IsRecordField <$> field "name" <*> field "body"
        "rec_capture_pattern" -> IsRecordCapture <$> field "name"
        _                   -> fallthrough

    -- Alt
  , Descent do
      boilerplate $ \case
        "matching"  -> Alt <$> field "pattern" <*> field  "body"
        _           -> fallthrough

    -- Record fields
  , Descent do
      boilerplate $ \case
        "rec_assignment" -> FieldAssignment <$> (pure <$> field "accessor") <*> field "value"
        "rec_path_assignment" -> FieldAssignment <$> fields "accessor" <*> field "value"
        _ -> fallthrough

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

    -- MichelsonCode
  , Descent do
      boilerplate' \case
        ("michelson_code", code) -> return $ MichelsonCode code
        _                        -> fallthrough

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

    -- NameModule
  , Descent do
      boilerplate' $ \case
        ("NameModule", n) -> return $ NameModule n
        _                 -> fallthrough

    -- FieldName
  , Descent do
      boilerplate' $ \case
        ("FieldName", n) -> return $ FieldName n
        _                -> fallthrough

    -- Type
  , Descent do
      boilerplate $ \case
        "type_fun"           -> TArrow   <$> field  "domain" <*> field "codomain"
        "type_app"           -> TApply   <$> field  "f"      <*> fields "x"
        "type_product"       -> TProduct <$> fields "x"
        "type_tuple"         -> TProduct <$> fields "x"
        "type_rec"           -> TRecord  <$> fields "field"
        "type_sum"           -> TSum     <$> fields "variant"
        "TypeWildcard"       -> pure TWildcard
        _                 -> fallthrough

    -- Module access:
  , Descent do
      boilerplate $ \case
        "module_TypeName" -> ModuleAccess <$> fields "path" <*> field "type"
        "module_access"   -> ModuleAccess <$> fields "path" <*> field "field"
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
        ("TypeName", name) -> return $ TypeName name
        _                  -> fallthrough

    -- Preprocessor
  , Descent do
      boilerplate \case
        "preprocessor" -> Preprocessor <$> field "preprocessor_command"
        _              -> fallthrough

    -- ProcessorCommand
  , Descent do
      boilerplate' \case
        ("p_if"      , rest) -> return $ PreprocessorCommand $ "#if "      <> rest
        ("p_error"   , rest) -> return $ PreprocessorCommand $ "#error "   <> rest
        ("p_warning" , rest) -> return $ PreprocessorCommand $ "#warning " <> rest
        ("p_define"  , rest) -> return $ PreprocessorCommand $ "#define "  <> rest
        _                    -> fallthrough

    -- Ctor
  , Descent do
      boilerplate' $ \case
        ("ConstrName", name)   -> return $ Ctor name
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
