-- | Parser for a PascaLigo contract.
module AST.Parser.Pascaligo
  ( recognise
  ) where

import AST.Skeleton

import Duplo.Tree

import Parser
import ParseTree

recognise :: SomeRawTree -> ParserM (SomeLIGO Info)
recognise (SomeRawTree dialect rawTree)
  = fmap (SomeLIGO dialect)
  $ flip (descent (error "Pascaligo.recognise")) rawTree
  $ map usingScope
  [ -- Contract
    Descent do
      boilerplate \case
        "source_file" -> RawContract <$> fields "declaration"
        _ -> fallthrough

    -- Expr
  , Descent do
      boilerplate \case
        "block_with"        -> Let       <$> field  "locals"    <*> field "body"
        "block_with_closed" -> Let       <$> field  "locals"    <*> field "body"
        "call_expr"         -> Apply     <$> field  "f"         <*> fields "argument"
        "unop"              -> UnOp      <$> field  "negate"    <*> field "arg"
        "binop"             -> BinOp     <$> field  "arg1"      <*> field "op"   <*> field "arg2"
        "block"             -> Seq       <$> fields "statement"
        "clause_block"      -> Seq       <$> fields "statement"
        "list_expr"         -> List      <$> fields "element"
        "annot_expr"        -> Annot     <$> field  "subject"   <*> field "type"
        "if_then_instr"     -> If <$> field "selector" <*> field "then" <*> pure Nothing
        "if_then_else_instr" -> If <$> field "selector" <*> field "then" <*> (Just <$> field "else")
        "if_then_else_instr_closed" -> If <$> field "selector" <*> field "then" <*> (Just <$> field "else")
        "if_then_expr"      -> If <$> field "selector" <*> field "then" <*> pure Nothing
        "if_then_else_expr" -> If <$> field "selector" <*> field "then" <*> (Just <$> field "else")
        "if_then_else_expr_closed" -> If <$> field "selector" <*> field "then" <*> (Just <$> field "else")
        "assignment"        -> Assign    <$> field  "LHS"       <*> field "RHS"
        "assignment_closed" -> Assign    <$> field  "LHS"       <*> field "RHS"
        "record_expr"       -> Record    <$> fields "assignment"
        "remove_instr"      -> Remove    <$> field  "key" <*> field "remove" <*> field "container"
        "tuple_expr"        -> Tuple     <$> fields "element"
        "skip"              -> return Skip
        "case_expr"         -> Case      <$> field  "subject"    <*> fields   "case"
        "case_instr"        -> Case      <$> field  "subject"    <*> fields   "case"
        "fun_expr"          -> Lambda    <$> fields "parameter"  <*> fieldOpt    "type"  <*> field "body"
        "fun_expr_closed"   -> Lambda    <$> fields "parameter"  <*> fieldOpt    "type"  <*> field "body"
        "for_int"           -> ForLoop   <$> field  "name"       <*> field    "begin" <*> field "end" <*> fieldOpt "step" <*> field "body"
        "for_in"            -> ForBox    <$> field  "key"        <*> fieldOpt "value" <*> field "kind"  <*> field "collection" <*> field "body"
        "while_loop"        -> WhileLoop <$> field  "breaker"    <*> field    "body"
        "map_expr"          -> Map       <$> fields "binding"
        "big_map_expr"      -> BigMap    <$> fields "binding"
        "list_injection"    -> List      <$> fields "element"
        "set_expr"          -> Set       <$> fields "element"
        "patch_instr"       -> Patch     <$> field  "container"  <*> field "expr"
        "update_record"     -> RecordUpd <$> field  "record"     <*> fields "assignment"
        "code_inj"          -> CodeInj    <$> field  "lang"      <*> field "code"
        "paren_expr"        -> Paren     <$> field  "expr"
        "ctor_app_expr"     -> Apply     <$> field  "ctor" <*> fields "arguments"
        _                   -> fallthrough

  , Descent do
      boilerplate \case
        "patchable_call_expr"  -> PatchableExpr <$> field "patchable" <*> field "expr"
        "patchable_paren_expr" -> PatchableExpr <$> field "patchable" <*> field "expr"
        _ -> fallthrough

    -- Collection
  , Descent do
      boilerplate' \case
        ("collection", "map")  -> pure CMap
        ("collection", "set")  -> pure CSet
        ("collection", "list") -> pure CList
        _                      -> fallthrough

    -- Removable
  , Descent do
      boilerplate' \case
        ("removable", "map")  -> pure CMap
        ("removable", "set")  -> pure CSet
        _                     -> fallthrough

    -- Pattern
  , Descent do
      boilerplate \case
        "user_constr_pattern" -> IsConstr <$> field  "constr" <*> fieldOpt "arguments"
        "tuple_pattern"       -> IsTuple  <$> fields "element"
        "nil"                 -> return $ IsList []
        "list_pattern"        -> IsList   <$> fields "element"
        "cons_pattern"        -> IsCons   <$> field  "head"   <*> field "tail"
        "var_pattern"         -> IsVar    <$> field  "name"
        "record_pattern"      -> IsRecord <$> fields "field"
        "wildcard_pattern"    -> pure IsWildcard
        _                     -> fallthrough

    -- Irrefutable tuple
  , Descent do
      boilerplate $ \case
        "irrefutable_tuple" -> IsTuple <$> fields "item"
        _                   -> fallthrough

    -- RecordFieldPattern
  , Descent do
      boilerplate $ \case
        "record_field_pattern"   -> IsRecordField <$> field "name" <*> field "body"
        "record_capture_pattern" -> IsRecordCapture <$> field "name"
        _                        -> fallthrough

    -- Alt
  , Descent do
      boilerplate \case
        "case_clause_expr"  -> Alt <$> field "pattern" <*> field  "body"
        "case_clause_instr" -> Alt <$> field "pattern" <*> field  "body"
        _                   -> fallthrough

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

    -- FieldAssignment
  , Descent do
      boilerplate \case
        "field_path_assignment" -> FieldAssignment <$> fields "accessor" <*> field "_rhs"
        _                       -> fallthrough

    -- MapBinding
  , Descent do
      boilerplate \case
        "binding" -> MapBinding <$> field "key" <*> field "value"
        _         -> fallthrough

  , Descent do
      boilerplate' \case
        ("negate",     op) -> return $ Op op
        ("adder",      op) -> return $ Op op
        ("multiplier", op) -> return $ Op op
        ("comparison", op) -> return $ Op op
        ("^",          _)  -> return $ Op "^"
        ("#",          _)  -> return $ Op "#"
        _                  -> fallthrough

  , Descent do
      boilerplate \case
        "data_projection" -> QualifiedName <$> field "selector" <*> fields "accessor"
        "field_path"      -> QualifiedName <$> field "selector" <*> fields "accessor"
        "map_lookup"      -> QualifiedName <$> field "container" <*> fields "index"
        "module_field"    -> QualifiedName <$> field "module" <*> fields "method"
        _                 -> fallthrough

    -- Literal
  , Descent do
      boilerplate' \case
        ("Int",    i) -> return $ CInt i
        ("Nat",    i) -> return $ CNat i
        ("Bytes",  i) -> return $ CBytes i
        ("String", i) -> return $ CString i
        ("Tez",    i) -> return $ CTez i
        _             -> fallthrough

    -- Declaration
  , Descent do
      boilerplate \case
        "fun_decl"   -> BFunction <$> flag "recursive" <*> field "name" <*> fields "parameter" <*> fieldOpt "type" <*> field "body"
        "const_decl" -> BConst    <$>             field    "name"       <*> fieldOpt "type" <*> fieldOpt "value"
        "var_decl"   -> BVar      <$>             field    "name"       <*> fieldOpt "type" <*> fieldOpt "value"
        "type_decl"  -> BTypeDecl <$>             field    "typeName"   <*> fieldOpt "params" <*> field "typeValue"
        "p_include"  -> BInclude  <$>             field    "filename"
        "p_import"   -> BImport   <$>             field    "filename" <*> field "alias"
        "module_decl" -> BModuleDecl <$> field "moduleName" <*> fields "declaration"
        "module_alias" -> BModuleAlias <$> field "moduleName" <*> fields "module"
        _            -> fallthrough

    -- TypeParams
  , Descent do
      boilerplate \case
        "type_params" -> TypeParams <$> fields "param"
        _             -> fallthrough

    -- VarDecl
  , Descent do
      boilerplate \case
        "param_decl" -> BParameter <$> field "name" <*> fieldOpt "type"
        _            -> fallthrough

    -- Name
  , Descent do
      boilerplate' \case
        ("Name", n)     -> return $ Name n
        ("and", _)      -> return $ Name "and"
        ("or", _)       -> return $ Name "or"
        ("contains", _) -> return $ Name "contains"
        _               -> fallthrough

    -- NameDecl
  , Descent do
      boilerplate' $ \case
        ("NameDecl", n) -> return $ NameDecl n
        _               -> fallthrough

    -- ModuleName
  , Descent do
      boilerplate' $ \case
        ("ModuleName", n) -> return $ ModuleName n
        _                 -> fallthrough

    -- Type
  , Descent do
      boilerplate \case
        "string_type"      -> TString  <$> field  "value"
        "fun_type"         -> TArrow   <$> field  "domain" <*> field "codomain"
        "prod_type"        -> TProduct <$> fields "element"
        "app_type"         -> TApply   <$> field  "name" <*> fields "arg"
        "record_type"      -> TRecord  <$> fields "field"
        "sum_type"         -> TSum     <$> fields1 "variant"
        "type_group"       -> TProduct <$> (pure <$> field "type")
        "TypeWildcard"     -> pure TWildcard
        "var_type"         -> TVariable <$> field "name"
        _                  -> fallthrough

    -- Module access:
  , Descent do
      boilerplate $ \case
        "module_TypeName" -> ModuleAccess <$> fields "path" <*> field "type"
        "module_access"   -> ModuleAccess <$> fields "path" <*> field "field"
        _                 -> fallthrough

    -- Variant
  , Descent do
      boilerplate \case
        "variant" -> Variant <$> field "constructor" <*> fieldOpt "arguments"
        _         -> fallthrough

  -- Verbatim
  , Descent do
      boilerplate' \case
        ("Verbatim", code) -> pure $ Verbatim code
        _                  -> fallthrough

    -- TField
  , Descent do
      boilerplate \case
        "field_decl" -> TField <$> field "fieldName" <*> fieldOpt "fieldType"
        _            -> fallthrough

    -- TypeName
  , Descent do
      boilerplate' \case
        ("TypeName", name) -> return $ TypeName name
        _                  -> fallthrough

    -- TypeVariableName
  , Descent do
      boilerplate' \case
        ("TypeVariableName", name) -> pure $ TypeVariableName name
        _                          -> fallthrough

    -- Ctor
  , Descent do
      boilerplate' \case
        ("ConstrName", name) -> return $ Ctor name
        ("ModuleName", name) -> return $ Ctor name
        ("constr", n)        -> return $ Ctor n
        _                    -> fallthrough

    -- FieldName
  , Descent do
      boilerplate' \case
        ("FieldName", name) -> return $ FieldName name
        _                   -> fallthrough

  -- Attr
  , Descent $
      boilerplate' \case
        ("Attr", attr) -> pure $ Attr attr
        _              -> fallthrough

    -- Err
  , Descent noMatch
  ]
