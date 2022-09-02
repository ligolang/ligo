open Types

module MVar = Ast_typed.ModuleVar

module AST = Ast_core

type def_list = (string * def) list

type reference =
    Variable of AST.expression_variable
  | Type of AST.type_variable
  | ModuleAccess of AST.module_variable list * AST.expression_variable

let declaration : AST.declaration -> def list * reference list
  = fun decl ->
      match decl.wrap_content with
        Declaration_constant { attr        = { hidden ; _ } ; _ }
      | Declaration_module   { module_attr = { hidden ; _ } ; _ }
      | Declaration_type     { type_attr   = { hidden ; _ } ; _ } when hidden -> [], []
      | Declaration_constant { binder      = { var ; ascr=_ ; _ } ; expr ; _ } ->
        (* add def for var *)
        (* calculate function for expr -> def list & reference list *)
        [], []
      | Declaration_type     { type_binder ; type_expr ; _ } ->
        (* add def for type_binder *)
        (* calculate function for type_expr -> [] & reference list (free type variables) *)
        [], []
      | Declaration_module   { module_binder ; module_ ; _ } ->
        (* add def for module_binder *)
        (* calculate function for module_ -> def list & free references *)
        (* should return [Module {def list}], free references *)
        [], []


let scopes : with_types:bool -> options:Compiler_options.middle_end -> AST.module_ -> (def_map * scopes)
  = fun ~with_types ~options prg ->
      Def_map.empty, []

(*

For each declaration there will be a def
a function on expression will returns def list & references (vars) list

for an expression its free_variable will be references

Initial version only defintions no types
next add types
next add scopes

*)