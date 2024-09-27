module Stdlib = Stdlib
module Source_input = BuildSystem.Source_input
module Ligo_dep_cameligo = Ligo_dep_cameligo
module Ligo_dep_jsligo = Ligo_dep_jsligo

module type Params = sig
  val raise : (Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  val options : Compiler_options.t
  val top_level_syntax : Syntax_types.t
end

module M : functor (Params : Params) -> sig
  type meta_data = Ligo_compile.Helpers.meta
end

module Ast_typed_target : functor (Params : Params) -> sig
  type meta_data = Ligo_compile.Helpers.meta

  module AST : sig
    type t = Ast_typed.module_
    type interface = Ast_typed.signature
  end
end

module Ast_core_target : functor (Params : Params) -> sig
  type meta_data = Ligo_compile.Helpers.meta

  module AST : sig
    type t = Ast_core.program
    type interface = unit list
  end
end

type expression_michelson =
  { expression : Stacking.compiled_expression
  ; ast_type : Ast_aggregated.type_expression
  }

type ('a, 'b) named =
  { name : 'a
  ; value : 'b
  }

type contract_michelson =
  { entrypoint : (Ligo_prim.Value_var.t, Stacking.compiled_expression) named
  ; views : (Ligo_prim.Value_var.t, Stacking.compiled_expression) named list
  }

type view_michelson = (Ligo_prim.Value_var.t, Stacking.compiled_expression) named

val qualified_typed
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> Source_input.code_input
  -> Ast_typed.program

val qualified_typed_with_env
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> Source_input.code_input
  -> Ast_typed.program * Checking.Persistent_env.t

val qualified_typed_with_signature
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> Source_input.code_input
  -> Ast_typed.program

val build_contract_meta_ligo
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> string
  -> (Ast_aggregated.Types.expression
     * (Ligo_prim.Value_var.t list * Ast_aggregated.expression) option)
     Lwt.t

val parse_module_path
  :  loc:Stdlib.Location.t
  -> string
  -> Ligo_prim__Var.Module_var.t list

val build_expression
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> Syntax_types.t
  -> string
  -> string option
  -> expression_michelson Lwt.t

type graph = Graph__Persistent.Digraph.Concrete(BuildSystem__Types.Node).t

val dependency_graph
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> Source_input.code_input
  -> graph

val module_deps
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t -> Source_input.code_input -> string String.Map.t

val build_contract
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> string
  -> Source_input.code_input
  -> contract_michelson Lwt.t

val qualified_core
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> Source_input.code_input
  -> Ast_core.program

val qualified_core_from_string
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> Source_input.raw_input
  -> Ast_core.program

val qualified_core_from_raw_input
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> string
  -> string
  -> Ast_core.program

val unqualified_core
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> Source_input.file_name
  -> Ast_core.program

val qualified_typed_str
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> string
  -> Ast_typed.program

val build_type_expression
  :  raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
  -> options:Compiler_options.t
  -> Syntax_types.t
  -> string
  -> Source_input.file_name option
  -> (Mini_c.meta, string) Tezos_micheline.Micheline.node
