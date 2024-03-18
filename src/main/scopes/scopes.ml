open Ligo_prim
open Types
module VVar = Value_var
module TVar = Type_var
module MVar = Module_var
module Formatter = Formatter
module Api_helper = Api_helper
module Misc = Misc
module LSet = Types.LSet
module LMap = Types.LMap
module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
module Types = Types
module Uid = Types.Uid
module SMap = Map.Make (String)

[@@@landmark "auto"]

(* To profile
   Build: dune build --instrument-with landarks ./src/bin/runligo.exe
   Run:   OCAML_LANDMARKS=auto _build/default/src/bin/runligo.exe info get-scope x.mligo --format dev --with-types
*)

module PP = PP

type def = Types.def
type definitions = Types.definitions
type scopes = Types.scopes
type inlined_scopes = Types.inlined_scopes

type t = Types.t =
  { definitions : definitions
  ; program : Ast_typed.program option
  ; inlined_scopes : inlined_scopes lazy_t
  ; lambda_types : Ast_typed.ty_expr LMap.t
  }

let scopes_declarations
    :  options:Compiler_options.middle_end -> stdlib:Ast_typed.program * Ast_core.program
    -> prg:Ast_core.module_ -> Scopes_pass.t
  =
 fun ~options ~stdlib ~prg ->
  let _, stdlib_core = stdlib in
  let env_preload_decls = if options.no_stdlib then [] else stdlib_core in
  Scopes_pass.Of_Ast.declarations ~env_preload_decls prg


(* Slow scopes calculation. Needed for
   backward compatibility (e,g, for the debugger) *)
let inlined_scopes
    :  options:Compiler_options.middle_end -> stdlib:Ast_typed.program * Ast_core.program
    -> prg:Ast_core.module_ -> definitions:definitions -> inlined_scopes
  =
 fun ~options ~stdlib ~prg ~definitions ->
  scopes_declarations ~options ~stdlib ~prg
  |> Scopes_pass.inline_scopes (Uid_map.of_defs_list definitions)


(* Fast scopes calculation that is used in LSP *)
let scopes
    :  options:Compiler_options.middle_end -> stdlib:Ast_typed.program * Ast_core.program
    -> prg:Ast_core.module_ -> scopes
  =
 fun ~options ~stdlib ~prg ->
  scopes_declarations ~options ~stdlib ~prg
  |> LMap.map (List.filter_map ~f:Env.Def.make_uid)
  |> LMap.to_kv_list


let run
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
    -> options:Compiler_options.middle_end -> stdlib:Ast_typed.program * Ast_core.program
    -> prg:Ast_core.module_ -> module_deps:string SMap.t -> with_types:bool -> t
  =
 fun ~raise ~options ~stdlib ~prg ~module_deps ~with_types ->
  let stdlib_decls, stdlib_core, stdlib_defs =
    if options.no_stdlib
    then fst stdlib, [], []
    else (
      let stdlib_decls, stdlib_core = stdlib in
      let stdlib_core_types =
        Types_pass.(
          Of_Ast_core.program ~raise (empty Env.empty) stdlib_decls.pr_sig stdlib_core)
      in
      ( stdlib_decls
      , stdlib_core
      , Definitions.Of_Stdlib_Ast.definitions stdlib_core module_deps
        |> Types_pass.patch stdlib_core_types ))
  in
  let stdlib_and_prg = stdlib_core @ prg in
  let m_alias, env = Module_aliases_pass.declarations stdlib_and_prg in
  let tenv, typed =
    if with_types
    then
      Tuple2.map_snd ~f:Option.some
      @@ Types_pass.resolve ~raise ~options ~stdlib_decls ~module_env:env prg
    else Types_pass.empty Env.empty, None
  in
  let mangled_uids, defs = Definitions.Of_Ast.definitions prg module_deps stdlib_defs in
  let definitions =
    defs
    |> Module_aliases_pass.patch m_alias
    |> (if with_types then Types_pass.patch tenv else Fn.id)
    |> References.patch (References.declarations stdlib_and_prg tenv.label_cases)
    |> Inline_mangled_modules_pass.patch mangled_uids
    |> wrap_definitions
  in
  { definitions
  ; program = typed
  ; inlined_scopes = lazy (inlined_scopes ~options ~stdlib ~prg ~definitions)
  ; lambda_types = tenv.lambda_cases
  }
