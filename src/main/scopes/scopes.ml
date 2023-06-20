open Ligo_prim
open Types
module AST = Ast_core
module VVar = Value_var
module TVar = Type_var
module MVar = Module_var
module Formatter = Formatter
module Api_helper = Api_helper
module LSet = Types.LSet
module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
module Types = Types

[@@@landmark "auto"]

(* To profile
   Build: dune build --instrument-with landarks ./src/bin/runligo.exe
   Run:   OCAML_LANDMARKS=auto _build/default/src/bin/runligo.exe info get-scope x.mligo --format dev --with-types
*)

module PP = PP

type def = Types.def
type scopes = Types.scopes

let scopes
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise -> with_types:bool
    -> options:Compiler_options.middle_end -> stdlib:Ast_typed.program * Ast_core.program
    -> AST.module_ -> def list * scopes
  =
 fun ~raise ~with_types ~options ~stdlib prg ->
  let stdlib_decls, stdlib_core = stdlib in
  let stdlib_defs, env_preload_decls =
    if options.no_stdlib
    then [], []
    else
      ( (let stdlib_core_types = Types_pass.(Of_Ast_core.declarations empty stdlib_core) in
         Definitions.Of_Stdlib.definitions stdlib_core
         |> Types_pass.patch stdlib_core_types)
      , stdlib_core )
  in
  let defs =
    Definitions.definitions prg stdlib_defs
    |> (if with_types
       then Types_pass.patch (Types_pass.resolve ~raise ~options ~stdlib_decls prg)
       else Fn.id)
    |> References.patch (References.declarations (stdlib_core @ prg))
    |> Module_aliases_pass.patch (Module_aliases_pass.declarations prg)
  in
  let scopes =
    Scopes_pass.Of_Ast.declarations ~env_preload_decls prg
    |> Scopes_pass.to_old_scopes (flatten_defs defs @ stdlib_defs)
    |> fix_shadowing_in_scopes
  in
  defs, scopes
