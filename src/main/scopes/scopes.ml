open Ligo_prim
open Types
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

let defs_and_typed_program
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
    -> options:Compiler_options.middle_end -> stdlib:Ast_typed.program * Ast_core.program
    -> prg:Ast_core.module_ -> with_types:bool
    -> def list * (Ast_typed.signature * Ast_typed.declaration list) option
  =
 fun ~raise ~options ~stdlib ~prg ~with_types ->
  let stdlib_decls, stdlib_core = stdlib in
  let stdlib_defs =
    if options.no_stdlib
    then []
    else (
      let stdlib_core_types = Types_pass.(Of_Ast_core.declarations empty stdlib_core) in
      Definitions.Of_Stdlib.definitions stdlib_core |> Types_pass.patch stdlib_core_types)
  in
  let bindings, typed =
    if with_types
    then (
      let Types_pass.Typing_env.{ type_env; bindings; decls } =
        Types_pass.resolve ~raise ~options ~stdlib_decls prg
      in
      bindings, Some (type_env, decls))
    else Types_pass.empty, None
  in
  ( Definitions.definitions prg stdlib_defs
    |> (if with_types then Types_pass.patch bindings else Fn.id)
    |> References.patch (References.declarations (stdlib_core @ prg))
    |> Module_aliases_pass.patch (Module_aliases_pass.declarations prg)
  , typed )


let scopes
    :  options:Compiler_options.middle_end -> stdlib:Ast_typed.program * Ast_core.program
    -> prg:Ast_core.module_ -> definitions:def list -> scopes
  =
 fun ~options ~stdlib ~prg ~definitions ->
  let _stdlib_decls, stdlib_core = stdlib in
  let stdlib_defs, env_preload_decls =
    if options.no_stdlib
    then [], []
    else
      ( (let stdlib_core_types = Types_pass.(Of_Ast_core.declarations empty stdlib_core) in
         Definitions.Of_Stdlib.definitions stdlib_core
         |> Types_pass.patch stdlib_core_types)
      , stdlib_core )
  in
  Scopes_pass.Of_Ast.declarations ~env_preload_decls prg
  |> Scopes_pass.to_old_scopes (flatten_defs definitions @ stdlib_defs)
  |> fix_shadowing_in_scopes


let defs_and_typed_program_and_scopes
    :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
    -> options:Compiler_options.middle_end -> stdlib:Ast_typed.program * Ast_core.program
    -> prg:Ast_core.module_ -> with_types:bool
    -> def list * (Ast_typed.signature * Ast_typed.declaration list) option * scopes
  =
 fun ~raise ~options ~stdlib ~prg ~with_types ->
  let definitions, typed =
    defs_and_typed_program ~raise ~options ~stdlib ~prg ~with_types
  in
  let scopes = scopes ~options ~stdlib ~prg ~definitions in
  definitions, typed, scopes
