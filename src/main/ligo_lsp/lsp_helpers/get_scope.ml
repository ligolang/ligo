module Raw_options = Compiler_options.Raw_options
module Trace = Simple_utils.Trace
open Scopes.Types

(** This is used by the LSP. Note: defs must be unfolded before constructing this,
    see [unfold_defs] *)
type defs_and_diagnostics =
  { errors : Main_errors.all list
  ; warnings : Main_warnings.all list
  ; definitions : Def.t list
  }

let with_code_input
    :  f:
         (options:Compiler_options.middle_end
          -> stdlib:Ast_typed.program * Ast_core.program
          -> prg:Ast_core.module_
          -> 'a)
    -> raw_options:Raw_options.t -> raise:(Main_errors.all, Main_warnings.all) Trace.raise
    -> code_input:BuildSystem.Source_input.code_input -> 'a
  =
 fun ~f ~raw_options ~raise ~code_input ->
  let file_name =
    match code_input with
    | From_file file_name -> file_name
    | Raw { id; _ } -> id
    | Raw_input_lsp { file; _ } -> file
    | HTTP uri -> Simple_utils.Http_uri.get_filename uri
  in
  let syntax =
    Syntax.of_string_opt
      ~support_pascaligo:true
      ~raise
      (Syntax_name raw_options.syntax)
      (Some file_name)
  in
  let protocol_version =
    Ligo_compile.Helpers.protocol_to_variant ~raise raw_options.protocol_version
  in
  let options = Compiler_options.make ~raw_options ~syntax ~protocol_version () in
  (* let Compiler_options.{ with_types; _ } = options.tools in *)
  let prg =
    (* While building [Build.qualified_core] we need a smaller AST (without stdlib)
       that is the reason for no_stdlib as true *)
    let options = Compiler_options.set_no_stdlib options true in
    match code_input with
    | From_file file_name ->
      Build.qualified_core ~raise ~options (Build.Source_input.From_file file_name)
    | Raw file -> Build.qualified_core_from_string ~raise ~options file
    | Raw_input_lsp { file; code } ->
      Build.qualified_core_from_raw_input ~raise ~options file code
    | HTTP uri -> Build.qualified_core ~raise ~options (Build.Source_input.HTTP uri)
  in
  let lib =
    (* We need stdlib for [Build.Stdlib.get],
       because if [no_stdlib] we get [Build.Stdlib.empty] *)
    let options = Compiler_options.set_no_stdlib options false in
    Build.Stdlib.get ~options
  in
  let stdlib =
    Build.Stdlib.select_lib_typed syntax lib, Build.Stdlib.select_lib_core syntax lib
  in
  (* let () = assert (List.length (fst stdlib) = List.length (snd stdlib)) in *)
  f ~options:options.middle_end ~stdlib ~prg


(** Internal function, uses `raise` for throwing errors *)
let get_scope_raw
    (raw_options : Raw_options.t)
    (source_file : BuildSystem.Source_input.code_input)
    ~defs_only
    ~raise
  =
  let with_types = raw_options.with_types in
  with_code_input
    ~raw_options
    ~raise
    ~code_input:source_file
    ~f:
      (if defs_only
      then
        fun ~options ~stdlib ~prg ->
        Scopes.defs ~with_types ~raise ~options ~stdlib ~prg, []
      else Scopes.defs_and_scopes ~with_types ~raise)


(** Used by CLI, formats all definitions, errors and warnings and returns strings *)
let get_scope_cli_result
    (raw_options : Raw_options.t)
    ~source_file
    ~display_format
    ~no_colour
    ~defs_only
  =
  Scopes.Api_helper.format_result ~display_format ~no_colour
  @@ get_scope_raw raw_options (From_file source_file) ~defs_only


let get_scopes
    (raw_options : Raw_options.t)
    (code_input : BuildSystem.Source_input.code_input)
    (definitions : Def.t list)
  =
  Trace.try_with
    ~fast_fail:false
    (fun ~raise ~catch:_ ->
      with_code_input ~raw_options ~raise ~code_input ~f:(Scopes.scopes ~definitions))
    (fun ~catch:_ _ -> [])


(** Currently [Scopes.defs] result contains definitions from modules inside `mdef`.
    We want to search stuff only in `definitions`, so we traverse all local modules.
    TODO: change `Scopes.scopes_and_defs` so it produces list that we need *)
let unfold_defs (errors, warnings, nested_defs_opt) : defs_and_diagnostics =
  let extract_defs : def list -> def list =
    let rec from_module (m : mdef) =
      match m.mod_case with
      | Alias _ -> []
      | Def defs -> from_defs defs
    and from_defs defs =
      let current_module_defs : mdef list =
        List.filter_map
          ~f:(function
            | Module m -> Some m
            | Variable _ | Type _ -> None)
          defs
      in
      defs @ List.concat_map ~f:from_module current_module_defs
    in
    from_defs
  in
  let definitions =
    match nested_defs_opt with
    | None -> []
    | Some nested_defs -> extract_defs nested_defs
  in
  { errors; warnings; definitions }


(** Used by LSP, we're trying to get as many errors/warnings as possible *)
let get_defs_and_diagnostics
    (raw_options : Raw_options.t)
    (code_input : BuildSystem.Source_input.code_input)
  =
  let with_types = raw_options.with_types in
  unfold_defs
  @@ Trace.try_with
       ~fast_fail:false
       (fun ~raise ~catch ->
         let v =
           with_code_input
             ~raw_options
             ~raise
             ~code_input
             ~f:(Scopes.defs ~raise ~with_types)
         in
         catch.errors (), catch.warnings (), Some v)
       (fun ~catch e -> e :: catch.errors (), catch.warnings (), None)
