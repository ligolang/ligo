module Raw_options = Compiler_options.Raw_options
module Trace = Simple_utils.Trace
open Scopes.Types

(** This is obtained by `ligo info get-scope`*)
type get_scope_api_result =
  Main_errors.all list * Main_warnings.all list * (Scopes.def list * Scopes.scopes) option

(** This is used by `LSP`. See `unfold_get_scope`*)
type get_scope_info =
  { errors : Main_errors.all list
  ; warnings : Main_warnings.all list
  ; has_info : bool
  ; definitions : Scopes.def list
  ; scopes : Scopes.scopes option
  }

(** Internal function, uses `raise` for throwing errors *)
let get_scope_raw
    (raw_options : Raw_options.t)
    (source_file : BuildSystem.Source_input.code_input)
    ~raise
    ()
  =
  let file_name =
    match source_file with
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
  let Compiler_options.{ with_types; _ } = options.tools in
  let core_prg =
    (* While building [Build.qualified_core] we need a smaller AST (without stdlib)
       that is the reason for no_stdlib as true *)
    let options = Compiler_options.set_no_stdlib options true in
    match source_file with
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
  Scopes.scopes ~raise ~options:options.middle_end ~with_types ~stdlib core_prg


(** Used by CLI, formats all errors and warnings and returns strings *)
let get_scope_cli_result
    (raw_options : Raw_options.t)
    source_file
    display_format
    no_colour
    ()
  =
  Scopes.Api_helper.format_result ~display_format ~no_colour
  @@ get_scope_raw raw_options (From_file source_file) ()


(** Used by LSP, we're trying to get as many errors/warnings as possible *)
let get_scope_trace
    (raw_options : Raw_options.t)
    (source_file : BuildSystem.Source_input.code_input)
    ()
  =
  Trace.try_with
    ~fast_fail:false
    (fun ~raise ~catch ->
      let v = get_scope_raw raw_options source_file () ~raise in
      catch.errors (), catch.warnings (), Some v)
    (fun ~catch e -> e :: catch.errors (), catch.warnings (), None)


(** Currently `get_scope_api_result` contains definitions from modules inside `mdef`.
    We want to search stuff only in `definitions`, so we traverse all local modules.
    TODO: change `Scopes.scopes` so it produces list that we need *)
let unfold_get_scope ((errors, warnings, plain_data_opt) : get_scope_api_result)
    : get_scope_info
  =
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
  let has_info, definitions, scopes =
    match plain_data_opt with
    | None -> false, [], None
    | Some (plain_decls, scopes) -> true, extract_defs plain_decls, Some scopes
  in
  { errors; warnings; has_info; definitions; scopes }
