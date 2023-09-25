module Raw_options = Compiler_options.Raw_options
module Trace = Simple_utils.Trace
open Scopes.Types

(** Caches diagnostics for a storage definition. Useful to avoid excessive TZIP-16 checks,
    which can download JSONs from the internet and lag the language server. For now, just
    warnings as errors aren't thrown. *)
type storage_diagnostics = { storage_warnings : Main_warnings.all list }

let empty_storage_diagnostics = { storage_warnings = [] }

(** This is used by the LSP. Note: defs must be unfolded before constructing this,
    see [unfold_defs]. [storage_errors] and [storage_warnings] are separated from other
    diagnostics in order to be cached, as these diagnostics may require downloads, which
    can be slow. *)
type defs_and_diagnostics =
  { errors : Main_errors.all list
  ; warnings : Main_warnings.all list
  ; storage_diagnostics : storage_diagnostics
  ; definitions : Def.t list
  }

let with_code_input
    :  f:
         (options:Compiler_options.middle_end
          -> syntax:Syntax_types.t
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
    Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some file_name)
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
    | From_file _ | HTTP _ -> Build.qualified_core ~raise ~options code_input
    | Raw file -> Build.qualified_core_from_string ~raise ~options file
    | Raw_input_lsp { file; code } ->
      Build.qualified_core_from_raw_input ~raise ~options file code
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
  f ~options:options.middle_end ~stdlib ~prg ~syntax


(** Internal function, uses `raise` for throwing errors *)
let get_scope_raw
    (raw_options : Raw_options.t)
    (source_file : BuildSystem.Source_input.code_input)
    ~defs_only
    ~raise
    : def list * (Ast_typed.signature * Ast_typed.declaration list) option * scopes
  =
  let with_types = raw_options.with_types in
  with_code_input
    ~raw_options
    ~raise
    ~code_input:source_file
    ~f:(fun ~options ~syntax:_ ~stdlib ~prg ->
      if defs_only
      then (
        let defs, typed =
          Scopes.defs_and_typed_program ~with_types ~raise ~options ~stdlib ~prg
        in
        defs, typed, [])
      else
        Scopes.defs_and_typed_program_and_scopes ~with_types ~raise ~options ~stdlib ~prg)


(** Used by CLI, formats all definitions, errors and warnings and returns strings *)
let get_scope_cli_result
    (raw_options : Raw_options.t)
    ~source_file
    ~display_format
    ~no_colour
    ~defs_only
  =
  Scopes.Api_helper.format_result ~display_format ~no_colour
  @@ fun ~raise ->
  let defs, _, scopes =
    get_scope_raw raw_options (From_file source_file) ~defs_only ~raise
  in
  defs, scopes


let get_scopes
    (raw_options : Raw_options.t)
    (code_input : BuildSystem.Source_input.code_input)
    (definitions : Def.t list)
    : scopes
  =
  Trace.try_with
    ~fast_fail:false
    (fun ~raise ~catch:_ ->
      with_code_input ~raw_options ~raise ~code_input ~f:(fun ~options ~syntax:_ ->
          Scopes.scopes ~definitions ~options))
    (fun ~catch:_ _ -> [])


(** Currently [Scopes.defs] result contains definitions from modules inside `mdef`.
    We want to search stuff only in `definitions`, so we traverse all local modules.
    TODO: change `Scopes.scopes_and_defs` so it produces list that we need *)
let unfold_defs (errors, warnings, { storage_warnings }, nested_defs_opt)
    : defs_and_diagnostics
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
  let definitions =
    match nested_defs_opt with
    | None -> []
    | Some nested_defs -> extract_defs nested_defs
  in
  { errors; warnings; storage_diagnostics = { storage_warnings }; definitions }


let storage_name = "storage"
let virtual_main_name = "lsp_virtual_main"

(** When compiling past the typed AST, we need to provide an entry-point, otherwise the
    compilation may fail. This function will generate a virtual entry-point with the name
    provided by [virtual_main_name], using the never type as the parameter, and
    [storage_type_name] as the storage. This will be type-checked using [context]. *)
let make_lsp_virtual_main
    ~raise
    ~(options : Compiler_options.t)
    ~(context : Ast_typed.signature)
    ~(syntax : Syntax_types.t)
    (storage_type_name : string)
    : Ast_typed.program * Ast_core.program
  =
  let main =
    Format.sprintf
      (match syntax with
      | CameLIGO -> "[@entry]\nlet %s (_ : never) (s : %s) : operation list * %s = [], s"
      | JsLIGO ->
        "@entry\nconst %s = (_: never, s: %s): [list<operation>, %s] => [list([]), s]")
      virtual_main_name
      storage_type_name
      storage_type_name
  in
  Ligo_compile.Utils.type_program_string ~raise ~options ~context syntax main


let find_storage =
  List.find_map ~f:(function
      | Variable vdef ->
        Option.some_if Caml.(vdef.name = storage_name && vdef.def_type = Global) vdef
      | Type _ | Module _ -> None)


let storage_diagnostics
    ?(json_download : bool option)
    ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
    ~(stdlib_context : Ast_typed.signature)
    ~(syntax : Syntax_types.t)
    (raw_options : Raw_options.t)
    (env : Ast_typed.signature)
    (prg : Ast_typed.declaration list)
    (defs : def list)
    : unit Lwt.t
  =
  let options =
    let protocol_version =
      Ligo_compile.Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
    Compiler_options.make
      ~raw_options
      ~syntax
      ~protocol_version
      ~has_env_comments:false
      ()
  in
  try%lwt
    (* Do we have a definition called "storage"? *)
    match find_storage defs with
    | None ->
      (* No storage, but let's try to proceed with the compilation anyway to show more
         diagnostics. This is done by compiling the virtual entry-point using the never
         type as the storage. *)
      let storage_type = "never" in
      let virtual_main_typed, _virtual_main_core =
        make_lsp_virtual_main ~raise ~options ~context:stdlib_context ~syntax storage_type
      in
      let prg_with_ep : Ast_typed.program =
        { pr_module = virtual_main_typed.pr_module @ prg
        ; pr_sig =
            { sig_items = virtual_main_typed.pr_sig.sig_items @ env.sig_items
            ; sig_sort = virtual_main_typed.pr_sig.sig_sort
            }
        }
      in
      let ast_aggregated =
        Ligo_compile.Of_typed.compile_expression_in_context
          ~self_pass:true
          ~self_program:true
          ~raise
          ~options:options.middle_end
          None
          prg_with_ep
          (Ast_typed.e_a_unit ~loc:Simple_utils.Location.dummy ())
      in
      let ast_expanded =
        Ligo_compile.Of_aggregated.compile_expression ~raise ast_aggregated
      in
      let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise ast_expanded in
      let open Lwt.Let_syntax in
      let%map _mich = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
      ()
    | Some storage ->
      (* Introduce a virtual entry-point using this storage. *)
      let storage_type =
        let core_to_string =
          Pretty.pretty_print_type_expression
            { indent = 2; width = Int.max_value }
            ~syntax
        in
        (* We might not have the type, or we might not be able to pretty-print it for
           whatever reason. Let's not fail, but rather just use the never type. *)
        match
          match storage.t with
          | Core t -> core_to_string t
          | Resolved t ->
            core_to_string @@ Checking.untype_type_expression ~use_orig_var:true t
          | Unresolved -> `Ok "never"
        with
        | `Nonpretty _ -> "never"
        | `Ok t -> t
      in
      let context : Ast_typed.signature =
        { sig_items = env.sig_items @ stdlib_context.sig_items; sig_sort = env.sig_sort }
      in
      let virtual_main_typed, _virtual_main_core =
        make_lsp_virtual_main ~raise ~options ~context ~syntax storage_type
      in
      let prg_with_ep : Ast_typed.program =
        { pr_module = virtual_main_typed.pr_module @ prg
        ; pr_sig =
            { sig_items = virtual_main_typed.pr_sig.sig_items @ env.sig_items
            ; sig_sort = virtual_main_typed.pr_sig.sig_sort
            }
        }
      in
      Lwt.map ignore
      @@ snd
           ~raise
           (Ligo_api.Compile.storage_of_typed
              { raw_options with json_download }
              ~syntax
              prg_with_ep
              storage.name
              (Some storage.range)
              ~amount:"0"
              ~balance:"0"
              `Text)
  with
  | _exn -> Lwt.return ()


(** Returns [true] if this is an expensive check that might require downloading from the
    internet and thus lag the language server, or [false] otherwise. *)
let is_metadata_check : Main_warnings.all -> bool = function
  | `Self_ast_aggregated_warning_unused _
  | `Self_ast_aggregated_warning_muchused _
  | `Self_ast_aggregated_warning_unused_rec _
  | `Self_ast_aggregated_metadata_invalid_type _
  | `Checking_ambiguous_constructor_expr _
  | `Checking_ambiguous_constructor_pat _
  | `Nanopasses_attribute_ignored _
  | `Nanopasses_infinite_for_loop _
  | `Self_ast_imperative_warning_deprecated_polymorphic_variable _
  | `Main_view_ignored _
  | `Main_entry_ignored _
  | `Michelson_typecheck_failed_with_different_protocol _
  | `Jsligo_deprecated_failwith_no_return _
  | `Jsligo_deprecated_toplevel_let _
  | `Jsligo_unreachable_code _
  | `Use_meta_ligo _
  | `Self_ast_aggregated_warning_bad_self_type _ -> false
  | `Metadata_cannot_parse _
  | `Metadata_no_empty_key _
  | `Metadata_tezos_storage_not_found _
  | `Metadata_not_valid_URI _
  | `Metadata_slash_not_valid_URI _
  | `Metadata_invalid_JSON _
  | `Metadata_error_JSON_object _
  | `Metadata_hash_fails _
  | `Metadata_error_download _
  | `Metadata_json_download _ -> true


(** Used by LSP, we're trying to get as many errors/warnings as possible *)
let get_defs_and_diagnostics
    ?(json_download : bool option)
    (raw_options : Raw_options.t)
    (code_input : BuildSystem.Source_input.code_input)
    : defs_and_diagnostics Lwt.t
  =
  let with_types = raw_options.with_types in
  Lwt.map unfold_defs
  @@ Trace.try_with_lwt
       ~fast_fail:false
       (fun ~raise ~catch ->
         let open Lwt.Let_syntax in
         let%map errors, warnings, storage_diagnostics, v =
           with_code_input
             ~raw_options
             ~raise
             ~code_input
             ~f:(fun ~options ~syntax ~stdlib ~prg ->
               let defs, prg =
                 Scopes.defs_and_typed_program ~raise ~with_types ~options ~stdlib ~prg
               in
               let%map errors, warnings, storage_diagnostics =
                 match prg with
                 | None -> Lwt.return ([], [], empty_storage_diagnostics)
                 | Some (env, prg) ->
                   let stdlib_context = (fst stdlib).pr_sig in
                   Trace.try_with_lwt
                     ~fast_fail:false
                     (fun ~raise ~catch ->
                       let%map () =
                         storage_diagnostics
                           ?json_download
                           ~raise
                           ~stdlib_context
                           ~syntax
                           raw_options
                           env
                           prg
                           defs
                       in
                       (* The only thing we really want to cache are those checks that
                          might require a download, so we partition them into storage and
                          ordinary warnings. *)
                       let storage_warnings, other_warnings =
                         List.partition_tf (catch.warnings ()) ~f:is_metadata_check
                       in
                       catch.errors (), other_warnings, { storage_warnings })
                     (fun ~catch e ->
                       let storage_warnings, other_warnings =
                         List.partition_tf (catch.warnings ()) ~f:is_metadata_check
                       in
                       Lwt.return
                         (e :: catch.errors (), other_warnings, { storage_warnings }))
               in
               errors, warnings, storage_diagnostics, defs)
         in
         ( errors @ catch.errors ()
         , warnings @ catch.warnings ()
         , storage_diagnostics
         , Some v ))
       (fun ~catch e ->
         Lwt.return
           (e :: catch.errors (), catch.warnings (), empty_storage_diagnostics, None))
