module Raw_options = Compiler_options.Raw_options
module Trace = Simple_utils.Trace
module SMap = Map.Make (String)
open Scopes.Types

(** This is used by the LSP. Note: defs must be unfolded before constructing this,
    see [unfold_defs].
    [errors] and [warnings] can come from [Scopes] module (which runs compiler up to self_ast_typed pass)
    , or from [following_passes_diagnostics] (collected in this module) *)
type defs_and_diagnostics =
  { errors : Main_errors.all list
  ; warnings : Main_warnings.all list
  ; definitions : Def.t list
  }

let with_code_input
    :  f:
         (options:Compiler_options.middle_end
          -> syntax:Syntax_types.t
          -> stdlib:Ast_typed.program * Ast_core.program
          -> prg:Ast_core.module_
          -> module_deps:string SMap.t
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

  (* Here we need to extract module dependencies in order to show
    to the user file paths of mangled modules *)
  let prg, module_deps =
    (* While building [Build.qualified_core] we need a smaller AST (without stdlib)
       that is the reason for no_stdlib as true *)
    let options = Compiler_options.set_no_stdlib options true in
    (* Let's build a dependency graph for the given code input.
      It will collect a [(file_name * module_name) list] which we can use
      to create a module name to file name mapping.*)
    let module_deps =
      let module Deps_map = Stdlib__Map.Make (Stdlib__String) in
      Build.dependency_graph ~raise ~options code_input
      |> snd
      |> Deps_map.to_seq
      |> Seq.fold_left (fun acc (_, (_, _, _, lst)) -> lst :: acc) []
      |> List.concat
      |> List.fold_left ~init:SMap.empty ~f:(fun acc (file_name, mangled_name) ->
             match SMap.add ~key:mangled_name ~data:file_name acc with
             | `Duplicate -> acc
             | `Ok added -> added)
    in
    ( (match code_input with
      | From_file _ | HTTP _ -> Build.qualified_core ~raise ~options code_input
      | Raw file -> Build.qualified_core_from_string ~raise ~options file
      | Raw_input_lsp { file; code } ->
        Build.qualified_core_from_raw_input ~raise ~options file code)
    , module_deps )
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
  f ~options:options.middle_end ~stdlib ~prg ~syntax ~module_deps


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
    ~f:(fun ~options ~syntax:_ ~stdlib ~prg ~module_deps ->
      if defs_only
      then (
        let defs, typed =
          Scopes.defs_and_typed_program
            ~with_types
            ~raise
            ~options
            ~stdlib
            ~prg
            ~module_deps
        in
        defs, typed, [])
      else
        Scopes.defs_and_typed_program_and_scopes
          ~with_types
          ~raise
          ~options
          ~stdlib
          ~prg
          ~module_deps)


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
let unfold_defs (errors, warnings, nested_defs_opt) : defs_and_diagnostics =
  let errors = List.dedup_and_sort ~compare:Caml.compare errors in
  let warnings = List.dedup_and_sort ~compare:Caml.compare warnings in
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


let storage_name = "storage"
let virtual_main_name = "lsp_virtual_main"

(** When compiling past the typed AST, we need to provide an entry-point, otherwise the
    compilation may fail. This function will generate a virtual entry-point with the name
    provided by [virtual_main_name], using the never type as the parameter, and
    [storage_type_expr] as the storage. This will be type-checked using [context].

    Can be used with custom storage type to proceed with compiling the storage expr. *)
let make_lsp_virtual_main
    ~raise
    ~(options : Compiler_options.t)
    ~(context : Ast_typed.signature)
    ~(syntax : Syntax_types.t)
    (storage_type_expr : string)
    : Ast_typed.program * Ast_core.program
  =
  let main =
    Format.sprintf
      (match syntax with
      | CameLIGO -> "[@entry]\nlet %s (_ : never) (s : %s) : operation list * %s = [], s"
      | JsLIGO ->
        "@entry\nconst %s = (_: never, s: %s): [list<operation>, %s] => [list([]), s]")
      virtual_main_name
      storage_type_expr
      storage_type_expr
  in
  let typed, core =
    Ligo_compile.Utils.type_program_string ~raise ~options ~context syntax main
  in
  let sig_sort =
    Option.value ~default:Ast_typed.Ss_module
    @@ Trace.to_option
    @@ Checking.eval_signature_sort
         ~options:options.middle_end
         ~loc:Location.dummy
         { typed.pr_sig with sig_items = typed.pr_sig.sig_items @ context.sig_items }
  in
  { typed with pr_sig = { typed.pr_sig with sig_sort } }, core


(* Compiles (Ast_typed -> Ast_aggrefgated -> Ast_expanded -> mini_c -> michelson)
  the unit expression with the program as a context. This is enough to raise the
  unused variable warnings, warnings about wrong storage metadata type and some others.
  This require our file to have an entrypoint, so we can add a virtual one. *)
let following_passes_diagnostics
    ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
    ~(stdlib_context : Ast_typed.signature)
    ~(syntax : Syntax_types.t)
    (raw_options : Raw_options.t)
    (env : Ast_typed.signature)
    (prg : Ast_typed.declaration list)
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
  let contract_sig, pr_sig_items, prg =
    match env.sig_sort with
    | Ss_contract contract_sig ->
      (* There is an entrypoint in a contract, so we know the storage *)
      Some contract_sig, env.sig_items, prg
    | Ss_module ->
      let storage_type = "never" in
      let context = { stdlib_context with sig_items = env.sig_items } in
      let virtual_main_typed, _virtual_main_core =
        make_lsp_virtual_main ~raise ~options ~context ~syntax storage_type
      in
      ( (match virtual_main_typed.pr_sig.sig_sort with
        | Ss_module -> None (* Should be impossible since virtual main is an entrypoint *)
        | Ss_contract contract_sig -> Some contract_sig)
      , env.sig_items @ virtual_main_typed.pr_sig.sig_items
      , prg @ virtual_main_typed.pr_module )
  in
  let prg : Ast_typed.program =
    { pr_module = prg
    ; pr_sig =
        { sig_sort =
            Option.value_map
              ~default:Ast_typed.Ss_module
              ~f:(fun x -> Ss_contract x)
              contract_sig
        ; sig_items = pr_sig_items
        }
    }
  in
  let errors, warnings =
    match contract_sig with
    | Some contract_sig ->
      Trace.try_with
        ~fast_fail:false
        (fun ~raise ~catch ->
          let prg =
            Trace.trace ~raise Main_errors.self_ast_typed_tracer
            @@ Self_ast_typed.all_program prg
          in
          List.iter
            ~f:(fun module_path ->
              let _ast_aggregated =
                Ligo_compile.Of_typed.apply_to_entrypoint_with_contract_type
                  ~raise
                  ~options:options.middle_end
                  prg
                  module_path
                  contract_sig
              in
              ())
            (Ligo_compile.Of_typed.get_modules_with_entries prg);
          catch.errors (), catch.warnings ())
        (fun ~catch e -> e :: catch.errors (), catch.warnings ())
    | None -> [], []
  in
  let ast_aggregated =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~self_pass:true
      ~self_program:true
      ~raise
      ~options:options.middle_end
      contract_sig
      prg
      (Ast_typed.e_a_unit ~loc:Simple_utils.Location.dummy ())
  in
  let ast_expanded =
    Ligo_compile.Of_aggregated.compile_expression ~raise ast_aggregated
  in
  let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise ast_expanded in
  let open Lwt.Let_syntax in
  let%map _mich = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  List.iter warnings ~f:raise.warning;
  List.iter errors ~f:raise.error


(** Used by LSP, we're trying to get as many errors/warnings as possible *)
let get_defs_and_diagnostics
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
         let%map errors, warnings, v =
           with_code_input
             ~raw_options
             ~raise
             ~code_input
             ~f:(fun ~options ~syntax ~stdlib ~prg ~module_deps ->
               let defs, prg =
                 Scopes.defs_and_typed_program
                   ~raise
                   ~with_types
                   ~options
                   ~stdlib
                   ~prg
                   ~module_deps
               in
               let%map errors, warnings =
                 match prg with
                 | None -> Lwt.return ([], [])
                 | Some (env, prg) ->
                   let stdlib_context = (fst stdlib).pr_sig in
                   Trace.try_with_lwt
                     ~fast_fail:false
                     (fun ~raise ~catch ->
                       let%map () =
                         following_passes_diagnostics
                           ~raise
                           ~stdlib_context
                           ~syntax
                           raw_options
                           env
                           prg
                       in
                       catch.errors (), catch.warnings ())
                     (fun ~catch e ->
                       Lwt.return (e :: catch.errors (), catch.warnings ()))
               in
               errors, warnings, defs)
         in
         errors @ catch.errors (), warnings @ catch.warnings (), Some v)
       (fun ~catch e -> Lwt.return (e :: catch.errors (), catch.warnings (), None))
