module Raw_options = Compiler_options.Raw_options
module Trace = Simple_utils.Trace
module SMap = Map.Make (String)
open Scopes.Types

(** Holds data collected from [Scopes] or from [following_passes_diagnostics]. [errors]
    and [warnings] can come from [Scopes] module (which runs compiler up to
    [Self_ast_typed] pass), or from [following_passes_diagnostics] (collected in this
    module). See also: [Ligo_interface.file_data_case]. *)
type defs_and_diagnostics =
  { errors : Main_errors.all list
  ; warnings : Main_warnings.all list
  ; definitions : Def.definitions
  ; potential_tzip16_storages : Ast_typed.expression_variable list
  ; lambda_types : Ast_typed.ty_expr LMap.t
  }

(** Compiles [code_input] from the source file up to the typed AST, providing all of its
    module dependencies and stdlib to the callback [f]. *)
let with_code_input
    :  f:
         (options:Compiler_options.middle_end
          -> syntax:Syntax_types.t
          -> stdlib:Ast_typed.program * Ast_core.program
          -> prg:Ast_core.program
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
  let options = Compiler_options.make ~raw_options ~syntax () in
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
  f ~options:options.middle_end ~stdlib ~prg ~syntax ~module_deps


(** Used by CLI, formats all definitions, errors and warnings and returns strings. *)
let get_scope_cli_result
    (raw_options : Raw_options.t)
    ~source_file
    ~display_format
    ~no_colour
    ~defs_only
  =
  Scopes.Api_helper.format_result ~display_format ~no_colour
  @@ fun ~raise ->
  let v =
    let with_types = raw_options.with_types in
    with_code_input
      ~raw_options
      ~raise
      ~code_input:(From_file source_file)
      ~f:(fun ~options ~syntax:_ ~stdlib ~prg ~module_deps ->
        Scopes.run ~with_types ~raise ~options ~stdlib ~prg ~module_deps)
  in
  { v with inlined_scopes = (if defs_only then Lazy.from_val [] else v.inlined_scopes) }


(** Internal function used by [get_defs_and_diagnostics] to create [defs_and_diagnostics]
    from that output. Doesn't do much besides deduping diagnostics and wrapping the data
    in the structure. *)
let make_defs_and_diagnostics
    (errors, warnings, defs_opt, potential_storage_vars, lambda_types)
    : defs_and_diagnostics
  =
  let errors = List.dedup_and_sort ~compare:Caml.compare errors in
  let warnings = List.dedup_and_sort ~compare:Caml.compare warnings in
  let definitions = Option.value ~default:{ definitions = [] } defs_opt in
  let potential_tzip16_storages = potential_storage_vars in
  { errors; warnings; definitions; potential_tzip16_storages; lambda_types }


(** We want to produce all possible warnings/errors in following passes (everything after checking pass).
    However, aggregation gets rid of unused declarations. Take this contract:

    {[
      [@deprecated "This value is deprecated"]
      let some_deprecated_value = 42

      module A = struct
        [@entry]
        let entrypoint (p : int) (s : int) : operation list * int =
          let _ = some_deprecated_value in
          [], p + s
      end

      [@entry]
      let another_entrypoint (p : int) (s : int) : operation list * int =
        let _ = some_deprecated_value in
        [], p + s
    }]
    This contract has 2 entrypoints. If we compile only one of them then we lose a deprecation warning from the other one.

    After [self_ast_typed] pass all declarations with [@entry] attribute are bundled into one large entrypoint with [$main] name.
    Let's create an entrypoint that will mention all the entrypoints of the contract. For the contract above it'll look like:

    {[
      let $common_lsp_main () () : operation list * unit =
        let _ = ignore A.$main in
        let _ = ignore $main in
        [], ()
    ]} *)
let make_common_entrypoint
    (modules_with_sigs : (Ligo_prim.Module_var.t list * Ast_typed.contract_sig) list)
    : Ast_typed.module_ (* Common entrypoint *)
      * Ast_typed.expression (* Expression to compile in context *)
      * Ast_typed.sig_items (* Common entrypoint signature *)
      * Ast_typed.contract_sig (* Contract signature of common entrypoint *)
  =
  let open Ligo_prim in
  let open Ast_typed in
  let loc = Location.generated in
  let wildcard = Value_var.fresh ~loc ~name:"_" in
  let unit_type = t_unit ~loc () in
  let param_stor_type = t_pair ~loc unit_type unit_type in
  let ep_type = Misc.build_entry_type unit_type unit_type in
  let oplist = t_list ~loc (t_operation ~loc ()) in
  let oplist_storage = t_pair ~loc oplist unit_type in
  let init =
    e_record
      ~loc
      (Record.record_of_tuple
         [ e_constant ~loc { cons_name = C_LIST_EMPTY; arguments = [] } oplist
         ; e_literal ~loc Literal_unit unit_type
         ])
      oplist_storage
  in
  let result =
    List.fold
      modules_with_sigs
      ~init
      ~f:(fun let_result (module_path, { parameter; storage }) ->
        let let_binder = Pattern.var ~loc (Binder.make (wildcard ()) unit_type) in
        let rhs =
          let cur_ep_type = Misc.build_entry_type parameter storage in
          let cur_ep =
            e_module_accessor
              ~loc
              { module_path; element = Magic_vars.generated_main }
              cur_ep_type
          in
          let ignore_expr =
            let forall_var = Type_var.of_input_var ~loc "a" in
            e_variable
              ~loc
              (Value_var.of_input_var ~loc "ignore")
              (t_for_all
                 ~loc
                 forall_var
                 Type
                 (t_arrow ~loc (t_variable ~loc forall_var ()) unit_type ()))
          in
          e_a_application
            ~loc
            (e_type_inst
               ~loc
               { forall = ignore_expr; type_ = cur_ep_type }
               (t_arrow ~loc cur_ep_type unit_type ()))
            cur_ep
            unit_type
        in
        let attributes = ValueAttr.default_attributes in
        e_let_in ~loc { let_binder; let_result; rhs; attributes } oplist_storage)
  in
  let lamb =
    e_lambda
      ~loc
      { binder = Param.make (wildcard ()) param_stor_type
      ; output_type = oplist_storage
      ; result
      }
      ep_type
  in
  let decl_var = Magic_vars.common_lsp_main in
  let decl =
    Location.wrap ~loc
    @@ D_value
         { binder = Binder.make decl_var ep_type
         ; expr = lamb
         ; attr = Value_attr.default_attributes
         }
  in
  let ep_expr = e_variable ~loc decl_var ep_type in
  let sig_items =
    [ Location.wrap ~loc @@ S_value (decl_var, ep_type, Sig_item_attr.default_attributes)
    ]
  in
  let contract_sig = { parameter = unit_type; storage = unit_type } in
  [ decl ], ep_expr, sig_items, contract_sig


(** Compiles (Ast_typed -> Ast_aggregated -> Ast_expanded -> Mini_c -> Michelson) a
    virtual entry-point that references all other entrypoints in the file with the program
    as a context. This is enough to raise the unused variable warnings, warnings about
    wrong storage metadata type and some others. *)
let following_passes_diagnostics
    ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
    ~(stdlib_program : Ast_typed.program)
    ~(syntax : Syntax_types.t)
    ~(path : Path.t)
    ~(tzip16_download_options : Tzip16_storage.download_options)
    (raw_options : Raw_options.t)
    ({ pr_module; pr_sig = { sig_items; sig_sort } } : Ast_typed.program)
    : unit Lwt.t
  =
  let options =
    Compiler_options.make
      ~raw_options
      ~syntax
      ~has_env_comments:false
      ()
  in
  let prg : Ast_typed.program =
    { pr_module = stdlib_program.pr_module @ pr_module
    ; pr_sig = { sig_sort; sig_items = stdlib_program.pr_sig.sig_items @ sig_items }
    }
  in
  let log_diagnostics errors warnings =
    List.iter warnings ~f:raise.warning;
    List.iter errors ~f:raise.log_error
  in
  let prg =
    (* TODO: Partially redundant? Already performed by [Types_pass.Typing_env.
       self_ast_typed_pass], but we do it again since now we have a virtual entry...
       nonetheless, removing either this or that pass will prevent some diagnostics from
       being shown. *)
    let prg, errors, warnings =
      Trace.try_with
        ~fast_fail:false
        (fun ~raise ~catch ->
          let prg =
            Trace.trace ~raise Main_errors.self_ast_typed_tracer
            @@ Self_ast_typed.all_program prg
          in
          prg, catch.errors (), catch.warnings ())
        (fun ~catch e -> prg, e :: catch.errors (), catch.warnings ())
    in
    log_diagnostics errors warnings;
    prg
  in
  let modules_with_sigs = Ligo_compile.Of_typed.get_modules_with_entries prg in
  let decls, ep_expr, sig_items_ep, contract_sig =
    make_common_entrypoint modules_with_sigs
  in
  let prg =
    let pr_module = prg.pr_module @ decls in
    let pr_sig =
      Ast_typed.
        { sig_items = prg.pr_sig.sig_items @ sig_items_ep
        ; sig_sort = Ss_contract contract_sig
        }
    in
    Ast_typed.{ pr_module; pr_sig }
  in
  let ast_aggregated =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~self_pass:true
      ~self_program:true
      ~raise
      ~options:options.middle_end
      (Some contract_sig)
      prg
      ep_expr
  in
  let ast_expanded =
    Ligo_compile.Of_aggregated.compile_expression ~raise ast_aggregated
  in
  let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise ast_expanded in
  let open Lwt.Let_syntax in
  let%bind _mich = Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  Tzip16_storage.check_typed_program
    ~options
    ~json_download:tzip16_download_options
    ~raise
    ~constants:[] (* TODO #2159: fill them *)
    ~cur_file:path
    prg


(** Calculates the [Def.t]s and diagnostics for a given file. [code] is the source code of
    this file. [logger] is a function that will log messages (see
    [Requests.Handler.send_log_msg] for a good choice of logger).
    [tzip16_download_options] is used to control whether to display diagnostics that
    require downloads from TZIP-16-compatible storages. *)
let get_defs_and_diagnostics
    ~(logger : type_:Lsp.Types.MessageType.t -> string -> unit Lwt.t)
    ~(tzip16_download_options : Tzip16_storage.download_options)
    (raw_options : Raw_options.t)
    (path : Path.t)
    (code : string)
    : defs_and_diagnostics Lwt.t
  =
  let with_types = raw_options.with_types in
  Lwt.map make_defs_and_diagnostics
  @@ Trace.try_with_lwt
       ~fast_fail:false
       (fun ~raise ~catch ->
         let open Lwt.Let_syntax in
         let%map errors, warnings, v, storage_vars, lambda_types =
           with_code_input
             ~raw_options
             ~raise
             ~code_input:(Raw_input_lsp { file = Path.to_string path; code })
             ~f:(fun ~options ~syntax ~stdlib ~prg ~module_deps ->
               let { Scopes.definitions; program; inlined_scopes = _; lambda_types } =
                 Scopes.run ~raise ~with_types ~options ~stdlib ~prg ~module_deps
               in
               let potential_tzip16_storages =
                 match program with
                 | None -> []
                 | Some program ->
                   Tzip16_storage.vars_to_mark_as_tzip16_compatible path program
               in
               let%map errors, warnings =
                 match program with
                 | None -> Lwt.return ([], [])
                 | Some program ->
                   let stdlib_program = fst stdlib in
                   (try%lwt
                      Trace.try_with_lwt
                        ~fast_fail:false
                        (fun ~raise ~catch ->
                          let%map () =
                            following_passes_diagnostics
                              ~raise
                              ~stdlib_program
                              ~syntax
                              ~path
                              ~tzip16_download_options
                              raw_options
                              program
                          in
                          catch.errors (), catch.warnings ())
                        (fun ~catch e ->
                          Lwt.return (e :: catch.errors (), catch.warnings ()))
                    with
                   | exn ->
                     let stack = Printexc.get_backtrace () in
                     let%bind () =
                       logger ~type_:Error
                       @@ Format.asprintf
                            "Unexpected exception in following passes: %a\n%s"
                            Exn.pp
                            exn
                            stack
                     in
                     Lwt.return (catch.errors (), catch.warnings ()))
               in
               errors, warnings, definitions, potential_tzip16_storages, lambda_types)
         in
         ( errors @ catch.errors ()
         , warnings @ catch.warnings ()
         , Some v
         , storage_vars
         , lambda_types ))
       (fun ~catch e ->
         Lwt.return (e :: catch.errors (), catch.warnings (), None, [], LMap.empty))
