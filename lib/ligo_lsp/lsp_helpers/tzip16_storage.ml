open Core
open Scopes.Types

(** Detects all contracts appearing in the module *)
let rec get_all_contract_sigs (env : Ast_typed.signature) : Ast_typed.contract_sig list =
  let this_sig =
    match env.sig_sort with
    | Ss_contract s -> Some s
    | Ss_module -> None
  in
  let child_sigs =
    List.concat_map env.sig_items ~f:(fun sig_item ->
        match sig_item.wrap_content with
        | S_module (_, s) | S_module_type (_, s) -> get_all_contract_sigs s
        | S_value _ | S_type _ | S_type_var _ -> [])
  in
  Option.to_list this_sig @ child_sigs


type var_declaration_info =
  { binders : Ast_typed.type_expression Ligo_prim.Binder.t list
  ; value : Ast_typed.expression
  ; has_tzip16_compatible_attr : bool
  }

let rec get_all_variables_info (prg : Ast_typed.declaration list)
    : var_declaration_info Sequence.t
  =
  Sequence.concat_map (Sequence.of_list prg) ~f:(fun decl ->
      match decl.wrap_content with
      | D_value { binder; expr; attr } ->
        Sequence.singleton
          { binders = [ binder ]
          ; value = expr
          ; has_tzip16_compatible_attr = attr.tzip16_compatible
          }
      | D_irrefutable_match { pattern; expr; attr } ->
        Sequence.singleton
          { binders = Ast_core.Pattern.binders pattern
          ; value = expr
          ; has_tzip16_compatible_attr = attr.tzip16_compatible
          }
      | D_module { module_; _ } ->
        (match module_.module_content with
        | M_struct decls -> get_all_variables_info decls
        | M_variable _ | M_module_path _ -> Sequence.empty)
      | D_module_include _ | D_type _ | D_signature _ | D_import _ -> Sequence.empty)


(** Makes best effort to tell whether this type is some storage type or not. *)
let is_potential_storage_type
    (type_expr : Ast_typed.type_expression)
    (known_storage_types : Ast_typed.type_expression list)
    : bool
  =
  let has_metadata =
    match type_expr.type_content with
    | T_record r -> Map.mem r.fields (Ligo_prim.Label.create "metadata")
    | _ -> false
  in
  let has_known_type () =
    List.mem known_storage_types type_expr ~equal:Ast_typed.equal_ty_expr
  in
  has_metadata && has_known_type ()


let location_is_from_file (path : Path.t) : Location.t -> bool = function
  | Virtual _ -> false
  | File reg -> String.equal (Path.to_string path) reg#file


(** Extract all potential storages from a file *)
let vars_to_mark_as_tzip16_compatible (cur_file : Path.t) (prg : Ast_typed.program)
    : Ast_typed.expression_variable list
  =
  let all_storage_types =
    List.map (get_all_contract_sigs prg.pr_sig) ~f:(fun contract_sig ->
        contract_sig.storage)
  in
  get_all_variables_info prg.pr_module
  |> Sequence.filter_map ~f:(fun decl_info ->
         match decl_info.binders with
         | [] -> None
         | _ :: _ :: _ ->
           (* Multiple binders may mean that several variables are declared
              at the same line, and such case is hard to process when "Add
              TZIP-16-compatible attribute" code lens will be invoked. Let's
              skip this rare case *)
           None
         | [ binder ] ->
           if (not decl_info.has_tzip16_compatible_attr)
              && location_is_from_file
                   cur_file
                   (Ligo_prim.Value_var.get_location binder.var)
              && is_potential_storage_type binder.ascr all_storage_types
           then Some binder.var
           else None)
  |> Sequence.to_list


let values_for_tzip16_check (cur_file : Path.t) (prg : Ast_typed.declaration list)
    : (Ast_typed.expression * Location.t) Sequence.t
  =
  get_all_variables_info prg
  |> Sequence.filter_map ~f:(fun decl_info ->
         match decl_info.binders with
         | binder :: _ when decl_info.has_tzip16_compatible_attr ->
           Some (decl_info.value, Ligo_prim.Value_var.get_location binder.var)
         | _ -> None)
  |> Sequence.filter ~f:(fun (_expr, loc) -> location_is_from_file cur_file loc)


type download_options = Ligo_run.Of_michelson.Checks.Json_download.options

let create_download_options ~(enabled : bool) ~(timeout_sec : float) : download_options =
  if enabled
  then
    `Enabled
      { cache = Ligo_run.Of_michelson.Checks.Json_download.use_lru_cache ()
      ; timeout_sec = Some timeout_sec
      }
  else `Disabled


let check_typed_storage
    ~(options : Compiler_options.t)
    ~(json_download : download_options)
    ~raise
    ~(constants : string list)
    ~(loc : Location.t)
    (prg : Ast_typed.program)
    (storage : Ast_typed.expression)
    : unit Lwt.t
  =
  let open Lwt.Let_syntax in
  let aggregated_storage =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~self_pass:true
      ~self_program:true
      ~raise
      ~options:options.middle_end
      None
      prg
      storage
  in
  let expanded_storage =
    Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_storage
  in
  let mini_c_storage =
    Ligo_compile.Of_expanded.compile_expression ~raise expanded_storage
  in
  let%bind compiled_storage =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_storage
  in
  let module Run = Ligo_run.Of_michelson in
  let dry_run_options : Run.dry_run_options =
    Run.(
      (* There is no way how environment can affect metadata since metadata
         carries bytes values, so passing arbitrary values below *)
      { now = None
      ; amount = "0"
      ; balance = "0"
      ; sender = None
      ; source = None
      ; parameter_ty = None
      })
  in
  let%bind michelson_value =
    let%bind options = Run.make_dry_run_options ~raise ~constants dry_run_options in
    Run.evaluate_expression ~raise ~options compiled_storage.expr compiled_storage.expr_ty
  in
  Run.Checks.tzip16_check_storage
    ~raise
    ~loc
    ~json_download
    ~storage_type:compiled_storage.expr_ty
    michelson_value


let check_typed_program
    ~(options : Compiler_options.t)
    ~(json_download : download_options)
    ~raise
    ~(constants : string list)
    ~(cur_file : Path.t)
    (prg : Ast_typed.program)
    : unit Lwt.t
  =
  let storages = values_for_tzip16_check cur_file prg.pr_module in
  Lwt_list.iter_p
    (fun (expr, loc) ->
      check_typed_storage ~options ~json_download ~raise ~loc ~constants prg expr)
    (Sequence.to_list storages)
