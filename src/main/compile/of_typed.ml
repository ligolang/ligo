open Simple_utils.Trace
open Ast_typed
open Aggregation
open Main_errors

module Var = Simple_utils.Var

module SMap = Map.Make(String)

let compile_program ~raise : Ast_typed.program -> Ast_typed.expression Ast_aggregated.program = fun p ->
  trace ~raise aggregation_tracer @@ Aggregation.compile_program p

let compile_expression_in_context ~raise ~add_warning ~options : Ast_typed.expression -> Ast_typed.expression Ast_aggregated.program -> Ast_aggregated.expression =
  fun exp prg ->
    let x = Aggregation.compile_expression_in_context exp prg in
    trace ~raise self_ast_aggregated_tracer @@ Self_ast_aggregated.all_expression ~add_warning ~options x

let compile_expression ~raise ~add_warning ~options : Ast_typed.expression -> Ast_aggregated.expression = fun e ->
  let x = trace ~raise aggregation_tracer @@ compile_expression e in
  trace ~raise self_ast_aggregated_tracer @@ Self_ast_aggregated.all_expression ~add_warning ~options x

let apply_to_entrypoint_contract ~raise ~add_warning ~options : Ast_typed.program -> Ast_typed.expression_variable -> Ast_aggregated.expression =
    fun prg entrypoint ->
  let aggregated_prg = compile_program ~raise prg in
  let Self_ast_typed.Helpers.{parameter=p_ty ; storage=s_ty} =
    trace ~raise self_ast_typed_tracer @@ Self_ast_typed.Helpers.fetch_contract_type entrypoint prg
  in
  let ty = t_arrow (t_pair p_ty s_ty) (t_pair (t_list (t_operation ())) s_ty) () in
  let var_ep = Ast_typed.(e_a_variable entrypoint ty) in
  compile_expression_in_context ~raise ~add_warning ~options var_ep aggregated_prg

let apply_to_entrypoint_view ~raise ~add_warning ~options : Ast_typed.program -> Ast_typed.expression_variable list -> Ast_aggregated.expression =
    fun prg views ->
  let aggregated_prg = compile_program ~raise prg in
  let aux : int -> expression_variable -> (label * expression) = fun i view ->
    let Self_ast_typed.Helpers.{arg=a_ty ; storage=s_ty ; return=r_ty}, _ =
      trace ~raise self_ast_typed_tracer @@ Self_ast_typed.Helpers.fetch_view_type view prg in
    let ty = t_arrow (t_pair a_ty s_ty) r_ty () in
    Ast_typed.Label (string_of_int i), Ast_typed.(e_a_variable view ty)
  in
  let tuple_view = Ast_typed.ez_e_a_record ~layout:L_comb (List.mapi ~f:aux views) in
  compile_expression_in_context ~raise ~add_warning ~options tuple_view aggregated_prg

let apply_to_entrypoint ~raise ~add_warning ~options : Ast_typed.program -> string -> Ast_aggregated.expression =
    fun prg entrypoint ->
  let aggregated_prg = compile_program ~raise prg in
  let v = Ast_typed.ValueVar.of_input_var entrypoint in
  let ty, _ =
    trace ~raise self_ast_typed_tracer @@ Self_ast_typed.Helpers.fetch_entry_type entrypoint prg in
  let var_ep = Ast_typed.(e_a_variable v ty) in
  compile_expression_in_context ~raise ~add_warning ~options var_ep aggregated_prg

let assert_equal_contract_type ~raise : Simple_utils.Runned_result.check_type -> Ast_typed.expression_variable -> Ast_typed.program -> Ast_typed.expression -> unit  =
    fun c entry contract param ->
  let entry_point = trace_option ~raise main_entrypoint_not_found (Ast_typed.get_entry contract entry) in
  trace ~raise (check_typed_arguments_tracer c) (
    fun ~raise ->
    match entry_point.type_expression.type_content with
    | T_arrow {type1=args;type2=_} -> (
        match args.type_content with
        | T_record m when LMap.cardinal m.content = 2 -> (
          let {associated_type=param_exp;_} = LMap.find (Label "0") m.content in
          let {associated_type=storage_exp;_} = LMap.find (Label "1") m.content in
            match c with
            | Check_parameter -> trace ~raise checking_tracer @@ Checking.assert_type_expression_eq entry_point.location (param_exp, param.type_expression)
            | Check_storage   -> trace ~raise checking_tracer @@ Checking.assert_type_expression_eq entry_point.location (storage_exp, param.type_expression)
        )
        | _ -> raise.raise @@ main_entrypoint_not_a_function )
    | _ -> raise.raise @@ main_entrypoint_not_a_function
  )

let get_views : Ast_typed.program -> (expression_variable * location) list = fun p ->
  let f : (expression_variable * location) list -> declaration -> (expression_variable * location) list =
    fun acc {wrap_content=decl ; location=_ } ->
      match decl with
      | Declaration_constant { binder ; expr=_ ; attr } when attr.view -> (binder.var, Ast_typed.ValueVar.get_location binder.var)::acc
      (* TODO: check for [@view] attributes in the AST and warn if [@view] is used anywhere other than top-level
        (e.g. warn if [@view] is used inside a module)
        Write a pass to check for known attributes (source_attributes -> known_attributes)
        *)
      (* | Declaration_module { module_binder=_ ; module_ ; module_attr=_} -> get_views module_ @ acc *)
      | _ -> acc
  in
  List.fold ~init:[] ~f p

let list_declarations (m : Ast_typed.module_) : expression_variable list =
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match el.wrap_content with
      | Declaration_constant {binder;attr;_} when attr.public -> binder.var::prev
      | _ -> prev)
    ~init:[] m

let list_type_declarations (m : Ast_typed.module_) : type_variable list =
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match el.wrap_content with
      | Declaration_type {type_binder;type_attr;_} when type_attr.public -> type_binder::prev
      | _ -> prev)
    ~init:[] m

let list_mod_declarations (m : Ast_typed.module_) : module_variable list =
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match el.wrap_content with
      | Declaration_module {module_binder;module_attr;_} when module_attr.public -> module_binder::prev
      | _ -> prev)
    ~init:[] m
