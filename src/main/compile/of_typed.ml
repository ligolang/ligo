open Simple_utils.Trace
open Ligo_prim
open Ast_typed
open Aggregation
open Main_errors
module Var = Simple_utils.Var
module SMap = Map.Make (String)

let compile_expression_in_context
    ~raise
    ~options
    ?(self_pass = true)
    ?(contract_pass = false)
    : Ast_typed.program -> Ast_typed.expression -> Ast_aggregated.expression
  =
 fun ctxt exp ->
  let ctxt, exp =
    trace ~raise aggregation_tracer @@ Aggregation.compile_program exp ctxt
  in
  let ctxt, exp =
    if self_pass
    then
      trace ~raise self_ast_aggregated_tracer
      @@ Self_ast_aggregated.all_program ~options (ctxt, exp)
    else ctxt, exp
  in
  let exp = Ast_aggregated.context_apply ctxt exp in
  let exp =
    if self_pass
    then
      trace ~raise self_ast_aggregated_tracer
      @@ Self_ast_aggregated.all_aggregated_expression exp
    else exp
  in
  let exp =
    if contract_pass
    then (
      let parameter_ty, storage_ty =
        trace_option
          ~raise
          (`Self_ast_aggregated_tracer
            (Self_ast_aggregated.Errors.corner_case
               "Could not recover types from contract"))
          (let open Simple_utils.Option in
          let open Ast_aggregated in
          let* { type1 = input_ty; _ } = Ast_aggregated.get_t_arrow exp.type_expression in
          Ast_aggregated.get_t_pair input_ty)
      in
      trace ~raise self_ast_aggregated_tracer
      @@ Self_ast_aggregated.all_contract parameter_ty storage_ty exp)
    else exp
  in
  if self_pass then Self_ast_aggregated.remove_check_self exp else exp


let compile_expression ~raise ~options : Ast_typed.expression -> Ast_aggregated.expression
  =
 fun e ->
  let x = trace ~raise aggregation_tracer @@ compile_expression e in
  trace ~raise self_ast_aggregated_tracer @@ Self_ast_aggregated.all_expression ~options x


let apply_to_entrypoint_contract ~raise ~options ?(contract_pass = false)
    : Ast_typed.program -> Value_var.t -> Ast_aggregated.expression
  =
 fun prg entrypoint ->
  let loc = Location.dummy in
  let prg, entrypoint, Self_ast_typed.Helpers.{ parameter = p_ty; storage = s_ty } =
    trace ~raise self_ast_typed_tracer
    @@ Self_ast_typed.Helpers.fetch_contract_type entrypoint prg
  in
  let ty =
    t_arrow
      ~loc
      (t_pair ~loc p_ty s_ty)
      (t_pair ~loc (t_list ~loc (t_operation ~loc ())) s_ty)
      ()
  in
  let var_ep = Ast_typed.(e_a_variable ~loc entrypoint ty) in
  compile_expression_in_context ~raise ~options ~contract_pass prg var_ep


let apply_to_entrypoint ~raise ~options
    : Ast_typed.program -> string -> Ast_aggregated.expression
  =
 fun prg entrypoint ->
  let loc = Location.dummy in
  let v = Value_var.of_input_var ~loc entrypoint in
  let ty, _ =
    trace ~raise self_ast_typed_tracer
    @@ Self_ast_typed.Helpers.fetch_entry_type entrypoint prg
  in
  let var_ep = Ast_typed.(e_a_variable ~loc v ty) in
  compile_expression_in_context ~raise ~options prg var_ep


let assert_equal_contract_type ~raise
    :  Simple_utils.Runned_result.check_type -> Value_var.t -> Ast_typed.program
    -> Ast_typed.expression -> unit
  =
 fun c entry contract param ->
  let entry_point =
    trace_option ~raise main_entrypoint_not_found (Ast_typed.get_entry contract entry)
  in
  trace ~raise (check_typed_arguments_tracer c) (fun ~raise ->
      match entry_point.type_expression.type_content with
      | T_arrow { type1 = args; type2 = _ } ->
        (match args.type_content with
        | T_record m when Record.cardinal m.fields = 2 ->
          let param_exp = Record.find m.fields (Label "0") in
          let storage_exp = Record.find m.fields (Label "1") in
          (match c with
          | Check_parameter ->
            trace ~raise checking_tracer
            @@ Checking.assert_type_expression_eq
                 entry_point.location
                 (param_exp, param.type_expression)
          | Check_storage ->
            trace ~raise checking_tracer
            @@ Checking.assert_type_expression_eq
                 entry_point.location
                 (storage_exp, param.type_expression))
        | _ -> raise.error @@ main_entrypoint_not_a_function)
      | _ -> raise.error @@ main_entrypoint_not_a_function)


let apply_to_entrypoint_view ~raise ~options
    : Ast_typed.program -> Ast_aggregated.expression
  =
 fun prg ->
  let loc = Location.dummy in
  let prg, views_info = Ast_typed.Helpers.fetch_views_in_program prg in
  let aux : int -> _ -> Label.t * expression =
   fun i (view_ty, view_binder) ->
    let a_ty, s_ty, r_ty =
      (* at this point the self-pass on views has been applied, we assume the types are correct *)
      trace_option ~raise main_unknown @@ Ast_typed.get_view_form view_ty
    in
    let ty = t_arrow ~loc (t_pair ~loc a_ty s_ty) r_ty () in
    Label.of_int i, Ast_typed.(e_a_variable ~loc (Binder.get_var view_binder) ty)
  in
  let tuple_view =
    Ast_typed.ez_e_a_record ~loc ~layout:Layout.comb (List.mapi ~f:aux views_info)
  in
  let e = compile_expression_in_context ~raise ~options prg tuple_view in
  Self_ast_aggregated.remove_check_self e


(* 
  if only_ep, we only list the declarations with types fiting an entrypoint
  TODO (when we have module signature): extract declaration names from sig type
*)
let list_declarations (only_ep : bool) (m : Ast_typed.program) : Value_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match el.wrap_content with
      | D_irrefutable_match { pattern = { wrap_content = P_var binder; _ }; attr; expr }
      | D_value { binder; attr; expr }
        when not attr.hidden ->
        if only_ep
        then
          if is_some
               (Ast_typed.Misc.get_type_of_contract expr.type_expression.type_content)
          then Binder.get_var binder :: prev
          else prev
        else Binder.get_var binder :: prev
      | D_value _ | D_irrefutable_match _ | D_type _ | D_module _ -> prev)
    ~init:[]
    m


let list_type_declarations (m : Ast_typed.program) : Type_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match el.wrap_content with
      | D_type { type_binder; type_attr; _ } when type_attr.public -> type_binder :: prev
      | _ -> prev)
    ~init:[]
    m


let list_mod_declarations (m : Ast_typed.program) : Module_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Simple_utils.Location in
      match el.wrap_content with
      | D_module { module_binder; module_attr; _ } when module_attr.public ->
        module_binder :: prev
      | _ -> prev)
    ~init:[]
    m
