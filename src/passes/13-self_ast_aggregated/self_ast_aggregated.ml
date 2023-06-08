open Ligo_prim
module Errors = Errors
module Helpers = Helpers

let map_expression = Ast_aggregated.Helpers.map_expression
let expression_obj ~raise e = Obj_ligo.check_obj_ligo ~raise e

let make_forced : Ast_aggregated.expression -> Ast_aggregated.expression =
  let f e =
    let open Ast_aggregated in
    match e.expression_content with
    | E_lambda { binder; result; output_type } ->
      let binder = Param.set_forced_flag binder in
      { e with expression_content = E_lambda { binder; result; output_type } }
    | _ -> e
  in
  map_expression f


let accessor_reduce : Ast_aggregated.expression -> Ast_aggregated.expression =
  let f (e : Ast_aggregated.expression) =
    match e.expression_content with
    | E_accessor { struct_ = { expression_content = E_record m; _ }; path } ->
      Record.find m path
    | _ -> e
  in
  map_expression f


let replace_location
    : Location.t -> Ast_aggregated.expression -> Ast_aggregated.expression
  =
 fun location ->
  let f (e : Ast_aggregated.expression) = { e with location } in
  map_expression f


let inline_thunk : bool ref -> Ast_aggregated.expression -> Ast_aggregated.expression =
 fun changed e ->
  let return_changed e =
    changed := true;
    e
  in
  match e.expression_content with
  | E_let_in
      { let_binder = { wrap_content = P_var let_binder; _ }
      ; rhs
      ; let_result
      ; attributes = { thunk = true; _ }
      } ->
    let rhs = make_forced rhs in
    let e =
      Subst.subst_expression ~body:let_result ~x:(Binder.get_var let_binder) ~expr:rhs
    in
    return_changed e
  | E_application { lamb = { expression_content = E_lambda { binder; result }; _ }; args }
    when Param.is_forced binder ->
    (* Some locations would point to stdlib (none) *)
    let result = replace_location e.location result in
    let args = replace_location e.location args in
    (* Do the beta reduction *)
    let e = Subst.subst_expression ~body:result ~x:(Param.get_var binder) ~expr:args in
    (* This is needed to solve accessors such as ("%foo", ...).0 after substitution *)
    let e = accessor_reduce e in
    return_changed e
  | _ -> e


let inline_thunks : bool ref -> Ast_aggregated.expression -> Ast_aggregated.expression =
 fun changed -> map_expression (inline_thunk changed)


let rec thunk e =
  let changed = ref false in
  let e = inline_thunks changed e in
  if !changed then thunk e else e


let remove_check_self : Ast_aggregated.expression -> Ast_aggregated.expression =
  let f (e : Ast_aggregated.expression) =
    let open Ast_aggregated in
    let loc = e.location in
    match e.expression_content, Ast_aggregated.get_t_option e.type_expression with
    | E_constant { cons_name = C_CHECK_SELF; arguments = [ _ ] }, Some t ->
      Ast_aggregated.e_a_none ~loc t
    | _ -> e
  in
  map_expression f


let all_aggregated_expression ~raise e =
  let e = Monomorphisation.mono_polymorphic_expr ~raise e in
  let e = Uncurry.uncurry_expression e in
  let e = thunk e in
  let e = map_expression (Literal_replace.expression ~raise) e in
  let e = map_expression (Contract_passes.entrypoint_typing ~raise) e in
  let e = map_expression (Contract_passes.emit_event_typing ~raise) e in
  let e = map_expression (Contract_passes.self_literal_typing ~raise) e in
  let e = map_expression (Contract_passes.litstr_check ~raise) e in
  e


let all_expression ~raise ~(options : Compiler_options.middle_end) e =
  let e = map_expression Polymorphic_replace.expression e in
  let e =
    if not options.test
    then (
      let () = Obj_ligo.check_obj_ligo ~raise e in
      (* for good measure .. *)
      e)
    else e
  in
  all_aggregated_expression ~raise e


let all_program
    ~raise
    ~(options : Compiler_options.middle_end)
    (prg : Ast_aggregated.program)
  =
  let prg = if not options.test then Remove_unused.remove_unused prg else prg in
  let prg = Ast_aggregated.Helpers.map_program Polymorphic_replace.expression prg in
  let prg =
    if not options.test
    then (
      let prg = Obj_ligo.purge_meta_ligo_program ~raise prg in
      let () = Obj_ligo.check_obj_ligo_program ~raise prg in
      (* for good measure .. *)
      prg)
    else prg
  in
  prg


let contract_passes ~raise = [ Contract_passes.self_typing ~raise ]

let contract_passes_map ~raise =
  [ Contract_passes.entrypoint_typing ~raise; Contract_passes.emit_event_typing ~raise ]


let all_contract ~raise parameter storage prg =
  let contract_type : Contract_passes.contract_type = { parameter; storage } in
  let all_p =
    List.map ~f:(fun pass ->
        Ast_aggregated.Helpers.fold_map_expression pass contract_type)
    @@ contract_passes ~raise
  in
  let prg = List.fold ~f:(fun x f -> snd @@ f x) all_p ~init:prg in
  let all_p =
    List.map ~f:(fun pass -> map_expression pass) @@ contract_passes_map ~raise
  in
  let prg = List.fold ~f:(fun x f -> f x) all_p ~init:prg in
  prg
