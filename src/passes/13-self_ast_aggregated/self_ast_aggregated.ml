module Errors = Errors
module Helpers = Helpers

let reset_counter () = Monomorphisation.poly_counter_reset ()
let expression_obj ~raise e = Obj_ligo.check_obj_ligo ~raise e

let rec get_abstractions acc e =
  let open Ast_aggregated in
  match e.expression_content with
  | E_lambda { binder = { var ; _ } ; result ; _ } ->
     get_abstractions (acc @ [var]) result
  | E_constant { cons_name ; arguments = tuple ; _ } ->
     let f b e v =
       match e.expression_content with
       | E_variable u when Ligo_prim.Value_var.equal u v -> b
       | _ -> false
     in
     begin match List.fold2 tuple acc ~f ~init:true with
     | List.Or_unequal_lengths.Unequal_lengths -> None
     | Ok false -> None
     | Ok true -> Some cons_name
     end
  | _ -> None

let rec get_applications acc e =
  let open Ast_aggregated in
  match e.expression_content with
  | E_application { lamb ; args } ->
     get_applications (args :: acc) lamb
  | _ -> acc, e

let inline_let : bool ref -> Ast_aggregated.expression -> Ast_aggregated.expression =
  fun changed e ->
  match e.expression_content with
  | E_let_in { let_binder = { var ; _ } ; rhs ; let_result ; attr = { thunk = true ; _ } } ->
     let e2' = Subst.subst_expression ~body:let_result ~x:var ~expr:rhs in
     (changed := true ; e2')
  | E_application _ ->
     begin
       let arguments, expr = get_applications [] e in
       match get_abstractions [] expr with
       | Some cons_name -> { e with expression_content = E_constant { cons_name ; arguments } }
       | None -> e
     end
  | _ -> e

let inline_lets : bool ref -> Ast_aggregated.expression -> Ast_aggregated.expression =
  fun changed ->
  Helpers.map_expression (inline_let changed)

let rec thunk e =
  let changed = ref false in
  let e = inline_lets changed e in
  if !changed
  then thunk e
  else e

let all_expression ~raise ~(options : Compiler_options.middle_end) e =
  let e = Helpers.map_expression Polymorphic_replace.expression e in
  let e = if not options.test then Obj_ligo.check_obj_ligo ~raise e else e in
  let e = Purify_assignations.expression ~raise e in
  let e = Monomorphisation.mono_polymorphic_expr ~raise e in
  let e = Uncurry.uncurry_expression e in
  let e = thunk e in
  let e = Helpers.map_expression (Literal_replace.expression ~raise) e in
  e

let contract_passes ~raise = [
  Contract_passes.self_typing ~raise ;
  Contract_passes.entrypoint_typing ~raise ;
  Contract_passes.emit_event_typing ~raise ;
]

let all_contract ~raise parameter storage prg =
  let contract_type : Contract_passes.contract_type = { parameter ; storage } in
  let all_p = List.map ~f:(fun pass -> Helpers.fold_map_expression pass contract_type) @@ contract_passes ~raise in
  let prg = List.fold ~f:(fun x f -> snd @@ f x) all_p ~init:prg in
  prg
