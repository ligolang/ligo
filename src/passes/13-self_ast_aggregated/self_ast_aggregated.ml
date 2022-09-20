open Ligo_prim
module Errors = Errors
module Helpers = Helpers

let reset_counter () = Monomorphisation.poly_counter_reset ()
let expression_obj ~raise e = Obj_ligo.check_obj_ligo ~raise e

let eta_reduce : Ast_aggregated.expression -> Ast_aggregated.expression option =
  fun e ->
  match e.expression_content with
  |  E_lambda { binder ;
                result = { expression_content = E_application { lamb ; args } ; _ } } ->
     begin match Ast_aggregated.get_e_variable args with
     | Some y when Param.is_imm binder && Value_var.equal (Param.get_var binder) y -> Some lamb
     | _ -> None
     end
  | _ -> None

let get_abstraction_tuple e =
  let open Ast_aggregated in
  match e.expression_content with
  | E_lambda { binder ; result ; _ } ->
    let var = Param.get_var binder in
    begin match result.expression_content with
    | E_matching { matchee = { expression_content = E_variable u ; _ } ; cases = Match_record { fields ; body ; _ } }
      when Value_var.equal u var && Param.is_imm binder && Record.is_tuple fields ->
      let acc = Record.tuple_of_record fields in
      let acc = List.map ~f:snd acc in
      let acc = List.map ~f:(Binder.get_var) acc in
      Some (acc, body)
    | _ -> None end
  | _ -> None

let get_application_tuple e =
  let open Ast_aggregated in
  match e.expression_content with
  | E_application { lamb ; args = { expression_content = E_record p ; _ } } ->
      let acc = Record.tuple_of_record p in
      let acc = List.map ~f:snd acc in
      Some (acc, lamb)
  | _ -> None
  

(* This function destructs an expression of the form:
     (fun (x1, ..., xn) -> EXPR)(A1, ..., An) *)
let get_app_and_abs_tuple e =
  let open Option in
  let* arguments, lamb = get_application_tuple e in
  let* vars, expr = get_abstraction_tuple lamb in
  Some (arguments, vars, expr)

let rec get_abstractions acc e =
  let open Ast_aggregated in
  match acc, e.expression_content with
  | _, E_lambda { binder ; result ; _ } ->
    (* This treatment of params only works bcs of the specific transformation this analysis is used in -- i.e. it is a hack! *)
    get_abstractions (acc @ [Param.get_var binder]) result
  | [], _ -> None
  | _, _ -> Some (acc, e)

let rec get_applications acc e =
  let open Ast_aggregated in
  match acc, e.expression_content with
  | _, E_application { lamb ; args } ->
     get_applications (args :: acc) lamb
  | [], _ -> None
  | _, _ -> Some (acc, e)

(* This function destructs an expression of the form:
     (fun x1 -> ... -> fun xn -> EXPR) A1 ... An *)
let get_apps_and_abs e =
  let open Option in
  let* arguments, lamb = get_applications [] e in
  let* vars, expr = get_abstractions [] lamb in
  Some (arguments, vars, expr)

let get_abstract_and_app e =
  match get_app_and_abs_tuple e with
  | Some v -> Some v
  | None -> get_apps_and_abs e

(* Given a list [x1;...;xn] of vars, and an expression e, it checks
   whether e = CONST(x1, ..., xn) *)
let get_const acc e =
  let open Ast_aggregated in
  match e.expression_content with
  | E_constant { cons_name ; arguments = tuple ; _ } ->
     let rec f b e v =
       match e.expression_content with
       | E_variable u when Value_var.equal u v -> b
       | E_lambda _ when Option.is_some (eta_reduce e) ->
          let e = Option.value_exn (eta_reduce e) in
          f b e v
       | _ -> false
     in
     begin match List.fold2 tuple acc ~f ~init:true with
     | Unequal_lengths -> None
     | Ok false -> None
     | Ok true -> Some (cons_name, List.length tuple)
     end
  | _ -> None

let inline_let : bool ref -> Ast_aggregated.expression -> Ast_aggregated.expression =
  fun changed e ->
  match e.expression_content with
  | E_let_in { let_binder ; rhs ; let_result ; attr = { thunk = true ; _ } } ->
     let e2' = Subst.subst_expression ~body:let_result ~x:(Binder.get_var let_binder) ~expr:rhs in
     (changed := true ; e2')
  | E_application _ ->
    (* This case tries to reduce
         `(fun x1 -> ... -> fun xn -> CONST(x1, ..., xn)) A1 ... An` to `CONST(A1, ... AN)` or
         `(fun (x1, ..., xn) -> CONST(x1, ..., xn))(A1, ..., An)` to `CONST(A1, ... AN)`
       It is needed to handle special constants in the stdlib
     *)
     begin
       match get_abstract_and_app e with
       | Some (arguments, vars, const) ->
         begin match get_const vars const with
           | Some (cons_name, len) when len = List.length arguments ->
             (changed := true; { e with expression_content = E_constant { cons_name ; arguments } })
           | Some _ | None -> e
         end
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
