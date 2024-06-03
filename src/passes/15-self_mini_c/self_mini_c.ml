open Mini_c
open Ligo_prim
module Errors = Errors
module Trace = Simple_utils.Trace

let eta_expand : expression -> type_expression -> type_expression -> anon_function =
 fun e in_ty out_ty ->
  let loc = e.location in
  let binder = Value_var.fresh ~loc () in
  let var = e_var binder in_ty in
  let app = e_application e out_ty var in
  { binder; body = app }


let get_t_function ~raise e =
  Trace.trace_option ~raise Errors.not_a_function @@ Mini_c.get_t_function e


let get_t_pair ~raise e =
  Trace.trace_option ~raise Errors.not_a_pair @@ Mini_c.get_t_pair e


let get_function_or_eta_expand ~(raise : _ Trace.raise) e =
  let in_ty, out_ty =
    match e.type_expression.type_content with
    | T_function t -> t
    | _ -> raise.error (Errors.corner_case "contract do not have the type of a function")
  in
  match e.content with
  | E_closure f -> f
  | _ -> eta_expand e in_ty out_ty


(* TODO hack to specialize map_expression to identity monad *)
let map_expression = Helpers.map_expression
let is_pure : expression -> bool = Helpers.is_pure

(* Let "inlining" mean transforming the code:

     let x = e1 in e2

   to:

     e2[e1/x]

   (where the latter signifies substituting e1 for x in e2.)

   Things which can go wrong for inlining:

   - If `e1` is not pure, inlining may fail to preserve semantics.
   - Free variables of `e1` may be shadowed in e2, at usages of `x`. This
     is not a problem if the substitution is capture-avoiding.
   - ?
*)

let inline_lets : bool ref -> expression -> expression =
 fun changed expr ->
  let res, is_changed = Inline.inline_lets expr in
  changed := is_changed;
  res


let remove_unused_e_let_tuples : bool ref -> expression -> expression =
 fun changed ->
  map_expression (fun e ->
      match e.content with
      | E_let_tuple (_, (bs, e2)) ->
        let fvs = get_fv [] e2 in
        if List.for_all bs ~f:(fun (v, _) -> not @@ Free_variables.mem fvs v)
        then (
          changed := true;
          e2)
        else e
      | _ -> e)


(* Let "beta" mean transforming the code:

     (\x. e1) e2

   to:

     let x = e2 in e1

   Things which can go wrong for beta reduction:

   - Nothing?
*)

let beta : bool ref -> expression -> expression =
 fun changed e ->
  match e.content with
  | E_application
      ( { content = E_closure { binder = x; body = e1 }
        ; type_expression = { type_content = T_function (xtv, tv); _ }
        ; location = _
        }
      , e2 ) ->
    changed := true;
    Expression.make (E_let_in (e2, false, ((x, xtv), e1))) tv
  (* also do CAR (PAIR x y) ↦ x, or CDR (PAIR x y) ↦ y, only if x and y are pure *)
  | E_constant
      { cons_name = (C_CAR | C_CDR) as const
      ; arguments =
          [ { content = E_constant { cons_name = C_PAIR; arguments = [ e1; e2 ] }
            ; type_expression = _
            ; location = _
            }
          ]
      } ->
    if is_pure e1 && is_pure e2
    then (
      changed := true;
      match const with
      | C_CAR -> e1
      | C_CDR -> e2
      | _ -> assert false)
    else e
  (* (e0, e1, ...).(i) ↦ ei  (only if all ei are pure) *)
  | E_proj ({ content = E_tuple es; _ }, i, _n) ->
    if List.for_all ~f:is_pure es
    then (
      changed := true;
      List.nth_exn es i)
    else e
  (* (let x = e1 in e2).(i) ↦ (let x = e1 in e2.(i)) *)
  | E_proj
      ( ({ content = E_let_in (e1, inline, ((x, a), e2))
         ; type_expression = _
         ; location = _
         } as e_let_in)
      , i
      , n ) ->
    changed := true;
    { e_let_in with
      content = E_let_in (e1, inline, ((x, a), { e with content = E_proj (e2, i, n) }))
    ; type_expression = e.type_expression
    }
  (* (let (x, y, ...) = e1 in e2).(i) ↦ (let (x, y, ...) = e1 in e2.(i)) *)
  | E_proj
      ( ({ content = E_let_tuple (e1, (vars, e2)); type_expression = _; location = _ } as
        e_let_tuple)
      , i
      , n ) ->
    changed := true;
    { e_let_tuple with
      content = E_let_tuple (e1, (vars, { e with content = E_proj (e2, i, n) }))
    ; type_expression = e.type_expression
    }
  (* (let x = (let y = e1 in e2) in e3) ↦ (let y = e1 in let x = e2 in e3) *)
  | E_let_in ({ content = E_let_in (e1, inline2, ((y, b), e2)); _ }, inline1, ((x, a), e3))
    ->
    let y' = Value_var.fresh_like y in
    let e2 = Inline.replace e2 y y' in
    changed := true;
    { e with
      content =
        E_let_in
          ( e1
          , inline2
          , ((y', b), { e with content = E_let_in (e2, inline1, ((x, a), e3)) }) )
    }
  (* note: E_let_tuple/E_let_in and E_let_in/E_let_tuple conversions
     not implemented yet because they don't seem important (?) *)
  (* (let x = e1 in e2)@e3 ↦ let x = e1 in e2@e3  (if e1 or e3 is pure) *)
  | E_application ({ content = E_let_in (e1, inline, ((x, a), e2)); _ }, e3) ->
    if is_pure e1 || is_pure e3
    then (
      let x' = Value_var.fresh_like x in
      let e2 = Inline.replace e2 x x' in
      changed := true;
      { e with
        content =
          E_let_in (e1, inline, ((x', a), { e with content = E_application (e2, e3) }))
      })
    else e
  (* (let (x, y, ...) = e1 in e2)@e3 ↦ let (x, y, ...) = e1 in e2@e3  (if e1 or e3 is pure) *)
  | E_application ({ content = E_let_tuple (e1, (vars, e2)); _ }, e3) ->
    if is_pure e1 || is_pure e3
    then (
      let vars = List.map ~f:(fun (x, a) -> x, Value_var.fresh_like x, a) vars in
      let e2 =
        List.fold_left vars ~init:e2 ~f:(fun e2 (x, x', _a) -> Inline.replace e2 x x')
      in
      let vars = List.map ~f:(fun (_x, x', a) -> x', a) vars in
      changed := true;
      { e with
        content = E_let_tuple (e1, (vars, { e with content = E_application (e2, e3) }))
      })
    else e
  (* let (x0, x1, ...) = (e0, e1, ...) in body ↦
     let ... in let x1 = e1 in let x0 = e0 in body
     (here, purity of the ei does not matter)
     *)
  | E_let_tuple ({ content = E_tuple es; _ }, (vars, body))
  | E_let_tuple
      ({ content = E_constant { cons_name = C_PAIR; arguments = es }; _ }, (vars, body))
    ->
    changed := true;
    List.fold_left
      ~f:(fun body (e, (v, t)) ->
        { content = E_let_in (e, false, ((v, t), body))
        ; location = Location.generated
        ; type_expression = body.type_expression
        })
      ~init:body
      (List.zip_exn es vars)
  | _ -> e


let betas : bool ref -> expression -> expression =
 fun changed -> map_expression (beta changed)


let eta : bool ref -> expression -> expression =
 fun changed e ->
  match e.content with
  | E_constant
      { cons_name = C_PAIR
      ; arguments =
          [ { content = E_constant { cons_name = C_CAR; arguments = [ e1 ] }
            ; type_expression = _
            ; location = _
            }
          ; { content = E_constant { cons_name = C_CDR; arguments = [ e2 ] }
            ; type_expression = _
            ; location = _
            }
          ]
      } ->
    (match e1.content, e2.content with
    | E_variable x1, E_variable x2 ->
      if Value_var.equal x1 x2
      then (
        changed := true;
        { e with content = e1.content })
      else e
    | _ -> e)
  (* (x.(0), x.(1), ...) ↦ x *)
  | E_tuple es ->
    let count = List.length es in
    let projs =
      List.mapi
        ~f:(fun i e ->
          match e.content with
          | E_proj (e', j, n) ->
            if i = j && n = count
            then (
              match e'.content with
              | E_variable x -> Some x
              | _ -> None)
            else None
          | _ -> None)
        es
    in
    (match Option.all projs with
    | None -> e
    | Some vars ->
      (match vars with
      | var :: _ ->
        if List.for_all ~f:(Value_var.equal var) vars
        then { e with content = E_variable var }
        else e
      | _ -> e))
  | _ -> e


let etas : bool ref -> expression -> expression =
 fun changed -> map_expression (eta changed)


let not_comparable ~raise : expression -> expression =
  map_expression (Michelson_restrictions.not_comparable ~raise)


let contract_check ~raise ~(options : Compiler_options.t) (init : anon_function)
    : anon_function
  =
  if options.backend.experimental_disable_optimizations_for_debugging
  then init
  else (
    let all =
      [ Michelson_restrictions.self_in_lambdas ~raise
      ; Michelson_restrictions.not_comparable ~raise
      ]
    in
    let all_e = List.map ~f:Helpers.map_sub_level_expression all in
    List.fold ~f:( |> ) all_e ~init)


let rec all_expression ~raise (options : Compiler_options.t) : expression -> expression =
 fun e ->
  if options.backend.experimental_disable_optimizations_for_debugging
  then e
  else (
    let changed = ref false in
    let e = inline_lets changed e in
    let e = betas changed e in
    let e = etas changed e in
    let e = not_comparable ~raise e in
    let e = remove_unused_e_let_tuples changed e in
    if !changed then all_expression ~raise options e else e)


let create_contract ~(raise : _ Trace.raise) expr =
  let _ =
    map_expression
      (fun expr ->
        match expr.content with
        | E_create_contract (_, _, ((x, t), lambda), _) ->
          let fvs = get_fv [ x, t ] lambda in
          if Int.equal (List.length fvs) 0
          then expr
          else
            raise.error
            @@ Errors.fvs_in_create_contract_lambda expr (fst @@ List.hd_exn fvs)
        | _ -> expr)
      expr
  in
  expr


let all_expression ~raise options e =
  let e = Uncurry.uncurry_expression e in
  let e = all_expression ~raise options e in
  let e = create_contract ~raise e in
  let e =
    if options.backend.disable_michelson_typechecking
    then e
    else Check_apply.capture_expression ~raise e
  in
  e
