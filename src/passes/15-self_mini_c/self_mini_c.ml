module Errors = Errors
open Errors
open Mini_c
open Simple_utils.Trace
open Ligo_prim

let eta_expand : expression -> type_expression -> type_expression -> anon_function =
  fun e in_ty out_ty ->
    let loc = e.location in
    let binder = Value_var.fresh ~loc () in
    let var = e_var binder in_ty in
    let app = e_application e out_ty var in
    { binder = binder ; body = app }
let get_t_function ~raise e =
  trace_option ~raise not_a_function @@ Mini_c.get_t_function e
let get_t_pair ~raise e =
  trace_option ~raise not_a_pair @@ Mini_c.get_t_pair e

let get_function_or_eta_expand ~raise e =
  let in_ty, out_ty = match e.type_expression.type_content with
    | T_function t -> t
    | _ -> raise.error (corner_case "contract do not have the type of a function")
  in
  match e.content with
  | E_closure f -> f
  | _ ->
    eta_expand e in_ty out_ty

(* TODO hack to specialize map_expression to identity monad *)
let map_expression = Helpers.map_expression

(* Conservative purity test: ok to treat pure things as impure, must
   not treat impure things as pure. *)

let rec is_pure : expression -> bool = fun e ->
  match e.content with
  | E_literal _
  | E_closure _
  | E_rec _
  | E_variable _
  | E_raw_michelson _
    -> true

  | E_if_bool (cond, bt, bf)
  | E_if_none (cond, bt, (_, bf))
  | E_if_cons (cond, bt, (_, bf))
  | E_if_left (cond, (_, bt), (_, bf))
    -> List.for_all ~f:is_pure [ cond ; bt ; bf ]

  | E_let_in (e1, _, (_, e2))
    -> List.for_all ~f:is_pure [ e1 ; e2 ]

  | E_tuple exprs
    -> List.for_all ~f:is_pure exprs
  | E_let_tuple (e1, (_, e2))
    -> List.for_all ~f:is_pure [ e1 ; e2 ]
  | E_proj (e, _i, _n)
    -> is_pure e
  | E_update (expr, _i, update, _n)
    -> List.for_all ~f:is_pure [ expr ; update ]

  | E_constant (c)
    -> Constant.constant'_is_pure c.cons_name && List.for_all ~f:is_pure c.arguments

  | E_global_constant (_hash, _args) ->
    (* hashed code can be impure :( *)
    false
  | E_create_contract _ ->
    (* very not pure *)
    false

 (* TODO E_let_mut_in is pure when the rhs is pure and the body's
     only impurity is assign/deref of the bound mutable variable *)
  | E_let_mut_in _
  | E_assign _
  | E_deref _ ->
     false

  (* these could be pure through the exception above for
     E_let_mut_in *)
  | E_for _ | E_for_each _ -> false

  (* never pure in any important case *)
  | E_while _ -> false

  (* I'm not sure about these. Maybe can be tested better? *)
  | E_application _
  | E_iterator _
  | E_fold _
  | E_fold_right _
    -> false

let occurs_count : Value_var.t -> expression -> int =
  fun x e ->
  let fvs = get_fv [] e in
  Free_variables.mem_count x fvs

let mutation_count : Value_var.t -> expression -> int =
  fun x e ->
    let muts = assigned_and_free_vars [] e in
    Free_variables.mem_count x muts
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

let is_variable : expression -> bool =
  fun e ->
  match e.content with
  | E_variable _ -> true
  | _ -> false

let should_inline : Value_var.t -> expression -> expression -> bool =
  fun x e1 e2 ->
  occurs_count x e2 <= 1 || is_variable e1

let should_inline_mut : Value_var.t -> expression -> bool =
  fun x e2 ->
  mutation_count x e2 = 0

let inline_let : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_let_in (e1, should_inline_here, ((x, _a), e2)) ->
    if (is_pure e1 && (should_inline_here || should_inline x e1 e2))
    then
      let e2' = Subst.subst_expression ~body:e2 ~x:x ~expr:e1 in
      (changed := true ; e2')
    else
      e
  | E_let_mut_in (e1, ((x, _a), e2)) ->
    if (is_pure e1 && (should_inline_mut x e2))
    then
      let e2' = Subst.subst_expression ~body:e2 ~x:x ~expr:e1 in
      (changed := true ; e2')
    else
      e
  | _ -> e

let inline_lets : bool ref -> expression -> expression =
  fun changed ->
  map_expression (inline_let changed)


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
  | E_application ({ content = E_closure { binder = x ; body = e1 } ; type_expression = {type_content = T_function (xtv, tv);_ }; location = _}, e2) ->
    (changed := true ;
     Expression.make (E_let_in (e2, false, ((x, xtv), e1))) tv)

  (* also do CAR (PAIR x y) ↦ x, or CDR (PAIR x y) ↦ y, only if x and y are pure *)
  | E_constant {cons_name = C_CAR| C_CDR as const; arguments = [ { content = E_constant {cons_name = C_PAIR; arguments = [ e1 ; e2 ]} ; type_expression = _ ; location = _} ]} ->
    if is_pure e1 && is_pure e2
    then (changed := true ;
          match const with
          | C_CAR -> e1
          | C_CDR -> e2
          | _ -> assert false)
    else e

  (* (e0, e1, ...).(i) ↦ ei  (only if all ei are pure) *)
  | E_proj ({ content = E_tuple es; _ }, i, _n) ->
    if List.for_all ~f:is_pure es
    then (changed := true;
          List.nth_exn es i)
    else e

  (** (let x = e1 in e2).(i) ↦ (let x = e1 in e2.(i)) *)
  | E_proj ({ content = E_let_in (e1, inline, ((x, a), e2));type_expression = _; location=_ } as e_let_in, i, n) ->
    changed := true;
    { e_let_in with content = E_let_in (e1, inline, ((x, a), ({ e with content = E_proj (e2, i, n) }))) ;
                    type_expression = e.type_expression }

  (** (let (x, y, ...) = e1 in e2).(i) ↦ (let (x, y, ...) = e1 in e2.(i)) *)
  | E_proj ({ content = E_let_tuple (e1, (vars, e2));type_expression = _; location=_ } as e_let_tuple, i, n) ->
    changed := true;
    { e_let_tuple with content = E_let_tuple (e1, (vars, ({ e with content = E_proj (e2, i, n) }))) ;
                       type_expression = e.type_expression }

  (** (let x = (let y = e1 in e2) in e3) ↦ (let y = e1 in let x = e2 in e3) *)
  | E_let_in ({ content = E_let_in (e1, inline2, ((y, b), e2)); _ }, inline1, ((x, a), e3)) ->
    let y' = Value_var.fresh_like y in
    let e2 = Subst.replace e2 y y' in
    changed := true;
    {e with content = E_let_in (e1, inline2, ((y', b), {e with content = E_let_in (e2, inline1, ((x, a), e3))}))}

  (** note: E_let_tuple/E_let_in and E_let_in/E_let_tuple conversions
      not implemented yet because they don't seem important (?) *)

  (** (let x = e1 in e2)@e3 ↦ let x = e1 in e2@e3  (if e1 or e3 is pure) *)
  | E_application ({ content = E_let_in (e1, inline, ((x, a), e2)); _ }, e3) ->
    if is_pure e1 || is_pure e3
    then
      let x' = Value_var.fresh_like x in
      let e2 = Subst.replace e2 x x' in
      changed := true;
      {e with content = E_let_in (e1, inline, ((x', a), {e with content = E_application (e2, e3)}))}
    else e

  (** (let (x, y, ...) = e1 in e2)@e3 ↦ let (x, y, ...) = e1 in e2@e3  (if e1 or e3 is pure) *)
  | E_application ({ content = E_let_tuple (e1, (vars, e2)); _ }, e3) ->
    if is_pure e1 || is_pure e3
    then
      let vars = List.map ~f:(fun (x, a) -> (x, Value_var.fresh_like x, a)) vars in
      let e2 = List.fold_left vars ~init:e2 ~f:(fun e2 (x, x', _a) -> Subst.replace e2 x x') in
      let vars = List.map ~f:(fun (_x, x', a) -> (x', a)) vars in
      changed := true;
      {e with content = E_let_tuple (e1, (vars, {e with content = E_application (e2, e3)}))}
    else e

  (* let (x0, x1, ...) = (e0, e1, ...) in body ↦
     let ... in let x1 = e1 in let x0 = e0 in body
     (here, purity of the ei does not matter)
     *)
  | E_let_tuple ({ content = E_tuple es; _ }, (vars, body))
  | E_let_tuple ({ content = E_constant { cons_name = C_PAIR ; arguments = es }; _ }, (vars, body)) ->
    changed := true;
    List.fold_left
      ~f:(fun body (e, (v, t)) ->
         { content = E_let_in (e, false, ((v, t), body));
           location = Location.generated;
           type_expression = body.type_expression })
      ~init:body (List.zip_exn es vars)
  | _ -> e

let betas : bool ref -> expression -> expression =
  fun changed ->
  map_expression (beta changed)

let eta : bool ref -> expression -> expression =
  fun changed e ->
  match e.content with
  | E_constant {cons_name = C_PAIR; arguments = [ { content = E_constant {cons_name = C_CAR; arguments = [ e1 ]} ; type_expression = _ ; location = _} ;
                                                  { content = E_constant {cons_name = C_CDR; arguments = [ e2 ]} ; type_expression = _ ; location = _}]} ->
    (match (e1.content, e2.content) with
     | E_variable x1, E_variable x2 ->
       if Value_var.equal x1 x2
       then
         (changed := true;
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
             then
               match e'.content with
               | E_variable x -> Some x
               | _ -> None
             else None
           | _ -> None)
        es in
    (match Option.all projs with
     | None -> e
     | Some vars ->
       match vars with
       | var :: _ ->
         if List.for_all ~f:(Value_var.equal var) vars
         then { e with content = E_variable var }
         else e
       | _ -> e)
  | _ -> e

let etas : bool ref -> expression -> expression =
  fun changed ->
  map_expression (eta changed)

let not_comparable ~raise : expression -> expression =
  map_expression (Michelson_restrictions.not_comparable ~raise)

let contract_check ~raise ~(options : Compiler_options.t) (init: anon_function) : anon_function=
  if options.backend.experimental_disable_optimizations_for_debugging
  then init
  else
    let all = [Michelson_restrictions.self_in_lambdas ~raise; Michelson_restrictions.not_comparable ~raise] in
    let all_e = List.map ~f:(Helpers.map_sub_level_expression) all in
    List.fold ~f:(|>) all_e ~init

let rec all_expression ~raise (options : Compiler_options.t) : expression -> expression =
  fun e ->
  if options.backend.experimental_disable_optimizations_for_debugging
  then e
  else
    let changed = ref false in
    let e = inline_lets changed e in
    let e = betas changed e in
    let e = etas changed e in
    let e = not_comparable ~raise e in
    if !changed
    then all_expression ~raise options e
    else e

let create_contract ~raise expr =
  let _ = map_expression (fun expr ->
                  match expr.content with
                  | E_create_contract (_, _, ((x, t), lambda), _) -> (
                    let fvs = get_fv [(x, t)] lambda in
                    if Int.equal (List.length fvs) 0 then expr
                    else raise.error @@ fvs_in_create_contract_lambda expr (fst @@ List.hd_exn fvs)
                  )
                  | _ -> expr) expr in
  expr

let all_expression ~raise options e =
  let e = Uncurry.uncurry_expression e in
  let e = all_expression ~raise options e in
  let e = create_contract ~raise e in
  let e = Check_apply.capture_expression ~raise e in
  e
