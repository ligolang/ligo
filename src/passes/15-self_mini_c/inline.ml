open Ligo_prim
open Mini_c
open Helpers
module Ligo_pair = Simple_utils.Ligo_pair

(* Reference implementation:
   https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/interp/lambda-subst/main.ml

   ...but, it has at least one bug: in subst,
   `let new_body = replace e' y fresh in ...` should be:
   `let new_body = replace e' fresh y in ...`,
   due to the arg order choice for replace.

   Below, this bug is fixed by adopting the other order choice for
   replace (as well as subst).  *)

let replace_var : var_name -> var_name -> var_name -> var_name =
 fun v x y -> if Value_var.equal v x then y else v


(* replace in `e` the variable `x` with `y`.

   It would be fine -- better? -- to only replace the _free_ x.
*)
let rec replace : expression -> var_name -> var_name -> expression =
 fun e x y ->
  let replace e = replace e x y in
  let return content = { e with content } in
  let replace_var v = replace_var v x y in
  match e.content with
  | E_literal _ -> e
  | E_closure { binder; body } ->
    let body = replace body in
    let binder = replace_var binder in
    return @@ E_closure { binder; body }
  | E_rec { func = { binder; body }; rec_binder } ->
    let body = replace body in
    let binder = replace_var binder in
    let rec_binder = replace_var rec_binder in
    return @@ E_rec { func = { binder; body }; rec_binder }
  | E_constant c ->
    let args = List.map ~f:replace c.arguments in
    return @@ E_constant { cons_name = c.cons_name; arguments = args }
  | E_application (f, x) ->
    let f, x = Ligo_pair.map ~f:replace (f, x) in
    return @@ E_application (f, x)
  | E_variable z ->
    let z = replace_var z in
    return @@ E_variable z
  | E_iterator (name, ((v, tv), body), expr) ->
    let body = replace body in
    let expr = replace expr in
    let v = replace_var v in
    return @@ E_iterator (name, ((v, tv), body), expr)
  | E_fold (((v, tv), body), collection, initial) ->
    let body = replace body in
    let collection = replace collection in
    let initial = replace initial in
    let v = replace_var v in
    return @@ E_fold (((v, tv), body), collection, initial)
  | E_fold_right (((v, tv), body), (collection, elem_tv), initial) ->
    let body = replace body in
    let collection = replace collection in
    let initial = replace initial in
    let v = replace_var v in
    return @@ E_fold_right (((v, tv), body), (collection, elem_tv), initial)
  | E_if_bool (c, bt, bf) ->
    let c = replace c in
    let bt = replace bt in
    let bf = replace bf in
    return @@ E_if_bool (c, bt, bf)
  | E_if_none (c, bt, ((v, tv), bf)) ->
    let c = replace c in
    let bt = replace bt in
    let bf = replace bf in
    let v = replace_var v in
    return @@ E_if_none (c, bt, ((v, tv), bf))
  | E_if_cons (c, bf, (((v1, tv1), (v2, tv2)), bt)) ->
    let c = replace c in
    let bf = replace bf in
    let v1 = replace_var v1 in
    let v2 = replace_var v2 in
    let bt = replace bt in
    return @@ E_if_cons (c, bf, (((v1, tv1), (v2, tv2)), bt))
  | E_if_left (c, ((v1, tv1), bt), ((v2, tv2), bf)) ->
    let c = replace c in
    let bf = replace bf in
    let v1 = replace_var v1 in
    let v2 = replace_var v2 in
    let bt = replace bt in
    return @@ E_if_left (c, ((v1, tv1), bt), ((v2, tv2), bf))
  | E_let_in (e1, inline, ((v, tv), e2)) ->
    let v = replace_var v in
    let e1 = replace e1 in
    let e2 = replace e2 in
    return @@ E_let_in (e1, inline, ((v, tv), e2))
  | E_tuple exprs ->
    let exprs = List.map ~f:replace exprs in
    return @@ E_tuple exprs
  | E_let_tuple (expr, (vtvs, body)) ->
    let expr = replace expr in
    let vtvs = List.map ~f:(fun (v, tv) -> replace_var v, tv) vtvs in
    let body = replace body in
    return @@ E_let_tuple (expr, (vtvs, body))
  | E_proj (expr, i, n) ->
    let expr = replace expr in
    return @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
    let expr = replace expr in
    let update = replace update in
    return @@ E_update (expr, i, update, n)
  | E_raw_michelson code -> return @@ E_raw_michelson code
  | E_inline_michelson (code, args') ->
    let args' = List.map ~f:replace args' in
    return @@ E_inline_michelson (code, args')
  | E_global_constant (hash, args) ->
    let args = List.map ~f:replace args in
    return @@ E_global_constant (hash, args)
  | E_create_contract (p, s, ((x, t), code), args) ->
    let args = List.map ~f:replace args in
    let x = replace_var x in
    let code = replace code in
    return @@ E_create_contract (p, s, ((x, t), code), args)
  (* Mutable stuff. We treat immutable and mutable variables
     identically because they share the context (e.g. a mutable
     variables might need to be renamed for capture-avoidance during
     substitution of an immutable variable.) *)
  | E_let_mut_in (e1, ((v, tv), e2)) ->
    let v = replace_var v in
    let e1 = replace e1 in
    let e2 = replace e2 in
    return @@ E_let_mut_in (e1, ((v, tv), e2))
  | E_deref v ->
    let v = replace_var v in
    return @@ E_deref v
  | E_assign (v, e) ->
    let v = replace_var v in
    let e = replace e in
    return @@ E_assign (v, e)
  | E_for (start, final, incr, ((x, a), body)) ->
    let start = replace start in
    let final = replace final in
    let incr = replace incr in
    let x = replace_var x in
    let body = replace body in
    return @@ E_for (start, final, incr, ((x, a), body))
  | E_for_each (coll, coll_type, (xs, body)) ->
    let coll = replace coll in
    let xs = List.map ~f:(fun (x, a) -> replace_var x, a) xs in
    let body = replace body in
    return @@ E_for_each (coll, coll_type, (xs, body))
  | E_while (cond, body) ->
    let cond = replace cond in
    let body = replace body in
    return @@ E_while (cond, body)


(** Set with free variables. *)
module Fvs = Set.Make (Value_var)

(** Map with [var_name] key. *)
module Var_map = Map.Make (Value_var)

(** Variable substitution. *)
type var_subst = var_name Var_map.t

(** Expression substitution. *)
type expr_subst = expression Var_map.t

(** State for monad [t]. *)
type state =
  { var_subst : var_subst (** Variable substitutions. *)
  ; expr_subst : expr_subst (** Expression substitutions. *)
  ; fvs : Fvs.t (** Free variables. *)
  ; changed : bool (** Flag which indicates that inlining occurred. *)
  }

(** A state monad. *)
type 'a t = state -> 'a * state

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return a state = a, state

  let bind m ~f state =
    let a, state = m state in
    f a state


  let map = `Define_using_bind
end)

(** Pick the current state. *)
let get_state : state t = fun state -> state, state

(** Update the state. *)
let set_state (state : state) : unit t = fun _ -> (), state

open Let_syntax

(** Add a substitution to the state. *)
let add_subst : var_name -> expression -> unit t =
 fun v expr ->
  let%bind state = get_state in
  let expr_subst = Map.update state.expr_subst v ~f:(Fn.const expr) in
  let fvs =
    List.fold_left (get_fv [] expr) ~init:state.fvs ~f:(fun fvs (fv, _) -> Set.add fvs fv)
  in
  set_state { state with expr_subst; fvs; changed = true }


(** Process the binder and update the state respectively. *)
let process_binder : var_name -> var_name t =
 fun v ->
  let%bind state = get_state in
  let expr_subst = (* Handle shadowing... *) Map.remove state.expr_subst v in
  let v, var_subst =
    if Set.mem state.fvs v
    then (
      (* We don't want to make a free variable to be bound.
         Let's rename the binder and add the variable substitution to the state. *)
      let fresh = Value_var.fresh_like v in
      let var_subst = Map.update state.var_subst v ~f:(Fn.const fresh) in
      fresh, var_subst)
    else v, state.var_subst
  in
  let%map () = set_state { state with expr_subst; var_subst } in
  v


(** Find expression substitution by its variable name. *)
let find_subst : var_name -> expression option t =
 fun v ->
  let%map state = get_state in
  Map.find state.expr_subst v


(** Lookup the correct variable name
    taking into account all binder replacements. *)
let find_replacement : var_name -> var_name t =
 fun v ->
  let%map state = get_state in
  Option.value ~default:v (Map.find state.var_subst v)


(** An auxiliary function which uses [t] state monad.
    If [only_vars] is set to [true] then [let x = e1 in e2] expression
    would be inlined if [e1] is a variable. Let-ins with [@inline] pragma
    would be also inlined. *)
let rec inline_lets : only_vars:bool -> expression -> expression t =
 fun ~only_vars expr ->
  let occurs_count : Value_var.t -> expression -> int =
   fun x e ->
    let fvs = get_fv [] e in
    Free_variables.mem_count x fvs
  in
  let mutation_count : Value_var.t -> expression -> int =
   fun x e ->
    let muts = assigned_and_free_vars [] e in
    Free_variables.mem_count x muts
  in
  let is_variable : expression -> bool =
   fun e ->
    match e.content with
    | E_variable _ -> true
    | _ -> false
  in
  let should_inline : only_vars:bool -> Value_var.t -> expression -> expression -> bool =
   fun ~only_vars x e1 e2 -> ((not only_vars) && occurs_count x e2 <= 1) || is_variable e1
  in
  let should_inline_mut : Value_var.t -> expression -> bool =
   fun x e2 -> mutation_count x e2 = 0
  in
  let self = inline_lets ~only_vars in
  let self_binder1 v ~body =
    let%bind state = get_state in
    let%bind v = process_binder v in
    let%bind body = self body in
    let%map () = set_state state in
    v, body
  in
  let self_binder2 (v1, v2) ~body =
    let%bind state = get_state in
    let%bind v1 = process_binder v1 in
    let%bind v2 = process_binder v2 in
    let%bind body = self body in
    let%map () = set_state state in
    (v1, v2), body
  in
  let self_binds vs ~body =
    let%bind state = get_state in
    let%bind vs = all @@ List.map ~f:process_binder vs in
    let%bind body = self body in
    let%map () = set_state state in
    vs, body
  in
  let return_expr content = { expr with content } in
  let process_variable : [> `Variable | `Deref ] -> var_name -> expression t =
   fun expr_case v ->
    match%bind find_subst v with
    | Some subst -> return subst
    | None ->
      let%map v = find_replacement v in
      return_expr
        (match expr_case with
        | `Variable -> E_variable v
        | `Deref -> E_deref v)
  in
  match expr.content with
  (* Here actual substitutions are created. *)
  | E_let_in (expr, inline, ((v, tv), body)) ->
    let%bind expr = self expr in
    if is_pure expr && (inline || should_inline ~only_vars v expr body)
    then (
      let%bind () = add_subst v expr in
      self body)
    else (
      let%map v, body = self_binder1 v ~body in
      return_expr @@ E_let_in (expr, inline, ((v, tv), body)))
  | E_let_mut_in (expr, ((v, tv), body)) ->
    let%bind expr = self expr in
    if is_pure expr && should_inline_mut v body
    then (
      let%bind () = add_subst v expr in
      self body)
    else (
      let%map v, body = self_binder1 v ~body in
      return_expr @@ E_let_mut_in (expr, ((v, tv), body)))
  (* Variables and dereferencing. *)
  | E_variable x' -> process_variable `Variable x'
  | E_deref x' -> process_variable `Deref x'
  (* Other stuff. *)
  | E_closure { binder; body } ->
    let%map binder, body = self_binder1 binder ~body in
    return_expr @@ E_closure { binder; body }
  | E_rec { func = { binder; body }; rec_binder } ->
    let%map (binder, rec_binder), body = self_binder2 (binder, rec_binder) ~body in
    return_expr @@ E_rec { func = { binder; body }; rec_binder }
  | E_tuple exprs ->
    let%map exprs = all @@ List.map ~f:self exprs in
    return_expr @@ E_tuple exprs
  | E_let_tuple (expr, (vtvs, body)) ->
    let%bind expr = self expr in
    let vs, tvs = List.unzip vtvs in
    let%map vs, body = self_binds vs ~body in
    let vtvs = List.zip_exn vs tvs in
    return_expr @@ E_let_tuple (expr, (vtvs, body))
  | E_proj (expr, i, n) ->
    let%map expr = self expr in
    return_expr @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
    let%bind expr = self expr in
    let%map update = self update in
    return_expr @@ E_update (expr, i, update, n)
  | E_iterator (s, ((name, tv), body), collection) ->
    let%bind collection = self collection in
    let%map name, body = self_binder1 name ~body in
    return_expr @@ E_iterator (s, ((name, tv), body), collection)
  | E_fold (((name, tv), body), collection, init) ->
    let%bind collection = self collection in
    let%bind init = self init in
    let%map name, body = self_binder1 name ~body in
    return_expr @@ E_fold (((name, tv), body), collection, init)
  | E_fold_right (((name, tv), body), (collection, elem_tv), init) ->
    let%bind collection = self collection in
    let%bind init = self init in
    let%map name, body = self_binder1 name ~body in
    return_expr @@ E_fold_right (((name, tv), body), (collection, elem_tv), init)
  | E_if_none (c, n, ((name, tv), s)) ->
    let%bind c = self c in
    let%bind n = self n in
    let%map name, s = self_binder1 name ~body:s in
    return_expr @@ E_if_none (c, n, ((name, tv), s))
  | E_if_cons (c, n, (((hd, hdtv), (tl, tltv)), cons)) ->
    let%bind c = self c in
    let%bind n = self n in
    let%map (hd, tl), cons = self_binder2 (hd, tl) ~body:cons in
    return_expr @@ E_if_cons (c, n, (((hd, hdtv), (tl, tltv)), cons))
  | E_if_left (c, ((name_l, tvl), l), ((name_r, tvr), r)) ->
    let%bind c = self c in
    let%bind name_l, l = self_binder1 name_l ~body:l in
    let%map name_r, r = self_binder1 name_r ~body:r in
    return_expr @@ E_if_left (c, ((name_l, tvl), l), ((name_r, tvr), r))
  | E_raw_michelson code -> return @@ return_expr @@ E_raw_michelson code
  | E_inline_michelson (code, args') ->
    let%map args' = all @@ List.map ~f:self args' in
    return_expr @@ E_inline_michelson (code, args')
  | E_literal _ -> return expr
  | E_constant { cons_name; arguments } ->
    let%map arguments = all @@ List.map ~f:self arguments in
    return_expr @@ E_constant { cons_name; arguments }
  | E_application farg ->
    let farg1, farg2 = Ligo_pair.map ~f:self farg in
    let%bind farg1 = farg1 in
    let%map farg2 = farg2 in
    return_expr @@ E_application (farg1, farg2)
  | E_if_bool (cab1, cab2, cab3) ->
    let cab1, cab2, cab3 = self cab1, self cab2, self cab3 in
    let%bind cab1 = cab1 in
    let%bind cab2 = cab2 in
    let%map cab3 = cab3 in
    return_expr @@ E_if_bool (cab1, cab2, cab3)
  | E_global_constant (hash, args) ->
    let%map args = all @@ List.map ~f:self args in
    return_expr @@ E_global_constant (hash, args)
  | E_create_contract (p, s, ((x, t), code), args) ->
    let%bind args = all @@ List.map ~f:self args in
    let%map x, code = self_binder1 x ~body:code in
    return_expr @@ E_create_contract (p, s, ((x, t), code), args)
  (* Mutable stuff. We allow substituting mutable variables which
     aren't mutated because it was easy. Hmm. *)
  | E_assign (x', e) ->
    let%bind state = get_state in
    if Map.mem state.expr_subst x'
    then
      failwith
        ("internal error, please report this as a bug: tried to substitute for mutated \
          var "
        ^ __LOC__)
    else (
      let%map e = self e in
      return_expr @@ E_assign (x', e))
  | E_for (start, final, incr, ((x, a), body)) ->
    let%bind start = self start in
    let%bind final = self final in
    let%bind incr = self incr in
    let%map x, body = self_binder1 x ~body in
    return_expr @@ E_for (start, final, incr, ((x, a), body))
  | E_for_each (coll, coll_type, (xs, body)) ->
    let%bind coll = self coll in
    let xs, ts = List.unzip xs in
    let%map xs, body = self_binds xs ~body in
    let xs = List.zip_exn xs ts in
    return_expr @@ E_for_each (coll, coll_type, (xs, body))
  | E_while (cond, body) ->
    let%bind cond = self cond in
    let%map body = self body in
    return_expr @@ E_while (cond, body)


(** Inlines all let-ins and let-mut-ins where it is possible.

    If [test] is true then it inlines only variables
    and let-ins with [@inline] pragma.
    Otherwise, it inlines the whole expression. *)
let inline_lets ?(test = false) : expression -> expression * bool =
 fun expr ->
  let state =
    { var_subst = Var_map.empty
    ; expr_subst = Var_map.empty
    ; fvs = Fvs.empty
    ; changed = false
    }
  in
  (* We don't want to produce less optimal code after inlinings.
     Let's inline variables first. *)
  let expr, only_vars_state = inline_lets ~only_vars:true expr state in
  let expr, not_only_vars_state =
    if test then expr, state else inline_lets ~only_vars:false expr state
  in
  expr, only_vars_state.changed || not_only_vars_state.changed


let%expect_test _ =
  let loc = Location.dummy in
  let dummy_type = Expression.make_t @@ T_base TB_unit in
  let wrap e = Expression.make e dummy_type in
  let show_subst ~body ~(x : var_name) ~expr =
    let expr_subst = wrap @@ E_let_in (expr, true, ((x, dummy_type), body)) in
    Format.printf
      "(%a)[%a := %a] =@ %a"
      PP.expression
      body
      Value_var.pp
      x
      PP.expression
      expr
      PP.expression
      (fst @@ inline_lets ~test:true expr_subst)
  in
  let x = Value_var.of_input_var ~loc "x" in
  let y = Value_var.of_input_var ~loc "y" in
  let z = Value_var.of_input_var ~loc "z" in
  let var x = wrap (E_variable x) in
  let app f x = wrap (E_application (f, x)) in
  let lam x u = wrap (E_closure { binder = x; body = u }) in
  let unit = wrap (E_literal Literal_unit) in
  (* substituted var *)
  Type_var.reset_counter ();
  show_subst ~body:(var x) ~x ~expr:unit;
  [%expect {|
    (x)[x := L(unit)] =
    L(unit) |}];
  (* other var *)
  Type_var.reset_counter ();
  show_subst ~body:(var y) ~x ~expr:unit;
  [%expect {|
    (y)[x := L(unit)] =
    y
  |}];
  (* closure shadowed *)
  Type_var.reset_counter ();
  show_subst ~body:(lam x (var x)) ~x ~expr:unit;
  [%expect {|
    (fun x -> (x))[x := L(unit)] =
    fun x -> (x)
  |}];
  (* closure not shadowed *)
  Type_var.reset_counter ();
  show_subst ~body:(lam y (var x)) ~x ~expr:unit;
  [%expect {|
    (fun y -> (x))[x := L(unit)] =
    fun y -> (L(unit))
  |}];
  (* closure capture-avoidance *)
  Type_var.reset_counter ();
  show_subst ~body:(lam y (app (var x) (var y))) ~x ~expr:(wrap (E_variable y));
  [%expect {|
    (fun y -> ((x)@(y)))[x := y] =
    fun y#2 -> ((y)@(y#2))
  |}];
  (* let-in shadowed (not in rhs) *)
  Type_var.reset_counter ();
  show_subst
    ~body:(wrap (E_let_in (var x, false, ((x, dummy_type), var x))))
    ~x
    ~expr:unit;
  [%expect {|
    (let x = x in x)[x := L(unit)] =
    let x = L(unit) in x
  |}];
  (* let-in not shadowed *)
  Type_var.reset_counter ();
  show_subst
    ~body:(wrap (E_let_in (var x, false, ((y, dummy_type), var x))))
    ~x
    ~expr:unit;
  [%expect {|
    (let y = x in x)[x := L(unit)] =
    let y = L(unit) in L(unit)
  |}];
  (* let-in capture avoidance *)
  Type_var.reset_counter ();
  show_subst
    ~body:
      (wrap
         (E_let_in (app (var x) (var x), false, ((y, dummy_type), app (var x) (var y)))))
    ~x
    ~expr:(var y);
  [%expect
    {|
    (let y = (x)@(x) in (x)@(y))[x := y] =
    let y#3 = (y)@(y) in (y)@(y#3)
  |}];
  (* iter shadowed *)
  Type_var.reset_counter ();
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((x, dummy_type), var x), var x)))
    ~x
    ~expr:unit;
  [%expect
    {|
    (for_ITER x of x do ( x ))[x := L(unit)] =
    for_ITER x of L(unit) do ( x )
  |}];
  (* iter not shadowed *)
  Type_var.reset_counter ();
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((y, dummy_type), var x), var x)))
    ~x
    ~expr:unit;
  [%expect
    {|
    (for_ITER y of x do ( x ))[x := L(unit)] =
    for_ITER y of L(unit) do ( L(unit) )
  |}];
  (* iter capture-avoiding *)
  Type_var.reset_counter ();
  show_subst
    ~body:
      (wrap
         (E_iterator (C_ITER, ((y, dummy_type), app (var x) (var y)), app (var x) (var y))))
    ~x
    ~expr:(var y);
  [%expect
    {|
    (for_ITER y of (x)@(y) do ( (x)@(y) ))[x := y] =
    for_ITER y#4 of (y)@(y) do ( (y)@(y#4) )
  |}];
  (* if_cons shadowed 1 *)
  Type_var.reset_counter ();
  show_subst
    ~body:(wrap (E_if_cons (var x, var x, (((x, dummy_type), (y, dummy_type)), var x))))
    ~x
    ~expr:unit;
  [%expect
    {|
    (x ?? x : (x :: y) -> x)[x := L(unit)] =
    L(unit) ?? L(unit) : (x :: y) -> x
  |}];
  (* if_cons shadowed 2 *)
  Type_var.reset_counter ();
  show_subst
    ~body:(wrap (E_if_cons (var x, var x, (((y, dummy_type), (x, dummy_type)), var x))))
    ~x
    ~expr:unit;
  [%expect
    {|
    (x ?? x : (y :: x) -> x)[x := L(unit)] =
    L(unit) ?? L(unit) : (y :: x) -> x
  |}];
  (* if_cons not shadowed *)
  Type_var.reset_counter ();
  show_subst
    ~body:(wrap (E_if_cons (var x, var x, (((y, dummy_type), (z, dummy_type)), var x))))
    ~x
    ~expr:unit;
  [%expect
    {|
    (x ?? x : (y :: z) -> x)[x := L(unit)] =
    L(unit) ?? L(unit) : (y :: z) -> L(unit)
  |}];
  (* if_cons capture avoidance 1 *)
  Type_var.reset_counter ();
  show_subst
    ~body:
      (wrap
         (E_if_cons
            ( var x
            , var x
            , (((y, dummy_type), (z, dummy_type)), app (var x) (app (var y) (var z))) )))
    ~x
    ~expr:(var y);
  [%expect
    {|
    (x ?? x : (y :: z) -> (x)@((y)@(z)))[x := y] =
    y ?? y : (y#5 :: z) -> (y)@((y#5)@(z))
  |}];
  (* if_cons capture avoidance 2 *)
  Type_var.reset_counter ();
  show_subst
    ~body:
      (wrap
         (E_if_cons
            ( var x
            , var x
            , (((y, dummy_type), (z, dummy_type)), app (var x) (app (var y) (var z))) )))
    ~x
    ~expr:(var z);
  [%expect
    {|
    (x ?? x : (y :: z) -> (x)@((y)@(z)))[x := z] =
    z ?? z : (y :: z#6) -> (z)@((y)@(z#6))
  |}];
  (* old bug *)
  Value_var.reset_counter ();
  let y0 = Value_var.fresh ~loc ~name:"y" () in
  show_subst ~body:(lam y (lam y0 (app (var x) (app (var y) (var y0))))) ~x ~expr:(var y);
  [%expect
    {|
    (fun y -> (fun y#2 -> ((x)@((y)@(y#2)))))[x := y] =
    fun y#3 -> (fun y#2 -> ((y)@((y#3)@(y#2))))
  |}]
