open Ligo_prim
open Mini_c

(* Reference implementation:
   https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/interp/lambda-subst/main.ml

   ...but, it has at least one bug: in subst,
   `let new_body = replace e' y fresh in ...` should be:
   `let new_body = replace e' fresh y in ...`,
   due to the arg order choice for replace.

   Below, this bug is fixed by adopting the other order choice for
   replace (as well as subst).  *)

let replace_var : var_name -> var_name -> var_name -> var_name =
  fun v x y ->
  if Value_var.equal v x
  then y
  else v

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
  | E_closure { binder ; body } ->
    let body = replace body in
    let binder = replace_var binder in
    return @@ E_closure { binder ; body }
  | E_constant (c) ->
    let args = List.map ~f:replace c.arguments in
    return @@ E_constant {cons_name = c.cons_name; arguments = args}
  | E_application (f, x) ->
    let (f, x) = Simple_utils.Tuple.map2 replace (f, x) in
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
    return @@ E_fold_right (((v, tv), body), (collection,elem_tv), initial)
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
    let vtvs = List.map ~f:(fun (v, tv) -> (replace_var v, tv)) vtvs in
    let body = replace body in
    return @@ E_let_tuple (expr, (vtvs, body))
  | E_proj (expr, i, n) ->
    let expr = replace expr in
    return @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
    let expr = replace expr in
    let update = replace update in
    return @@ E_update (expr, i, update, n)
  | E_raw_michelson (code, args) ->
    let args = List.map ~f:replace args in
    return @@ E_raw_michelson (code, args)
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
    let xs = List.map ~f:(fun (x, a) -> (replace_var x, a)) xs in
    let body = replace body in
    return @@ E_for_each (coll, coll_type, (xs, body))
  | E_while (cond, body) ->
    let cond = replace cond in
    let body = replace body in
    return @@ E_while (cond, body)


(* Given an implementation of substitution on an arbitary type of
   body, implements substitution on a binder (pair of bound variable
   and body) *)
let subst_binder : type body.
  (body:body -> x:var_name -> expr:expression -> body) ->
  (body -> var_name -> var_name -> body) ->
  body:(var_name * body) -> x:var_name -> expr:expression -> (var_name * body) =
  fun subst replace ~body:(y, body) ~x ~expr ->
    (* if x is shadowed, binder doesn't change *)
    if Value_var.equal x y
    then (y, body)
    (* else, if no capture, subst in binder *)
    else if not (Free_variables.mem (get_fv [] expr) y)
    then (y, subst ~body ~x ~expr)
    (* else, avoid capture and subst in binder *)
    else
      let fresh = Value_var.fresh_like y in
      let body = replace body y fresh in
      (fresh, subst ~body ~x ~expr)

(* extending to general n-ary binders *)
type 'body binds = var_name list * 'body

let replace_binds : type body.
  (body -> var_name -> var_name -> body) ->
  (body binds -> var_name -> var_name -> body binds) =
  fun replace (vars, body) x y ->
  (List.map ~f:(fun v -> replace_var v x y) vars,
   replace body x y)

let rec subst_binds : type body.
  (body:body -> x:var_name -> expr:expression -> body) ->
  (body -> var_name -> var_name -> body) ->
  body:(body binds) -> x:var_name -> expr:expression -> body binds =
  fun subst replace ~body ~x ~expr ->
  match body with
  | ([], body) -> ([], subst ~body ~x ~expr)
  | (v :: vs, body) ->
    let (v, (vs, body)) =
      subst_binder
        (subst_binds subst replace)
        (replace_binds replace)
        ~body:(v, (vs, body)) ~x ~expr in
    (v :: vs, body)

(**
   Computes `body[x := expr]`.
**)
let rec subst_expression : body:expression -> x:var_name -> expr:expression -> expression =
  fun ~body ~x ~expr ->
  let self body = subst_expression ~body ~x ~expr in
  let subst_binder1 =
    subst_binder subst_expression replace in
  let subst_binder2 =
    subst_binder
      subst_binder1
      (fun (x, body) y z -> (replace_var x y z, replace body y z)) in
  let subst_binds = subst_binds subst_expression replace in
  let self_binder1 ~body = subst_binder1 ~body ~x ~expr in
  let self_binder2 ~body = subst_binder2 ~body ~x ~expr in
  let self_binds ~body = subst_binds ~body ~x ~expr in
  let return content = {body with content} in
  let return_id = body in
  match body.content with
  | E_variable x' ->
     if Value_var.equal x' x
     then expr
     else return_id
  | E_closure { binder; body } -> (
    let (binder, body) = self_binder1 ~body:(binder, body) in
    return @@ E_closure { binder ; body }
  )
  | E_let_in (expr, inline, ((v , tv), body)) -> (
    let expr = self expr in
    let (v, body) = self_binder1 ~body:(v, body) in
    return @@ E_let_in (expr, inline, ((v , tv) , body))
  )
  | E_tuple exprs ->
    let exprs = List.map ~f:self exprs in
    return @@ E_tuple exprs
  | E_let_tuple (expr, (vtvs, body)) -> (
    let expr = self expr in
    let (vs, tvs) = List.unzip vtvs in
    let (vs, body) = self_binds ~body:(vs, body) in
    let vtvs = List.zip_exn vs tvs in
    return @@ E_let_tuple (expr, (vtvs, body))
  )
  | E_proj (expr, i, n) ->
    let expr = self expr in
    return @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
    let expr = self expr in
    let update = self update in
    return @@ E_update (expr, i, update, n)
  | E_iterator (s, ((name , tv) , body) , collection) -> (
    let (name, body) = self_binder1 ~body:(name, body) in
    let collection = self collection in
    return @@ E_iterator (s, ((name , tv) , body) , collection)
  )
  | E_fold (((name , tv) , body) , collection , init) -> (
    let (name, body) = self_binder1 ~body:(name, body) in
    let collection = self collection in
    let init = self init in
    return @@ E_fold (((name , tv) , body) , collection , init)
  )
  | E_fold_right (((name , tv) , body) , (collection, elem_tv) , init) -> (
    let (name, body) = self_binder1 ~body:(name, body) in
    let collection = self collection in
    let init = self init in
    return @@ E_fold_right (((name , tv) , body) , (collection, elem_tv) , init)
  )
  | E_if_none (c, n, ((name, tv) , s)) -> (
    let c = self c in
    let n = self n in
    let (name, s) = self_binder1 ~body:(name, s) in
    return @@ E_if_none (c, n, ((name, tv) , s))
  )
  | E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons)) -> (
    let c = self c in
    let n = self n in
    let (hd, (tl, cons)) = self_binder2 ~body:(hd, (tl, cons)) in
    return @@ E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons))
  )
  | E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r)) -> (
    let c = self c in
    let (name_l, l) = self_binder1 ~body:(name_l, l) in
    let (name_r, r) = self_binder1 ~body:(name_r, r) in
    return @@ E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r))
  )
  | E_raw_michelson (code, args) ->
    let args = List.map ~f:self args in
    return @@ E_raw_michelson (code, args)
  | E_literal _ ->
    return_id
  | E_constant {cons_name; arguments} -> (
      let arguments = List.map ~f:self arguments in
      return @@ E_constant {cons_name; arguments}
  )
  | E_application farg -> (
      let farg' = Simple_utils.Tuple.map2 self farg in
      return @@ E_application farg'
  )
  | E_if_bool cab -> (
      let cab' = Simple_utils.Tuple.map3 self cab in
      return @@ E_if_bool cab'
  )
  | E_global_constant (hash, args) ->
    let args = List.map ~f:self args in
    return @@ E_global_constant (hash, args)
  | E_create_contract (p, s, ((x, t), code), args) ->
    let args = List.map ~f:self args in
    let (x, code) = self_binder1 ~body:(x, code) in
    return @@ E_create_contract (p, s, ((x, t), code), args)
  (* Mutable stuff. We allow substituting mutable variables which
     aren't mutated because it was easy. Hmm. *)
  | E_let_mut_in (expr, ((v, tv), body)) ->
    let expr = self expr in
    let (v, body) = self_binder1 ~body:(v, body) in
    return @@ E_let_mut_in (expr, ((v , tv) , body))
  | E_deref x' ->
    if Value_var.equal x' x
    then expr
    else return_id
  | E_assign (x', e) ->
    if Value_var.equal x' x
    then failwith ("internal error, please report this as a bug: tried to substitute for mutated var " ^ __LOC__)
    else
      let e = self e in
      return @@ E_assign (x', e)
  | E_for (start, final, incr, ((x, a), body)) ->
    let start = self start in
    let final = self final in
    let incr = self incr in
    let (x, body) = self_binder1 ~body:(x, body) in
    return @@ E_for (start, final, incr, ((x, a), body))
  | E_for_each (coll, coll_type, (xs, body)) ->
    let coll = self coll in
    let (xs, ts) = List.unzip xs in
    let (xs, body) = self_binds ~body:(xs, body) in
    let xs = List.zip_exn xs ts in
    return @@ E_for_each (coll, coll_type, (xs, body))
  | E_while (cond, body) ->
    let cond = self cond in
    let body = self body in
    return @@ E_while (cond, body)


let%expect_test _ =
  let loc = Location.dummy in
  let dummy_type = Expression.make_t @@ T_base TB_unit in
  let wrap e = Expression.make e dummy_type in

  let show_subst ~body ~(x:var_name) ~expr =
    Format.printf "(%a)[%a := %a] =@ %a"
      PP.expression body
      Value_var.pp x
      PP.expression expr
      PP.expression (subst_expression ~body ~x ~expr) in

  let x = Value_var.of_input_var ~loc "x" in
  let y = Value_var.of_input_var ~loc "y" in
  let z = Value_var.of_input_var ~loc "z" in

  let var x = wrap (E_variable x) in
  let app f x = wrap (E_application (f, x)) in
  let lam x u = wrap (E_closure { binder = x ; body = u }) in
  let unit = wrap (E_literal Literal_unit) in

  (* substituted var *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(var x)
    ~x:x
    ~expr:unit ;
  [%expect{|
    (x)[x := L(unit)] =
    L(unit) |}] ;

  (* other var *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(var y)
    ~x:x
    ~expr:unit ;
  [%expect{|
    (y)[x := L(unit)] =
    y
  |}] ;

  (* closure shadowed *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(lam x (var x))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (fun x -> (x))[x := L(unit)] =
    fun x -> (x)
  |}] ;

  (* closure not shadowed *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(lam y (var x))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (fun y -> (x))[x := L(unit)] =
    fun y -> (L(unit))
  |}] ;

  (* closure capture-avoidance *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(lam y (app (var x) (var y)))
    ~x:x
    ~expr:(wrap (E_variable y)) ;
  [%expect{|
    (fun y -> ((x)@(y)))[x := y] =
    fun y#2 -> ((y)@(y#2))
  |}] ;

  (* let-in shadowed (not in rhs) *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in (var x, false, ((x, dummy_type), var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (let x = x in x)[x := L(unit)] =
    let x = L(unit) in x
  |}] ;

  (* let-in not shadowed *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in (var x, false, ((y, dummy_type), var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (let y = x in x)[x := L(unit)] =
    let y = L(unit) in L(unit)
  |}] ;

  (* let-in capture avoidance *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in (var x, false, ((y, dummy_type),
                           app (var x) (var y)))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (let y = x in (x)@(y))[x := y] =
    let y#3 = y in (y)@(y#3)
  |}] ;

  (* iter shadowed *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((x , dummy_type) , var x) , var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (for_ITER x of x do ( x ))[x := L(unit)] =
    for_ITER x of L(unit) do ( x )
  |}] ;

  (* iter not shadowed *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((y , dummy_type) , var x) , var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (for_ITER y of x do ( x ))[x := L(unit)] =
    for_ITER y of L(unit) do ( L(unit) )
  |}] ;

  (* iter capture-avoiding *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((y , dummy_type) , app (var x) (var y)), app (var x) (var y))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (for_ITER y of (x)@(y) do ( (x)@(y) ))[x := y] =
    for_ITER y#4 of (y)@(y) do ( (y)@(y#4) )
  |}] ;

  (* if_cons shadowed 1 *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((x, dummy_type), (y, dummy_type)),
                             var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (x ?? x : (x :: y) -> x)[x := L(unit)] =
    L(unit) ?? L(unit) : (x :: y) -> x
  |}] ;

  (* if_cons shadowed 2 *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (x, dummy_type)),
                             var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (x ?? x : (y :: x) -> x)[x := L(unit)] =
    L(unit) ?? L(unit) : (y :: x) -> x
  |}] ;

  (* if_cons not shadowed *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (z, dummy_type)),
                             var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (x ?? x : (y :: z) -> x)[x := L(unit)] =
    L(unit) ?? L(unit) : (y :: z) -> L(unit)
  |}] ;

  (* if_cons capture avoidance 1 *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (z, dummy_type)),
                             app (var x) (app (var y) (var z))))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (x ?? x : (y :: z) -> (x)@((y)@(z)))[x := y] =
    y ?? y : (y#5 :: z) -> (y)@((y#5)@(z))
  |}] ;

  (* if_cons capture avoidance 2 *)
  Type_var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (z, dummy_type)),
                             app (var x) (app (var y) (var z))))))
    ~x:x
    ~expr:(var z) ;
  [%expect{|
    (x ?? x : (y :: z) -> (x)@((y)@(z)))[x := z] =
    z ?? z : (y :: z#6) -> (z)@((y)@(z#6))
  |}] ;

  (* old bug *)
  Value_var.reset_counter () ;
  let y0 = Value_var.fresh ~loc ~name:"y" () in
  show_subst
    ~body:(lam y (lam y0 (app (var x) (app (var y) (var y0)))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (fun y -> (fun y#2 -> ((x)@((y)@(y#2)))))[x := y] =
    fun y#3 -> (fun y#2 -> ((y)@((y#3)@(y#2))))
  |}] ;
