open Mini_c

(* Reference implementation:
   https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/interp/lambda-subst/main.ml

   ...but, it has at least one bug: in subst,
   `let new_body = replace e' y fresh in ...` should be:
   `let new_body = replace e' fresh y in ...`,
   due to the arg order choice for replace.

   Below, this bug is fixed by adopting the other order choice for
   replace (as well as subst).  *)


(* replace in `e` the variable `x` with `y`.

   It would be fine -- better? -- to only replace the _free_ x.
*)
let rec replace : expression -> var_name -> var_name -> expression =
  fun e x y ->
  let replace e = replace e x y in
  let return content = { e with content } in
  let replace_var (v:var_name) =
    if Var.equal v.wrap_content x.wrap_content
    then y
    else v in
  match e.content with
  | E_literal _ -> e
  | E_closure { binder ; body } ->
    let body = replace body in
    let binder = replace_var binder in
    return @@ E_closure { binder ; body }
  | E_constant (c) ->
    let args = List.map replace c.arguments in
    return @@ E_constant {cons_name = c.cons_name; arguments = args}
  | E_application (f, x) ->
    let (f, x) = Tuple.map2 replace (f, x) in
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
  | E_let_in ((v, tv), inline, e1, e2) ->
    let v = replace_var v in
    let e1 = replace e1 in
    let e2 = replace e2 in
    return @@ E_let_in ((v, tv), inline, e1, e2)
  | E_let_pair (expr, (((v1, tv1), (v2, tv2)), body)) ->
    let expr = replace expr in
    let v1 = replace_var v1 in
    let v2 = replace_var v2 in
    let body = replace body in
    return @@ E_let_pair (expr, (((v1, tv1), (v2, tv2)), body))
  | E_raw_michelson _ -> e

(**
   Computes `body[x := expr]`.
   This raises Bad_argument in the case of assignments with a name clash. (`x <- 42[x := 23]` makes no sense.)
**)
let rec subst_expression : body:expression -> x:var_name -> expr:expression -> expression =
  fun ~body ~x ~expr ->
  let self body = subst_expression ~body ~x ~expr in
  let subst_binder (y:var_name) expr' =
    (* if x is shadowed, binder doesn't change *)
    if Var.equal x.wrap_content y.wrap_content
    then (y, expr')
    (* else, if no capture, subst in binder *)
    else if not (Free_variables.mem y (Free_variables.expression [] expr))
    then (y, self expr')
    (* else, avoid capture and subst in binder *)
    else
      let fresh = Location.wrap @@ Var.fresh_like y.wrap_content in
      let new_body = replace expr' y fresh in
      (fresh, self new_body) in
  (* hack to avoid reimplementing subst_binder for 2-ary binder in E_if_cons:
     intuitively, we substitute in \hd tl. expr' as if it were \hd. \tl. expr *)
  let subst_binder2 y z expr' =
    let dummy = Expression.make_t @@ T_base TB_unit in
    let hack = Expression.make (E_closure { binder = z ; body = expr' }) dummy in
    match subst_binder y hack with
    | (y', { content = E_closure { binder = z' ; body = body } ; type_expression = _dummy }) ->
      (y', z', { body with type_expression = expr'.type_expression })
    | _ -> assert false in
  let return content = {body with content} in
  let return_id = body in
  match body.content with
  | E_variable x' ->
     if Location.equal_content ~equal:Var.equal x' x
     then expr
     else return_id
  | E_closure { binder; body } -> (
    let (binder, body) = subst_binder binder body in
    return @@ E_closure { binder ; body }
  )
  | E_let_in ((v , tv) , inline, expr , body) -> (
    let expr = self expr in
    let (v, body) = subst_binder v body in
    return @@ E_let_in ((v , tv) , inline, expr , body)
  )
  | E_let_pair (expr, (((v1, tv1), (v2, tv2)), body)) -> (
    let expr = self expr in
    let (v1, v2, body) = subst_binder2 v1 v2 body in (* TODO is this backwards? *)
    return @@ E_let_pair (expr, (((v1, tv1), (v2, tv2)), body))
  )
  | E_iterator (s, ((name , tv) , body) , collection) -> (
    let (name, body) = subst_binder name body in
    let collection = self collection in
    return @@ E_iterator (s, ((name , tv) , body) , collection)
  )
  | E_fold (((name , tv) , body) , collection , init) -> (
    let (name, body) = subst_binder name body in
    let collection = self collection in
    let init = self init in
    return @@ E_fold (((name , tv) , body) , collection , init)
  )
  | E_if_none (c, n, ((name, tv) , s)) -> (
    let c = self c in
    let n = self n in
    let (name, s) = subst_binder name s in
    return @@ E_if_none (c, n, ((name, tv) , s))
  )
  | E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons)) -> (
    let c = self c in
    let n = self n in
    let (hd, tl, cons) = subst_binder2 hd tl cons in
    return @@ E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons))
  )
  | E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r)) -> (
    let c = self c in
    let (name_l, l) = subst_binder name_l l in
    let (name_r, r) = subst_binder name_r r in
    return @@ E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r))
  )
  (* All that follows is boilerplate *)
  | E_literal _ | E_raw_michelson _
    as em -> return em
  | E_constant (c) -> (
      let lst = List.map self c.arguments in
      return @@ E_constant {cons_name = c.cons_name; arguments = lst }
  )
  | E_application farg -> (
      let farg' = Tuple.map2 self farg in
      return @@ E_application farg'
  )
  | E_if_bool cab -> (
      let cab' = Tuple.map3 self cab in
      return @@ E_if_bool cab'
  )

let%expect_test _ =
  let dummy_type = Expression.make_t @@ T_base TB_unit in
  let wrap e = Expression.make e dummy_type in

  let show_subst ~body ~(x:var_name) ~expr =
    Format.printf "(%a)[%a := %a] =@ %a"
      PP.expression body
      Var.pp x.wrap_content
      PP.expression expr
      PP.expression (subst_expression ~body ~x ~expr) in

  let x = Location.wrap @@ Var.of_name "x" in
  let y = Location.wrap @@ Var.of_name "y" in
  let z = Location.wrap @@ Var.of_name "z" in

  let var x = wrap (E_variable x) in
  let app f x = wrap (E_application (f, x)) in
  let lam x u = wrap (E_closure { binder = x ; body = u }) in
  let unit = wrap (E_literal Literal_unit) in

  (* substituted var *)
  Var.reset_counter () ;
  show_subst
    ~body:(var x)
    ~x:x
    ~expr:unit ;
  [%expect{|
    (x)[x := L(unit)] =
    L(unit) |}] ;

  (* other var *)
  Var.reset_counter () ;
  show_subst
    ~body:(var y)
    ~x:x
    ~expr:unit ;
  [%expect{|
    (y)[x := L(unit)] =
    y
  |}] ;

  (* closure shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(lam x (var x))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (fun x -> (x))[x := L(unit)] =
    fun x -> (x)
  |}] ;

  (* closure not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(lam y (var x))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (fun y -> (x))[x := L(unit)] =
    fun y -> (L(unit))
  |}] ;

  (* closure capture-avoidance *)
  Var.reset_counter () ;
  show_subst
    ~body:(lam y (app (var x) (var y)))
    ~x:x
    ~expr:(wrap (E_variable y)) ;
  [%expect{|
    (fun y -> ((x)@(y)))[x := y] =
    fun y#1 -> ((y)@(y#1))
  |}] ;

  (* let-in shadowed (not in rhs) *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in ((x, dummy_type), false, var x, var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (let x = x in x)[x := L(unit)] =
    let x = L(unit) in x
  |}] ;

  (* let-in not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in ((y, dummy_type), false, var x, var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (let y = x in x)[x := L(unit)] =
    let y = L(unit) in L(unit)
  |}] ;

  (* let-in capture avoidance *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in ((y, dummy_type), false, var x,
                           app (var x) (var y))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (let y = x in (x)@(y))[x := y] =
    let y#1 = y in (y)@(y#1)
  |}] ;

  (* iter shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((x , dummy_type) , var x) , var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (for_ITER x of x do ( x ))[x := L(unit)] =
    for_ITER x of L(unit) do ( x )
  |}] ;

  (* iter not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((y , dummy_type) , var x) , var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (for_ITER y of x do ( x ))[x := L(unit)] =
    for_ITER y of L(unit) do ( L(unit) )
  |}] ;

  (* iter capture-avoiding *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((y , dummy_type) , app (var x) (var y)), app (var x) (var y))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (for_ITER y of (x)@(y) do ( (x)@(y) ))[x := y] =
    for_ITER y#1 of (y)@(y) do ( (y)@(y#1) )
  |}] ;

  (* if_cons shadowed 1 *)
  Var.reset_counter () ;
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
  Var.reset_counter () ;
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
  Var.reset_counter () ;
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
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (z, dummy_type)),
                             app (var x) (app (var y) (var z))))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (x ?? x : (y :: z) -> (x)@((y)@(z)))[x := y] =
    y ?? y : (y#1 :: z) -> (y)@((y#1)@(z))
  |}] ;

  (* if_cons capture avoidance 2 *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (z, dummy_type)),
                             app (var x) (app (var y) (var z))))))
    ~x:x
    ~expr:(var z) ;
  [%expect{|
    (x ?? x : (y :: z) -> (x)@((y)@(z)))[x := z] =
    z ?? z : (y :: z#1) -> (z)@((y)@(z#1))
  |}] ;

  (* old bug *)
  Var.reset_counter () ;
  let y0 = Location.wrap @@ Var.fresh ~name:"y" () in
  show_subst
    ~body:(lam y (lam y0 (app (var x) (app (var y) (var y0)))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (fun y -> (fun y#1 -> ((x)@((y)@(y#1)))))[x := y] =
    fun y#2 -> (fun y#1 -> ((y)@((y#2)@(y#1))))
  |}] ;
