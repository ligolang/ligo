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
  let replace_var v =
    if Var.equal v x
    then y
    else v in
  match e.content with
  | E_literal _ -> e
  | E_closure { binder ; body } ->
    let body = replace body in
    let binder = replace_var binder in
    return @@ E_closure { binder ; body }
  | E_skip -> e
  | E_constant (c, args) ->
    let args = List.map replace args in
    return @@ E_constant (c, args)
  | E_application (f, x) ->
    let (f, x) = Tuple.map2 replace (f, x) in
    return @@ E_application (f, x)
  | E_variable z ->
    let z = replace_var z in
    return @@ E_variable z
  | E_make_empty_map _ -> e
  | E_make_empty_big_map _ -> e
  | E_make_empty_list _ -> e
  | E_make_empty_set _ -> e
  | E_make_none _ -> e
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
  | E_let_in ((v, tv), e1, e2) ->
    let v = replace_var v in
    let e1 = replace e1 in
    let e2 = replace e2 in
    return @@ E_let_in ((v, tv), e1, e2)
  | E_sequence (e1, e2) ->
    let e1 = replace e1 in
    let e2 = replace e2 in
    return @@ E_sequence (e1, e2)
  | E_assignment (v, path, e) ->
    let v = replace_var v in
    let e = replace e in
    return @@ E_assignment (v, path, e)
  | E_update (r, updates) ->
    let r = replace r in
    let updates = List.map (fun (p,e)-> (p, replace e)) updates in
    return @@ E_update (r,updates)
  | E_while (cond, body) ->
    let cond = replace cond in
    let body = replace body in
    return @@ E_while (cond, body)

(**
   Computes `body[x := expr]`.
   This raises Bad_argument in the case of assignments with a name clash. (`x <- 42[x := 23]` makes no sense.)
**)
exception Bad_argument
let rec subst_expression : body:expression -> x:var_name -> expr:expression -> expression =
  fun ~body ~x ~expr ->
  let self body = subst_expression ~body ~x ~expr in
  let subst_binder y expr' =
    (* if x is shadowed, binder doesn't change *)
    if Var.equal x y
    then (y, expr')
    (* else, if no capture, subst in binder *)
    else if not (Free_variables.mem y (Free_variables.expression [] expr))
    then (y, self expr')
    (* else, avoid capture and subst in binder *)
    else
      let fresh = Var.fresh_like y in
      let new_body = replace expr' y fresh in
      (fresh, self new_body) in
  (* hack to avoid reimplementing subst_binder for 2-ary binder in E_if_cons:
     intuitively, we substitute in \hd tl. expr' as if it were \hd. \tl. expr *)
  let subst_binder2 y z expr' =
    let dummy = T_base Base_unit in
    let hack = { content = E_closure { binder = z ; body = expr' } ;
                 type_value = dummy } in
    match subst_binder y hack with
    | (y', { content = E_closure { binder = z' ; body = body } ; type_value = _dummy }) ->
      (y', z', { body with type_value = expr'.type_value })
    | _ -> assert false in
  let return content = {body with content} in
  let return_id = body in
  match body.content with
  | E_variable x' ->
     if x' = x
     then expr
     else return_id
  | E_closure { binder; body } -> (
    let (binder, body) = subst_binder binder body in
    return @@ E_closure { binder ; body }
  )
  | E_let_in ((v , tv) , expr , body) -> (
    let expr = self expr in
    let (v, body) = subst_binder v body in
    return @@ E_let_in ((v , tv) , expr , body)
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
  | E_literal _ | E_skip | E_make_none _
  | E_make_empty_map (_,_)
  | E_make_empty_big_map _
  | E_make_empty_list _
  | E_make_empty_set _ as em -> return em
  | E_constant (name, lst) -> (
      let lst' = List.map self lst in
      return @@ E_constant (name,lst')
  )
  | E_application farg -> (
      let farg' = Tuple.map2 self farg in
      return @@ E_application farg'
  )
  | E_while eb -> (
      let eb' = Tuple.map2 self eb in
      return @@ E_while eb'
  )
  | E_if_bool cab -> (
      let cab' = Tuple.map3 self cab in
      return @@ E_if_bool cab'
  )
  | E_sequence ab -> (
      let ab' = Tuple.map2 self ab in
      return @@ E_sequence ab'
  )
  | E_assignment (s, lrl, exp) -> (
      let exp' = self exp in
      if Var.equal s x then raise Bad_argument ;
      return @@ E_assignment (s, lrl, exp')
  )
  | E_update (r, updates) -> (
    let r' = self r in
    let updates' = List.map (fun (p,e) -> (p, self e)) updates in
    return @@ E_update(r',updates')
  )

let%expect_test _ =
  let dummy_type = T_base Base_unit in
  let wrap e = { content = e ; type_value = dummy_type } in

  let show_subst ~body ~x ~expr =
    Format.printf "(%a)[%a := %a] =@ %a"
      PP.expression body
      Var.pp x
      PP.expression expr
      PP.expression (subst_expression ~body ~x ~expr) in

  let x = Var.of_name "x" in
  let y = Var.of_name "y" in
  let z = Var.of_name "z" in

  let var x = wrap (E_variable x) in
  let app f x = wrap (E_application (f, x)) in
  let lam x u = wrap (E_closure { binder = x ; body = u }) in
  let unit = wrap (E_literal D_unit) in

  (* substituted var *)
  Var.reset_counter () ;
  show_subst
    ~body:(var x)
    ~x:x
    ~expr:unit ;
  [%expect{|
    (V(x))[x := L(unit)] =
    L(unit) |}] ;

  (* other var *)
  Var.reset_counter () ;
  show_subst
    ~body:(var y)
    ~x:x
    ~expr:unit ;
  [%expect{|
    (V(y))[x := L(unit)] =
    V(y)
  |}] ;

  (* closure shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(lam x (var x))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (C(fun x -> (V(x))))[x := L(unit)] =
    C(fun x -> (V(x)))
  |}] ;

  (* closure not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(lam y (var x))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (C(fun y -> (V(x))))[x := L(unit)] =
    C(fun y -> (L(unit)))
  |}] ;

  (* closure capture-avoidance *)
  Var.reset_counter () ;
  show_subst
    ~body:(lam y (app (var x) (var y)))
    ~x:x
    ~expr:(wrap (E_variable y)) ;
  [%expect{|
    (C(fun y -> ((V(x))@(V(y)))))[x := V(y)] =
    C(fun y#1 -> ((V(y))@(V(y#1))))
  |}] ;

  (* let-in shadowed (not in rhs) *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in ((x, dummy_type), var x, var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (let x = V(x) in ( V(x) ))[x := L(unit)] =
    let x = L(unit) in ( V(x) )
  |}] ;

  (* let-in not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in ((y, dummy_type), var x, var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (let y = V(x) in ( V(x) ))[x := L(unit)] =
    let y = L(unit) in ( L(unit) )
  |}] ;

  (* let-in capture avoidance *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in ((y, dummy_type), var x,
                           app (var x) (var y))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (let y = V(x) in ( (V(x))@(V(y)) ))[x := V(y)] =
    let y#1 = V(y) in ( (V(y))@(V(y#1)) )
  |}] ;

  (* iter shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((x , dummy_type) , var x) , var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (for_ITER x of V(x) do ( V(x) ))[x := L(unit)] =
    for_ITER x of L(unit) do ( V(x) )
  |}] ;

  (* iter not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((y , dummy_type) , var x) , var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (for_ITER y of V(x) do ( V(x) ))[x := L(unit)] =
    for_ITER y of L(unit) do ( L(unit) )
  |}] ;

  (* iter capture-avoiding *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((y , dummy_type) , app (var x) (var y)), app (var x) (var y))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (for_ITER y of (V(x))@(V(y)) do ( (V(x))@(V(y)) ))[x := V(y)] =
    for_ITER y#1 of (V(y))@(V(y)) do ( (V(y))@(V(y#1)) )
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
    (V(x) ?? V(x) : (x :: y) -> V(x))[x := L(unit)] =
    L(unit) ?? L(unit) : (x :: y) -> V(x)
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
    (V(x) ?? V(x) : (y :: x) -> V(x))[x := L(unit)] =
    L(unit) ?? L(unit) : (y :: x) -> V(x)
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
    (V(x) ?? V(x) : (y :: z) -> V(x))[x := L(unit)] =
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
    (V(x) ?? V(x) : (y :: z) -> (V(x))@((V(y))@(V(z))))[x := V(y)] =
    V(y) ?? V(y) : (y#1 :: z) -> (V(y))@((V(y#1))@(V(z)))
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
    (V(x) ?? V(x) : (y :: z) -> (V(x))@((V(y))@(V(z))))[x := V(z)] =
    V(z) ?? V(z) : (y :: z#1) -> (V(z))@((V(y))@(V(z#1)))
  |}] ;

  (* old bug *)
  Var.reset_counter () ;
  let y0 = Var.fresh ~name:"y" () in
  show_subst
    ~body:(lam y (lam y0 (app (var x) (app (var y) (var y0)))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (C(fun y -> (C(fun y#1 -> ((V(x))@((V(y))@(V(y#1))))))))[x := V(y)] =
    C(fun y#2 -> (C(fun y#1 -> ((V(y))@((V(y#2))@(V(y#1)))))))
  |}] ;
