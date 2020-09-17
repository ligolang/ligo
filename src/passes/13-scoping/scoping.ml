open Co_de_bruijn.Util

module I = Mini_c
module O = Co_de_bruijn

let translate_var (x : I.var_name) (env : I.environment) =
  let (_, idx) = I.Environment.Environment.get_i x env in
  let usages = List.repeat idx Drop
               @ [ Keep ]
               @ List.repeat (List.length env - idx - 1) Drop in
  (O.E_variable, usages)

let rec translate_expression (expr : I.expression) (env : I.environment) =
  let return (content, usages) =
    (O.{ content = content ;
         type_expression = expr.type_expression ;
         location = expr.location },
     usages) in
  let use_nothing = List.repeat (List.length env) Drop in
  match expr.content with
  | E_literal v ->
    return (E_literal v, use_nothing)
  | E_variable x ->
    return (translate_var x env)
  | E_closure { binder; body } ->
    let (binder_type, _) =
      (* TODO move binder type to the binder, like all other binders? *)
      (* at the moment, this is the only error here! so I am not
         bothering with error machinery... *)
      match Mini_c.get_t_function expr.type_expression with
      | None -> failwith ("function is not a function " ^ __LOC__)
      | Some t -> t in
    let binder = (binder, binder_type) in
    let (binder, usages) = translate_binder (binder, body) env in
    return (E_closure binder, usages)
  | E_constant { cons_name; arguments } ->
    let arguments = List.map (fun argument -> translate_expression argument env) arguments in
    let (arguments, usages) =
      List.fold_right
        (fun (arg, arg_usages) (args, args_usages) ->
           let (ss, us) = coproduct arg_usages args_usages in
           (O.Arg_cons (ss, arg, args), us))
        arguments
        (O.Arg_nil, use_nothing) in
    return (E_constant { cons_name ; arguments }, usages)
  | E_application (f, x) ->
    let (f, f_usages) = translate_expression f env in
    let (x, x_usages) = translate_expression x env in
    let (ss, us) = coproduct f_usages x_usages in
    return (E_application (ss, f, x), us)
  | E_iterator (name, body, expr) ->
    let (body, body_usages) = translate_binder body env in
    let (expr, expr_usages) = translate_expression expr env in
    let (ss, us) = coproduct body_usages expr_usages in
    return (E_iterator (name, (ss, body, expr)), us)
  | E_fold (body, coll, init) ->
    let (body, body_usages) = translate_binder body env in
    let (coll, coll_usages) = translate_expression coll env in
    let (init, init_usages) = translate_expression init env in
    let (ss1, us1) = coproduct coll_usages body_usages in
    let (ss2, us2) = coproduct init_usages us1 in
    return (E_fold (ss2, init, (ss1, coll, body)), us2)
  | E_if_bool (e1, e2, e3) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_expression e2 env in
    let (e3, us3) = translate_expression e3 env in
    let (inner, inner_us) = coproduct us2 us3 in
    let (outer, outer_us) = coproduct us1 inner_us in
    return (E_if_bool (outer, e1, (inner, e2, e3)), outer_us)
  | E_if_none (e1, e2, e3) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_expression e2 env in
    let (e3, us3) = translate_binder e3 env in
    let (inner, inner_us) = coproduct us2 us3 in
    let (outer, outer_us) = coproduct us1 inner_us in
    return (E_if_none (outer, e1, (inner, e2, e3)), outer_us)
  (* NB: flipping around because it is backwards in Mini_c *)
  | E_if_cons (e1, e3, e2) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_binder2 e2 env in
    let (e3, us3) = translate_expression e3 env in
    let (inner, inner_us) = coproduct us2 us3 in
    let (outer, outer_us) = coproduct us1 inner_us in
    return (E_if_cons (outer, e1, (inner, e2, e3)), outer_us)
  | E_if_left (e1, e2, e3) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_binder e2 env in
    let (e3, us3) = translate_binder e3 env in
    let (inner, inner_us) = coproduct us2 us3 in
    let (outer, outer_us) = coproduct us1 inner_us in
    return (E_if_left (outer, e1, (inner, e2, e3)), outer_us)
  | E_let_in (binder, inline, e1, e2) ->
    let e2 = (binder, e2) in
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_binder e2 env in
    let (ss, us) = coproduct us1 us2 in
    return (E_let_in (inline, (ss, e1, e2)), us)
  | E_record_update (e1, path, e2) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_expression e2 env in
    let (ss, us) = coproduct us1 us2 in
    return (E_record_update (path, (ss, e1, e2)), us)
  | E_raw_michelson code ->
    return (E_raw_michelson code, use_nothing)

and translate_binder (binder, body) env =
  let env' = I.Environment.add binder env in
  let (body, usages) = translate_expression body env' in
  let (_, binder_type) = binder in
  ((binder_type, List.hd usages, body), List.tl usages)

(* TODO is this right? *)
and translate_binder2 ((binder1, binder2), body) env =
  let env' = I.Environment.add binder2 (I.Environment.add binder1 env) in
  let (body, usages) = translate_expression body env' in
  let (_, binder1_type) = binder1 in
  let (_, binder2_type) = binder2 in
  ((binder1_type, List.hd (List.tl usages),
    (binder2_type, List.hd usages, body)),
   List.tl (List.tl usages))

let translate_closed_function ({ binder ; body } : I.anon_function) input_ty : O.expression O.bind =
  let (body, usages) = translate_expression body (Mini_c.Environment.add (binder, input_ty) []) in
  (input_ty, List.hd usages, body)


