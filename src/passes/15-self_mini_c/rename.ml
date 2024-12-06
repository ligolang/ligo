open Mini_c
open Ligo_prim
module Var_map = Map.Make (Value_var)

(** Global alpha renaming *)

(**
  The main goal is to ensures that every binder has a unique name globally.
  
  The main advantage is that a transformation such
    as moving a let will never capture a variable.

  let x = (let y = M in N) in K ↦ let y = M; let x = N in K
*)
let rec rename ~substs expr =
  let self expr = rename ~substs expr in
  let with_binder ~substs v =
    let v' = Value_var.fresh_like v in
    v', Map.update substs v ~f:(fun _ -> v')
  in
  let self_with_binder v ~body =
    let v, substs = with_binder ~substs v in
    let body = rename ~substs body in
    v, body
  in
  let self_with_binder_pair (v1, v2) ~body =
    let v1, substs = with_binder ~substs v1 in
    let v2, substs = with_binder ~substs v2 in
    let body = rename ~substs body in
    (v1, v2), body
  in
  let self_with_binders vs ~body =
    let rev_vs, substs =
      List.fold_left vs ~init:([], substs) ~f:(fun (rev_vs, substs) (v, vt) ->
          let v, substs = with_binder ~substs v in
          let rev_vs = (v, vt) :: rev_vs in
          rev_vs, substs)
    in
    let body = rename ~substs body in
    List.rev rev_vs, body
  in
  let solve_variable v =
    match Map.find substs v with
    | Some v' -> v'
    | None ->
      (* TODO: why does this happen? *)
      v
  in
  let return_expr content = { expr with content } in
  match expr.content with
  (* Here actual substitutions are created. *)
  | E_let_in (expr, inline, ((v, tv), body)) ->
    let expr = self expr in
    let v, body = self_with_binder v ~body in
    return_expr @@ E_let_in (expr, inline, ((v, tv), body))
  | E_let_mut_in (expr, ((v, tv), body)) ->
    let expr = self expr in
    let v, body = self_with_binder v ~body in
    return_expr @@ E_let_mut_in (expr, ((v, tv), body))
  (* Variables and dereferencing. *)
  | E_variable v ->
    let v = solve_variable v in
    return_expr @@ E_variable v
  | E_deref v ->
    let v = solve_variable v in
    return_expr @@ E_deref v
  (* Other stuff. *)
  | E_closure { binder; body } ->
    let binder, body = self_with_binder binder ~body in
    return_expr @@ E_closure { binder; body }
  | E_rec { func = { binder; body }; rec_binder } ->
    let (binder, rec_binder), body = self_with_binder_pair (binder, rec_binder) ~body in
    return_expr @@ E_rec { func = { binder; body }; rec_binder }
  | E_tuple exprs ->
    let exprs = List.map ~f:self exprs in
    return_expr @@ E_tuple exprs
  | E_let_tuple (expr, (vtvs, body)) ->
    let expr = self expr in
    let vtvs, body = self_with_binders vtvs ~body in
    return_expr @@ E_let_tuple (expr, (vtvs, body))
  | E_proj (expr, i, n) ->
    let expr = self expr in
    return_expr @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
    let expr = self expr in
    let update = self update in
    return_expr @@ E_update (expr, i, update, n)
  | E_iterator (s, ((name, tv), body), collection) ->
    let collection = self collection in
    let name, body = self_with_binder name ~body in
    return_expr @@ E_iterator (s, ((name, tv), body), collection)
  | E_fold (((name, tv), body), collection, init) ->
    let collection = self collection in
    let init = self init in
    let name, body = self_with_binder name ~body in
    return_expr @@ E_fold (((name, tv), body), collection, init)
  | E_fold_right (((name, tv), body), (collection, elem_tv), init) ->
    let collection = self collection in
    let init = self init in
    let name, body = self_with_binder name ~body in
    return_expr @@ E_fold_right (((name, tv), body), (collection, elem_tv), init)
  | E_if_none (c, n, ((name, tv), s)) ->
    let c = self c in
    let n = self n in
    let name, s = self_with_binder name ~body:s in
    return_expr @@ E_if_none (c, n, ((name, tv), s))
  | E_if_cons (c, n, (((hd, hdtv), (tl, tltv)), cons)) ->
    let c = self c in
    let n = self n in
    let (hd, tl), cons = self_with_binder_pair (hd, tl) ~body:cons in
    return_expr @@ E_if_cons (c, n, (((hd, hdtv), (tl, tltv)), cons))
  | E_if_left (c, ((name_l, tvl), l), ((name_r, tvr), r)) ->
    let c = self c in
    let name_l, l = self_with_binder name_l ~body:l in
    let name_r, r = self_with_binder name_r ~body:r in
    return_expr @@ E_if_left (c, ((name_l, tvl), l), ((name_r, tvr), r))
  | E_raw_michelson code -> return_expr @@ E_raw_michelson code
  | E_inline_michelson (code, args) ->
    let args' = List.map ~f:self args in
    return_expr @@ E_inline_michelson (code, args')
  | E_literal _ -> expr
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:self arguments in
    return_expr @@ E_constant { cons_name; arguments }
  | E_application (farg1, farg2) ->
    let farg1 = self farg1 in
    let farg2 = self farg2 in
    return_expr @@ E_application (farg1, farg2)
  | E_if_bool (cab1, cab2, cab3) ->
    let cab1 = self cab1 in
    let cab2 = self cab2 in
    let cab3 = self cab3 in
    return_expr @@ E_if_bool (cab1, cab2, cab3)
  | E_global_constant (hash, args) ->
    let args = List.map ~f:self args in
    return_expr @@ E_global_constant (hash, args)
  | E_create_contract (p, s, ((x, t), code), args) ->
    let args = List.map ~f:self args in
    let x, code = self_with_binder x ~body:code in
    return_expr @@ E_create_contract (p, s, ((x, t), code), args)
  (* Mutable stuff. We allow substituting mutable variables which
      aren't mutated because it was easy. Hmm. *)
  | E_assign (v, e) ->
    let v = solve_variable v in
    let e = self e in
    return_expr @@ E_assign (v, e)
  | E_for (start, final, incr, ((x, a), body)) ->
    let start = self start in
    let final = self final in
    let incr = self incr in
    let x, body = self_with_binder x ~body in
    return_expr @@ E_for (start, final, incr, ((x, a), body))
  | E_for_each (coll, coll_type, (xs, body)) ->
    let coll = self coll in
    let xs, body = self_with_binders xs ~body in
    return_expr @@ E_for_each (coll, coll_type, (xs, body))
  | E_while (cond, body) ->
    let cond = self cond in
    let body = self body in
    return_expr @@ E_while (cond, body)


let rename expr = rename ~substs:Var_map.empty expr
