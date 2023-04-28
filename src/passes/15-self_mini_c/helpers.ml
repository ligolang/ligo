module Pair   = Simple_utils.Pair
module Triple = Simple_utils.Triple
open Mini_c

type mapper_type = type_expression -> type_expression

let rec map_type_expression : mapper_type -> type_expression -> type_expression = fun f t ->
  let self = map_type_expression f in
  let self_annotated (a, b) = (a, self b) in
  let t' = f t in
  let return type_content = { t' with type_content } in
  match t'.type_content with
  | T_base _ as c -> return c
  | T_or (t1, t2) ->
    let t1 = self_annotated t1 in
    let t2 = self_annotated t2 in
    return @@ T_or (t1, t2)
  | T_tuple ts ->
    let ts = List.map ~f:self_annotated ts in
    return @@ T_tuple ts
  | T_function (t1, t2) ->
    let t1 = self t1 in
    let t2 = self t2 in
    return @@ T_function (t1, t2)
  | T_option t ->
    let t = self t in
    return @@ T_option t
  | T_list t ->
    let t = self t in
    return @@ T_list t
  | T_set t ->
    let t = self t in
    return @@ T_set t
  | T_contract t ->
    let t = self t in
    return @@ T_contract t
  | T_ticket t ->
    let t = self t in
    return @@ T_ticket t
  | T_map (t1, t2) ->
    let t1 = self t1 in
    let t2 = self t2 in
    return @@ T_map (t1, t2)
  | T_big_map (t1, t2) ->
    let t1 = self t1 in
    let t2 = self t2 in
    return @@ T_big_map (t1, t2)
  | (T_sapling_state _ | T_sapling_transaction _) as c -> return c

type mapper = expression -> expression

let rec map_expression : mapper -> expression -> expression = fun f e ->
  let self = map_expression f in
  let e' = f e in
  let return content = { e' with content } in
  match e'.content with
  | E_variable _ | E_literal _ | E_raw_michelson _
    as em -> return em
  | E_inline_michelson (code, arguments) -> (
      let arguments = List.map ~f:self arguments in
      return @@ E_inline_michelson (code, arguments)
  )
  | E_constant (c) -> (
      let lst = List.map ~f:self c.arguments in
      return @@ E_constant {cons_name = c.cons_name; arguments = lst}
  )
  | E_closure af -> (
      let body = self af.body in
      return @@ E_closure { af with body } 
  )
  | E_rec { func = af ; rec_binder } -> (
      let body = self af.body in
      return @@ E_rec { func = { af with body } ; rec_binder } 
  )
  | E_application farg -> (
      let farg' = Pair.map ~f:self farg in 
      return @@ E_application farg'
  )
  | E_iterator (s, ((name , tv) , body) , exp) -> (
      let (exp',body') = Pair.map ~f:self (exp,body) in
      return @@ E_iterator (s, ((name , tv) , body') , exp')
  )
  | E_fold (((name , tv) , body) , col , init) -> (
      let (body',col',init) = Triple.map ~f:self (body,col,init) in
      return @@ E_fold (((name , tv) , body') , col', init)
  )
  | E_fold_right (((name , tv) , body) , (col,el_ty) , init) -> (
      let (body',col',init) = Triple.map ~f:self (body,col,init) in
      return @@ E_fold_right (((name , tv) , body') , (col',el_ty), init)
  )
  | E_if_bool cab -> (
      let cab' = Triple.map ~f:self cab in
      return @@ E_if_bool cab'
  )
  | E_if_none (c, n, ((name, tv) , s)) -> (
      let (c',n',s') = Triple.map ~f:self (c,n,s) in
      return @@ E_if_none (c', n', ((name, tv) , s'))
  )
  | E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons)) -> (
      let (c',n',cons') = Triple.map ~f:self (c,n,cons) in
      return @@ E_if_cons (c', n', (((hd, hdtv) , (tl, tltv)) , cons'))
  )
  | E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r)) -> (
      let (c',l',r') = Triple.map ~f:self (c,l,r) in
      return @@ E_if_left (c', ((name_l, tvl) , l'), ((name_r, tvr) , r'))
  )
  | E_let_in (expr , inline , ((v , tv) , body)) -> (
      let (expr',body') = Pair.map ~f:self (expr,body) in
      return @@ E_let_in (expr', inline, ((v , tv) , body'))
  )
  | E_tuple exprs ->
      let exprs = List.map ~f:self exprs in
      return @@ E_tuple exprs
  | E_let_tuple (expr, (xs, body)) -> (
      let (expr', body') = Pair.map ~f:self (expr, body) in
      return @@ E_let_tuple (expr', (xs, body'))
  )
  | E_proj (expr, i, n) ->
      let expr = self expr in
      return @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
      let expr = self expr in
      let update = self update in
      return @@ E_update (expr, i, update, n)
  | E_global_constant (hash, args) ->
      let args = List.map ~f:self args in
      return @@ E_global_constant (hash, args)
  | E_create_contract (p, s, ((x, t), code), args) ->
      let args = List.map ~f:self args in
      let code = self code in
      return @@ E_create_contract (p, s, ((x, t), code), args)
  | E_let_mut_in (expr, ((x, a), body)) ->
      let expr = self expr in
      let body = self body in
      return @@ E_let_mut_in (expr, ((x, a), body))
  | E_deref x ->
      return @@ E_deref x
  | E_assign (x, e) ->
      let e = self e in
      return @@ E_assign (x, e)
  | E_for_each (coll, coll_type, (xs, body)) ->
      let coll = self coll in
      let body = self body in
      return @@ E_for_each (coll, coll_type, (xs, body))
  | E_for (start, final, incr, (x, body)) ->
      let start = self start in
      let final = self final in
      let incr = self incr in
      let body = self body in
      return @@ E_for (start, final, incr, (x, body))
  | E_while (cond, body) ->
      let cond = self cond in
      let body = self body in
      return @@ E_while (cond, body)

let map_sub_level_expression : mapper -> anon_function -> anon_function = fun f e ->
  let {binder ; body} : anon_function = e in
  let body = map_expression f body in
  {binder; body}
