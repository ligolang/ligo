module Pair   = Simple_utils.Pair
module Triple = Simple_utils.Triple
open Mini_c

type mapper = expression -> expression

let rec map_expression : mapper -> expression -> expression = fun f e ->
  let self = map_expression f in
  let e' = f e in
  let return content = { e' with content } in
  match e'.content with
  | E_variable _ | E_literal _ | E_raw_michelson _
    as em -> return em
  | E_constant (c) -> (
      let lst = List.map ~f:self c.arguments in
      return @@ E_constant {cons_name = c.cons_name; arguments = lst}
  )
  | E_closure af -> (
      let body = self af.body in
      return @@ E_closure { af with body } 
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
  | E_let_in (expr , inline , thunk, ((v , tv) , body)) -> (
      let (expr',body') = Pair.map ~f:self (expr,body) in
      return @@ E_let_in (expr', inline, thunk, ((v , tv) , body'))
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

let map_sub_level_expression : mapper -> anon_function -> anon_function = fun f e ->
  let {binder ; body} : anon_function = e in
  let body = map_expression f body in
  {binder; body}
