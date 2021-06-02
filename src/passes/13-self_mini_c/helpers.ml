open Mini_c
open Trace

let rec fold_type_value : ('a -> type_expression -> ('a,_) result) -> 'a -> type_expression -> ('a,_) result = fun f init t ->
  let self = fold_type_value f in
  let* init' = f init t in
  match t.type_content with
  | T_tuple ts ->
    bind_fold_list self init' (List.map ~f:snd ts)
  | T_or ((_, a), (_, b))
  | T_function (a, b)
  | T_map (a, b)
  | T_big_map (a, b) ->
     bind_fold_pair self init' (a, b)
  | T_list a
  | T_set a
  | T_contract a
  | T_option a ->
     self init' a
  | T_base _ ->
    ok init'
  | T_sapling_transaction _ -> ok init
  | T_sapling_state _ -> ok init
  | T_ticket x -> self init' x

type ('a,'err) folder = 'a -> expression -> ('a,'err) result
let rec fold_expression : ('a,'err) folder -> 'a -> expression -> ('a, 'err) result = fun f init e ->
  let self = fold_expression f in 
  let* init' = f init e in
  match e.content with
  | E_variable _
  | E_raw_michelson _
  | E_literal _ -> ok init'
  | E_constant (c) -> (
      let* res = bind_fold_list self init' c.arguments in
      ok res
  )
  | E_closure af -> (
      let* res = self init' af.body in
      ok res
  )
  | E_application farg -> (
      let* res = bind_fold_pair self init' farg in 
      ok res
  )
  | E_iterator (_, ((_ , _) , body) , exp) -> (
      let* res = bind_fold_pair self init' (exp,body) in
      ok res
  )
  | E_fold (((_ , _) , body) , col , init) -> (
      let* res = bind_fold_triple self init' (body,col,init) in
      ok res
  )
  | E_fold_right (((_ , _) , body) , (col,_) , init) -> (
      let* res = bind_fold_triple self init' (body,col,init) in
      ok res
  )
  | E_if_bool cab -> (
      let* res = bind_fold_triple self init' cab in
      ok res
  )
  | E_if_none (c, n, ((_, _) , s)) -> (
      let* res = bind_fold_triple self init' (c,n,s) in
      ok res
  )
  | E_if_cons (c, n, (((_, _) , (_, _)) , cons)) -> (
      let* res = bind_fold_triple self init' (c,n,cons) in
      ok res
  )
  | E_if_left (c, ((_, _) , l), ((_, _) , r)) -> (
      let* res = bind_fold_triple self init' (c,l,r) in
      ok res
  )
  | E_let_in (expr, _inline , ((_, _) , body)) -> (        
      let* res = bind_fold_pair self init' (expr,body) in
      ok res
  )
  | E_tuple exprs ->
      bind_fold_list self init' exprs
  | E_let_tuple (expr, (_, body)) -> (
      let* res = bind_fold_pair self init' (expr,body) in
      ok res
  )
  | E_proj (expr, _i, _n) ->
      self init' expr
  | E_update (expr, _i, update, _n) ->
      bind_fold_pair self init' (expr, update)

type 'err mapper = expression -> (expression,'err) result

let rec map_expression : 'err mapper -> expression -> (expression, 'err) result = fun f e ->
  let self = map_expression f in
  let* e' = f e in
  let return content = ok { e' with content } in
  match e'.content with
  | E_variable _ | E_literal _ | E_raw_michelson _
    as em -> return em
  | E_constant (c) -> (
      let* lst = bind_map_list self c.arguments in
      return @@ E_constant {cons_name = c.cons_name; arguments = lst}
  )
  | E_closure af -> (
      let* body = self af.body in
      return @@ E_closure { af with body } 
  )
  | E_application farg -> (
      let* farg' = bind_map_pair self farg in 
      return @@ E_application farg'
  )
  | E_iterator (s, ((name , tv) , body) , exp) -> (
      let* (exp',body') = bind_map_pair self (exp,body) in
      return @@ E_iterator (s, ((name , tv) , body') , exp')
  )
  | E_fold (((name , tv) , body) , col , init) -> (
      let* (body',col',init') = bind_map_triple self (body,col,init) in
      return @@ E_fold (((name , tv) , body') , col', init')
  )
  | E_fold_right (((name , tv) , body) , (col,el_ty) , init) -> (
      let* (body',col',init') = bind_map_triple self (body,col,init) in
      return @@ E_fold_right (((name , tv) , body') , (col',el_ty), init')
  )
  | E_if_bool cab -> (
      let* cab' = bind_map_triple self cab in
      return @@ E_if_bool cab'
  )
  | E_if_none (c, n, ((name, tv) , s)) -> (
      let* (c',n',s') = bind_map_triple self (c,n,s) in
      return @@ E_if_none (c', n', ((name, tv) , s'))
  )
  | E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons)) -> (
      let* (c',n',cons') = bind_map_triple self (c,n,cons) in
      return @@ E_if_cons (c', n', (((hd, hdtv) , (tl, tltv)) , cons'))
  )
  | E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r)) -> (
      let* (c',l',r') = bind_map_triple self (c,l,r) in
      return @@ E_if_left (c', ((name_l, tvl) , l'), ((name_r, tvr) , r'))
  )
  | E_let_in (expr , inline , ((v , tv) , body)) -> (
      let* (expr',body') = bind_map_pair self (expr,body) in
      return @@ E_let_in (expr', inline, ((v , tv) , body'))
  )
  | E_tuple exprs ->
      let* exprs = bind_map_list self exprs in
      return @@ E_tuple exprs
  | E_let_tuple (expr, (xs, body)) -> (
      let* (expr', body') = bind_map_pair self (expr, body) in
      return @@ E_let_tuple (expr', (xs, body'))
  )
  | E_proj (expr, i, n) ->
      let* expr = self expr in
      return @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
      let* expr = self expr in
      let* update = self update in
      return @@ E_update (expr, i, update, n)

let map_sub_level_expression : 'err mapper -> expression -> (expression , 'err) result = fun f e ->
  match e.content with
  | E_closure {binder ; body} ->
    let* body = map_expression f body in
    let content = E_closure {binder; body} in
    ok @@ { e with content }
  | _ -> ok e
