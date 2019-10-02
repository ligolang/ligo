open Mini_c
open Trace

type mapper = expression -> expression result
(* fold ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)

let rec map_expression : mapper -> expression -> expression result = fun f e ->
  let self = map_expression f in
  let%bind e' = f e in
  let return content = ok { e' with content } in
  match e'.content with
  | E_variable _ | E_skip | E_make_none _
  | E_make_empty_map (_,_) | E_make_empty_list _ | E_make_empty_set _ as em -> return em
  | E_literal v -> (
      let%bind v' = match v with
      | D_function an ->
        let%bind body = self an.body in
        ok @@ D_function { an with body }
      | _ -> ok v in
      return @@ E_literal v'
  )
  | E_constant (name, lst) -> (
      let%bind lst' = bind_map_list self lst in
      return @@ E_constant (name,lst')
  )
  | E_closure af -> (
      let%bind body = self af.body in
      return @@ E_closure { af with body } 
  )
  | E_application farg -> (
      let%bind farg' = bind_map_pair self farg in 
      return @@ E_application farg'
  )
  | E_iterator (s, ((name , tv) , body) , exp) -> (
      let%bind (exp',body') = bind_map_pair self (exp,body) in
      return @@ E_iterator (s, ((name , tv) , body') , exp')
  )
  | E_fold (((name , tv) , body) , col , init) -> (
      let%bind (body',col',init') = bind_map_triple self (body,col,init) in
      return @@ E_fold (((name , tv) , body') , col', init')
  )
  | E_while eb -> (
      let%bind eb' = bind_map_pair self eb in
      return @@ E_while eb'
  ) 
  | E_if_bool cab -> (
      let%bind cab' = bind_map_triple self cab in
      return @@ E_if_bool cab'
  )
  | E_if_none (c, n, ((name, tv) , s)) -> (
      let%bind (c',n',s') = bind_map_triple self (c,n,s) in
      return @@ E_if_none (c', n', ((name, tv) , s'))
  )
  | E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons)) -> (
      let%bind (c',n',cons') = bind_map_triple self (c,n,cons) in
      return @@ E_if_cons (c', n', (((hd, hdtv) , (tl, tltv)) , cons'))
  )
  | E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r)) -> (
      let%bind (c',l',r') = bind_map_triple self (c,l,r) in
      return @@ E_if_left (c', ((name_l, tvl) , l'), ((name_r, tvr) , r'))
  )
  | E_let_in ((v , tv) , expr , body) -> (
      let%bind (expr',body') = bind_map_pair self (expr,body) in
      return @@ E_let_in ((v , tv) , expr' , body')
  )
  | E_sequence ab -> (
      let%bind ab' = bind_map_pair self ab in
      return @@ E_sequence ab'
  )
  | E_assignment (s, lrl, exp) -> (
      let%bind exp' = self exp in
      return @@ E_assignment (s, lrl, exp')
  )