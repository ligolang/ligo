open Ast_simplified
open Trace

type 'a folder = 'a -> expression -> 'a result
let rec fold_expression : 'a folder -> 'a -> expression -> 'a result = fun f init e ->
  let self = fold_expression f in 
  let%bind init' = f init e in
  match e.expression with
  | E_literal _ | E_variable _ | E_skip -> ok init'
  | E_list lst | E_set lst | E_tuple lst | E_constant (_ , lst) -> (
    let%bind res = bind_fold_list self init' lst in
    ok res
  )
  | E_map lst | E_big_map lst -> (
    let%bind res = bind_fold_list (bind_fold_pair self) init' lst in
    ok res
  )
  | E_look_up ab | E_sequence ab | E_loop ab | E_application ab -> (
      let%bind res = bind_fold_pair self init' ab in
      ok res
    )
  | E_lambda { binder = _ ; input_type = _ ; output_type = _ ; result = e }
  | E_ascription (e , _) | E_constructor (_ , e) -> (
      let%bind res = self init' e in
      ok res
    )
  | E_assign (_ , _path , e) | E_accessor (e , _path) -> (
      let%bind res = self init' e in
      ok res
    )
  | E_matching (e , cases) -> (
      let%bind res = self init' e in
      let%bind res = fold_cases f res cases in
      ok res
    )
  | E_record m -> (
    let aux init'' _ expr =
      let%bind res = fold_expression self init'' expr in
      ok res
    in
    let%bind res = bind_fold_lmap aux (ok init') m in
    ok res
  )
  | E_update {record;updates} -> (
    let%bind res = self init' record in
    let aux res (_, expr) =
      let%bind res = fold_expression self res expr in
      ok res
    in
    let%bind res = bind_fold_list aux res updates in
    ok res 
  )
  | E_let_in { binder = _ ; rhs ; result } -> (
      let%bind res = self init' rhs in
      let%bind res = self res result in
      ok res
    )

and fold_cases : 'a folder -> 'a -> matching_expr -> 'a result = fun f init m ->
  match m with
  | Match_bool { match_true ; match_false } -> (
      let%bind res = fold_expression f init match_true in
      let%bind res = fold_expression f res match_false in
      ok res
    )
  | Match_list { match_nil ; match_cons = (_ , _ , cons, _) } -> (
      let%bind res = fold_expression f init match_nil in
      let%bind res = fold_expression f res cons in
      ok res
    )
  | Match_option { match_none ; match_some = (_ , some, _) } -> (
      let%bind res = fold_expression f init match_none in
      let%bind res = fold_expression f res some in
      ok res
    )
  | Match_tuple ((_ , e), _) -> (
      let%bind res = fold_expression f init e in
      ok res
    )
  | Match_variant (lst, _) -> (
      let aux init' ((_ , _) , e) =
        let%bind res' = fold_expression f init' e in
        ok res' in
      let%bind res = bind_fold_list aux init lst in
      ok res
    )

type mapper = expression -> expression result
let rec map_expression : mapper -> expression -> expression result = fun f e ->
  let self = map_expression f in
  let%bind e' = f e in
  let return expression = ok { e' with expression } in
  match e'.expression with
  | E_list lst -> (
    let%bind lst' = bind_map_list self lst in
    return @@ E_list lst'
  )
  | E_set lst -> (
    let%bind lst' = bind_map_list self lst in
    return @@ E_set lst'
  )
  | E_map lst -> (
    let%bind lst' = bind_map_list (bind_map_pair self) lst in
    return @@ E_map lst'
  )
  | E_big_map lst -> (
    let%bind lst' = bind_map_list (bind_map_pair self) lst in
    return @@ E_big_map lst'
  )
  | E_sequence ab -> (
      let%bind ab' = bind_map_pair self ab in
      return @@ E_sequence ab'
    )
  | E_look_up ab -> (
      let%bind ab' = bind_map_pair self ab in
      return @@ E_look_up ab'
    )
  | E_loop ab -> (
      let%bind ab' = bind_map_pair self ab in
      return @@ E_loop ab'
    )
  | E_ascription (e , t) -> (
      let%bind e' = self e in
      return @@ E_ascription (e' , t)
    )
  | E_assign (name , path , e) -> (
      let%bind e' = self e in
      return @@ E_assign (name , path , e')
    )
  | E_matching (e , cases) -> (
      let%bind e' = self e in
      let%bind cases' = map_cases f cases in
      return @@ E_matching (e' , cases')
    )
  | E_accessor (e , path) -> (
      let%bind e' = self e in
      return @@ E_accessor (e' , path)
    )
  | E_record m -> (
    let%bind m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_update {record; updates} -> (
    let%bind record = self record in
    let%bind updates = bind_map_list (fun(l,e) -> let%bind e = self e in ok (l,e)) updates in
    return @@ E_update {record;updates}
  )
  | E_constructor (name , e) -> (
      let%bind e' = self e in
      return @@ E_constructor (name , e')
    )
  | E_tuple lst -> (
      let%bind lst' = bind_map_list self lst in
      return @@ E_tuple lst'
    )
  | E_application ab -> (
      let%bind ab' = bind_map_pair self ab in
      return @@ E_application ab'
    )
  | E_let_in { binder ; rhs ; result } -> (
      let%bind rhs = self rhs in
      let%bind result = self result in
      return @@ E_let_in { binder ; rhs ; result }
    )
  | E_lambda { binder ; input_type ; output_type ; result } -> (
      let%bind result = self result in
      return @@ E_lambda { binder ; input_type ; output_type ; result }
    )
  | E_constant (name , lst) -> (
      let%bind lst' = bind_map_list self lst in
      return @@ E_constant (name , lst')
    )
  | E_literal _ | E_variable _ | E_skip as e' -> return e'


and map_cases : mapper -> matching_expr -> matching_expr result = fun f m ->
  match m with
  | Match_bool { match_true ; match_false } -> (
      let%bind match_true = map_expression f match_true in
      let%bind match_false = map_expression f match_false in
      ok @@ Match_bool { match_true ; match_false }
    )
  | Match_list { match_nil ; match_cons = (hd , tl , cons, _) } -> (
      let%bind match_nil = map_expression f match_nil in
      let%bind cons = map_expression f cons in
      ok @@ Match_list { match_nil ; match_cons = (hd , tl , cons, ()) }
    )
  | Match_option { match_none ; match_some = (name , some, _) } -> (
      let%bind match_none = map_expression f match_none in
      let%bind some = map_expression f some in
      ok @@ Match_option { match_none ; match_some = (name , some, ()) }
    )
  | Match_tuple ((names , e), _) -> (
      let%bind e' = map_expression f e in
      ok @@ Match_tuple ((names , e'), [])
    )
  | Match_variant (lst, _) -> (
      let aux ((a , b) , e) =
        let%bind e' = map_expression f e in
        ok ((a , b) , e')
      in
      let%bind lst' = bind_map_list aux lst in
      ok @@ Match_variant (lst', ())
    )

and map_program : mapper -> program -> program result = fun m p ->
  let aux = fun (x : declaration) ->
    match x with
    | Declaration_constant (t , o , e) -> (
        let%bind e' = map_expression m e in
        ok (Declaration_constant (t , o , e'))
      )
    | Declaration_type _ -> ok x
  in
  bind_map_list (bind_map_location aux) p
