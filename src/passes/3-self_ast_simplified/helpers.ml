open Ast_simplified
open Trace

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
  | E_annotation (e , t) -> (
      let%bind e' = self e in
      return @@ E_annotation (e' , t)
    )
  | E_assign (name , path , e) -> (
      let%bind e' = self e in
      let%bind path' = map_path f path in
      return @@ E_assign (name , path' , e')
    )
  | E_failwith e -> (
      let%bind e' = self e in
      return @@ E_failwith e'
    )
  | E_matching (e , cases) -> (
      let%bind e' = self e in
      let%bind cases' = map_cases f cases in
      return @@ E_matching (e' , cases')
    )
  | E_accessor (e , path) -> (
      let%bind e' = self e in
      let%bind path' = map_path f path in
      return @@ E_accessor (e' , path')
    )
  | E_record m -> (
    let%bind m' = bind_map_smap self m in
    return @@ E_record m'
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

and map_path : mapper -> access_path -> access_path result = fun f p -> bind_map_list (map_access f) p

and map_access : mapper -> access -> access result = fun f a ->
  match a with
  | Access_map e -> (
      let%bind e' = map_expression f e in
      ok @@ Access_map e'
    )
  | a -> ok a

and map_cases : mapper -> matching_expr -> matching_expr result = fun f m ->
  match m with
  | Match_bool { match_true ; match_false } -> (
      let%bind match_true = map_expression f match_true in
      let%bind match_false = map_expression f match_false in
      ok @@ Match_bool { match_true ; match_false }
    )
  | Match_list { match_nil ; match_cons = (hd , tl , cons) } -> (
      let%bind match_nil = map_expression f match_nil in
      let%bind cons = map_expression f cons in
      ok @@ Match_list { match_nil ; match_cons = (hd , tl , cons) }
    )
  | Match_option { match_none ; match_some = (name , some) } -> (
      let%bind match_none = map_expression f match_none in
      let%bind some = map_expression f some in
      ok @@ Match_option { match_none ; match_some = (name , some) }
    )
  | Match_tuple (names , e) -> (
      let%bind e' = map_expression f e in
      ok @@ Match_tuple (names , e')
    )
  | Match_variant lst -> (
      let aux ((a , b) , e) =
        let%bind e' = map_expression f e in
        ok ((a , b) , e')
      in
      let%bind lst' = bind_map_list aux lst in
      ok @@ Match_variant lst'
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
