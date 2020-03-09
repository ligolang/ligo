open Ast_simplified
open Trace
open Stage_common.Helpers

type 'a folder = 'a -> expression -> 'a result
let rec fold_expression : 'a folder -> 'a -> expression -> 'a result = fun f init e ->
  let self = fold_expression f in 
  let%bind init' = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_skip -> ok init'
  | E_list lst | E_set lst | E_constant {arguments=lst} -> (
    let%bind res = bind_fold_list self init' lst in
    ok res
  )
  | E_map lst | E_big_map lst -> (
    let%bind res = bind_fold_list (bind_fold_pair self) init' lst in
    ok res
  )
  | E_look_up ab ->
      let%bind res = bind_fold_pair self init' ab in
      ok res
  | E_application {expr1;expr2} -> (
      let ab = (expr1,expr2) in
      let%bind res = bind_fold_pair self init' ab in
      ok res
    )
  | E_lambda { binder = _ ; input_type = _ ; output_type = _ ; result = e }
  | E_ascription {anno_expr=e; _} | E_constructor {element=e} -> (
      let%bind res = self init' e in
      ok res
    )
  | E_matching {matchee=e; cases} -> (
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
  | E_record_update {record;update} -> (
    let%bind res = self init' record in
    let%bind res = fold_expression self res update in
    ok res 
  )
  | E_record_accessor {expr} -> (
     let%bind res = self init' expr in
     ok res
    )
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let%bind res = self init' rhs in
      let%bind res = self res let_result in
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
  let return expression_content = ok { e' with expression_content } in
  match e'.expression_content with
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
  | E_look_up ab -> (
      let%bind ab' = bind_map_pair self ab in
      return @@ E_look_up ab'
    )
  | E_ascription ascr -> (
      let%bind e' = self ascr.anno_expr in
      return @@ E_ascription {ascr with anno_expr=e'}
    )
  | E_matching {matchee=e;cases} -> (
      let%bind e' = self e in
      let%bind cases' = map_cases f cases in
      return @@ E_matching {matchee=e';cases=cases'}
    )
  | E_record_accessor acc -> (
      let%bind e' = self acc.expr in
      return @@ E_record_accessor {acc with expr = e'}
    )
  | E_record m -> (
    let%bind m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_record_update {record; path; update} -> (
    let%bind record = self record in
    let%bind update = self update in
    return @@ E_record_update {record;path;update}
  )
  | E_constructor c -> (
      let%bind e' = self c.element in
      return @@ E_constructor {c with element = e'}
  )
  | E_application {expr1;expr2} -> (
      let ab = (expr1,expr2) in
      let%bind (a,b) = bind_map_pair self ab in
      return @@ E_application {expr1=a;expr2=b}
    )
  | E_let_in { let_binder ; mut; rhs ; let_result; inline } -> (
      let%bind rhs = self rhs in
      let%bind let_result = self let_result in
      return @@ E_let_in { let_binder ; mut; rhs ; let_result; inline }
    )
  | E_lambda { binder ; input_type ; output_type ; result } -> (
      let%bind result = self result in
      return @@ E_lambda { binder ; input_type ; output_type ; result }
    )
  | E_constant c -> (
      let%bind args = bind_map_list self c.arguments in
      return @@ E_constant {c with arguments=args}
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
    | Declaration_constant (t , o , i, e) -> (
        let%bind e' = map_expression m e in
        ok (Declaration_constant (t , o , i, e'))
      )
    | Declaration_type _ -> ok x
  in
  bind_map_list (bind_map_location aux) p

type 'a fold_mapper = 'a -> expression -> (bool * 'a * expression) result
let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> ('a * expression) result = fun f a e ->
  let self = fold_map_expression f in
  let%bind (continue, init',e') = f a e in
  if (not continue) then ok(init',e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let%bind (res, lst') = bind_fold_map_list self init' lst in
    ok (res, return @@ E_list lst')
  )
  | E_set lst -> (
    let%bind (res, lst') = bind_fold_map_list self init' lst in
    ok (res, return @@ E_set lst')
  )
  | E_map lst -> (
    let%bind (res, lst') = bind_fold_map_list (bind_fold_map_pair self) init' lst in
    ok (res, return @@ E_map lst')
  )
  | E_big_map lst -> (
    let%bind (res, lst') = bind_fold_map_list (bind_fold_map_pair self) init' lst in
    ok (res, return @@ E_big_map lst')
  )
  | E_look_up ab -> (
      let%bind (res, ab') = bind_fold_map_pair self init' ab in
      ok (res, return @@ E_look_up ab')
    )
  | E_ascription ascr -> (
      let%bind (res,e') = self init' ascr.anno_expr in
      ok (res, return @@ E_ascription {ascr with anno_expr=e'})
    )
  | E_matching {matchee=e;cases} -> (
      let%bind (res, e') = self init' e in
      let%bind (res,cases') = fold_map_cases f res cases in
      ok (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record_accessor acc -> (
      let%bind (res, e') = self init' acc.expr in
      ok (res, return @@ E_record_accessor {acc with expr = e'})
    )
  | E_record m -> (
    let%bind (res, lst') = bind_fold_map_list (fun res (k,e) -> let%bind (res,e) = self res e in ok (res,(k,e))) init' (LMap.to_kv_list m) in
    let m' = LMap.of_list lst' in
    ok (res, return @@ E_record m')
  )
  | E_record_update {record; path; update} -> (
    let%bind (res, record) = self init' record in
    let%bind (res, update) = self res update in
    ok (res, return @@ E_record_update {record;path;update})
  )
  | E_constructor c -> (
      let%bind (res,e') = self init' c.element in
      ok (res, return @@ E_constructor {c with element = e'})
  )
  | E_application {expr1;expr2} -> (
      let ab = (expr1,expr2) in
      let%bind (res,(a,b)) = bind_fold_map_pair self init' ab in
      ok (res, return @@ E_application {expr1=a;expr2=b})
    )
  | E_let_in { let_binder ; mut; rhs ; let_result; inline } -> (
      let%bind (res,rhs) = self init' rhs in
      let%bind (res,let_result) = self res let_result in
      ok (res, return @@ E_let_in { let_binder ; mut; rhs ; let_result ; inline })
    )
  | E_lambda { binder ; input_type ; output_type ; result } -> (
      let%bind (res,result) = self init' result in
      ok ( res, return @@ E_lambda { binder ; input_type ; output_type ; result })
    )
  | E_constant c -> (
      let%bind (res,args) = bind_fold_map_list self init' c.arguments in
      ok (res, return @@ E_constant {c with arguments=args})
    )
  | E_literal _ | E_variable _ | E_skip as e' -> ok (init', return e')

and fold_map_cases : 'a fold_mapper -> 'a -> matching_expr -> ('a * matching_expr) result = fun f init m ->
  match m with
  | Match_bool { match_true ; match_false } -> (
      let%bind (init, match_true) = fold_map_expression f init match_true in
      let%bind (init, match_false) = fold_map_expression f init match_false in
      ok @@ (init, Match_bool { match_true ; match_false })
    )
  | Match_list { match_nil ; match_cons = (hd , tl , cons, _) } -> (
      let%bind (init, match_nil) = fold_map_expression f init match_nil in
      let%bind (init, cons) = fold_map_expression f init cons in
      ok @@ (init, Match_list { match_nil ; match_cons = (hd , tl , cons, ()) })
    )
  | Match_option { match_none ; match_some = (name , some, _) } -> (
      let%bind (init, match_none) = fold_map_expression f init match_none in
      let%bind (init, some) = fold_map_expression f init some in
      ok @@ (init, Match_option { match_none ; match_some = (name , some, ()) })
    )
  | Match_tuple ((names , e), _) -> (
      let%bind (init, e') = fold_map_expression f init e in
      ok @@ (init, Match_tuple ((names , e'), []))
    )
  | Match_variant (lst, _) -> (
      let aux init ((a , b) , e) =
        let%bind (init,e') = fold_map_expression f init e in
        ok (init, ((a , b) , e'))
      in
      let%bind (init,lst') = bind_fold_map_list aux init lst in
      ok @@ (init, Match_variant (lst', ()))
    )  
