open Ast_core
open Trace
open Stage_common.Helpers

include Stage_common.PP
include Stage_common.Types.Ast_generic_type(Ast_core_parameter)

let bind_map_cmap f map = bind_cmap (
  CMap.map 
    (fun ({ctor_type;_} as ctor) -> 
      let%bind ctor' = f ctor_type in
      ok {ctor with ctor_type = ctor'}) 
    map)

let bind_map_lmap_t f map = bind_lmap (
  LMap.map 
    (fun ({field_type;_} as field) -> 
      let%bind field' = f field_type in
      ok {field with field_type = field'}) 
    map)

type 'a folder = 'a -> expression -> 'a result
let rec fold_expression : 'a folder -> 'a -> expression -> 'a result = fun f init e ->
  let self = fold_expression f in 
  let%bind init' = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ -> ok init'
  | E_constant {arguments=lst} -> (
    let%bind res = bind_fold_list self init' lst in
    ok res
  )
  | E_application {lamb;args} -> (
      let ab = (lamb,args) in
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
  | E_record_accessor {record} -> (
     let%bind res = self init' record in
     ok res
    )
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let%bind res = self init' rhs in
      let%bind res = self res let_result in
      ok res
    )
  | E_recursive { lambda={result=e;_}; _} ->
      let%bind res = self init' e in
      ok res

and fold_cases : 'a folder -> 'a -> matching_expr -> 'a result = fun f init m ->
  match m with
  | Match_list { match_nil ; match_cons = (_ , _ , cons) } -> (
      let%bind res = fold_expression f init match_nil in
      let%bind res = fold_expression f res cons in
      ok res
    )
  | Match_option { match_none ; match_some = (_ , some) } -> (
      let%bind res = fold_expression f init match_none in
      let%bind res = fold_expression f res some in
      ok res
    )
  | Match_variant lst -> (
      let aux init' ((_ , _) , e) =
        let%bind res' = fold_expression f init' e in
        ok res' in
      let%bind res = bind_fold_list aux init lst in
      ok res
    )

type exp_mapper = expression -> expression result
type ty_exp_mapper = type_expression -> type_expression result
type abs_mapper =
  | Expression of exp_mapper
  | Type_expression of ty_exp_mapper 
let rec map_expression : exp_mapper -> expression -> expression result = fun f e ->
  let self = map_expression f in
  let%bind e' = f e in
  let return expression_content = ok { e' with expression_content } in
  match e'.expression_content with
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
      let%bind e' = self acc.record in
      return @@ E_record_accessor {acc with record = e'}
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
  | E_application {lamb;args} -> (
      let ab = (lamb,args) in
      let%bind (lamb,args) = bind_map_pair self ab in
      return @@ E_application {lamb;args}
    )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let%bind rhs = self rhs in
      let%bind let_result = self let_result in
      return @@ E_let_in { let_binder ; rhs ; let_result; inline }
    )
  | E_lambda { binder ; input_type ; output_type ; result } -> (
      let%bind result = self result in
      return @@ E_lambda { binder ; input_type ; output_type ; result }
    )
  | E_recursive { fun_name; fun_type; lambda} ->
      let%bind result = self lambda.result in
      let lambda = {lambda with result} in
      return @@ E_recursive { fun_name; fun_type; lambda}
  | E_constant c -> (
      let%bind args = bind_map_list self c.arguments in
      return @@ E_constant {c with arguments=args}
    )
  | E_literal _ | E_variable _ as e' -> return e'

and map_type_expression : ty_exp_mapper -> type_expression -> type_expression result = fun f ({type_content ; location ; type_meta} as te) ->
  let self = map_type_expression f in
  let%bind te' = f te in
  let return type_content = ok { type_content; location ; type_meta } in
  match type_content with
  | T_sum temap ->
    let%bind temap' = bind_map_cmap self temap in
    return @@ (T_sum temap')
  | T_record temap ->
    let%bind temap' = bind_map_lmap_t self temap in
    return @@ (T_record temap')
  | T_arrow {type1 ; type2} ->
    let%bind type1' = self type1 in
    let%bind type2' = self type2 in
    return @@ (T_arrow {type1=type1' ; type2=type2'})
  | T_operator _
  | T_variable _ | T_constant _ -> ok te'

and map_cases : exp_mapper -> matching_expr -> matching_expr result = fun f m ->
  match m with
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
  | Match_variant lst -> (
      let aux ((a , b) , e) =
        let%bind e' = map_expression f e in
        ok ((a , b) , e')
      in
      let%bind lst' = bind_map_list aux lst in
      ok @@ Match_variant lst'
    )

and map_program : abs_mapper -> program -> program result = fun m p ->
  let aux = fun (x : declaration) ->
    match x,m with
    | (Declaration_constant (t , o , i, e), Expression m') -> (
        let%bind e' = map_expression m' e in
        ok (Declaration_constant (t , o , i, e'))
      )
    | (Declaration_type (tv,te), Type_expression m') -> (
        let%bind te' = map_type_expression m' te in
        ok (Declaration_type (tv, te'))
      )
    | decl,_ -> ok decl
  (* | Declaration_type of (type_variable * type_expression) *)
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
      let%bind (res, e') = self init' acc.record in
      ok (res, return @@ E_record_accessor {acc with record = e'})
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
  | E_application {lamb;args} -> (
      let ab = (lamb,args) in
      let%bind (res,(a,b)) = bind_fold_map_pair self init' ab in
      ok (res, return @@ E_application {lamb=a;args=b})
    )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let%bind (res,rhs) = self init' rhs in
      let%bind (res,let_result) = self res let_result in
      ok (res, return @@ E_let_in { let_binder ; rhs ; let_result ; inline })
    )
  | E_lambda { binder ; input_type ; output_type ; result } -> (
      let%bind (res,result) = self init' result in
      ok ( res, return @@ E_lambda { binder ; input_type ; output_type ; result })
    )
  | E_recursive { fun_name; fun_type; lambda} ->
      let%bind (res, result) = self init' lambda.result in
      let lambda = {lambda with result} in
      ok ( res, return @@ E_recursive { fun_name; fun_type; lambda})
  | E_constant c -> (
      let%bind (res,args) = bind_fold_map_list self init' c.arguments in
      ok (res, return @@ E_constant {c with arguments=args})
    )
  | E_literal _ | E_variable _ as e' -> ok (init', return e')

and fold_map_cases : 'a fold_mapper -> 'a -> matching_expr -> ('a * matching_expr) result = fun f init m ->
  match m with
  | Match_list { match_nil ; match_cons = (hd , tl , cons) } -> (
      let%bind (init, match_nil) = fold_map_expression f init match_nil in
      let%bind (init, cons) = fold_map_expression f init cons in
      ok @@ (init, Match_list { match_nil ; match_cons = (hd , tl , cons) })
    )
  | Match_option { match_none ; match_some = (name , some) } -> (
      let%bind (init, match_none) = fold_map_expression f init match_none in
      let%bind (init, some) = fold_map_expression f init some in
      ok @@ (init, Match_option { match_none ; match_some = (name , some) })
    )
  | Match_variant lst -> (
      let aux init ((a , b) , e) =
        let%bind (init,e') = fold_map_expression f init e in
        ok (init, ((a , b) , e'))
      in
      let%bind (init,lst') = bind_fold_map_list aux init lst in
      ok @@ (init, Match_variant lst')
    )  
