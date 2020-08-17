open Ast_core
open Trace
open Ast_core.Helpers

include Ast_core.PP

let bind_map_lmap_t f map = bind_lmap (
  LMap.map 
    (fun ({associated_type;_} as field) -> 
      let%bind field' = f associated_type in
      ok {field with associated_type = field'}) 
    map)

type ('a,'err) folder = 'a -> expression -> ('a, 'err) result
let rec fold_expression : ('a, 'err) folder -> 'a -> expression -> ('a,'err) result = fun f init e ->
  let self = fold_expression f in 
  let%bind init' = f init e in
  match e.content with
  | E_literal _ | E_variable _ | E_raw_code _ -> ok init'
  | E_constant {arguments=lst} -> (
    let%bind res = bind_fold_list self init' lst in
    ok res
  )
  | E_application {lamb;args} -> (
      let ab = (lamb,args) in
      let%bind res = bind_fold_pair self init' ab in
      ok res
    )
  | E_lambda { binder = _ ; result = e }
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
    let%bind res = bind_fold_lmap aux init' m in
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

and fold_cases : ('a , 'err) folder -> 'a -> matching_expr -> ('a , 'err) result = fun f init m ->
  match m with
  | Match_list { match_nil ; match_cons = { body ; _ } } -> (
      let%bind res = fold_expression f init match_nil in
      let%bind res = fold_expression f res body in
      ok res
    )
  | Match_option { match_none ; match_some = { body ; _ } } -> (
      let%bind res = fold_expression f init match_none in
      let%bind res = fold_expression f res body in
      ok res
    )
  | Match_variant lst -> (
      let aux init' ({ body ; _ } : match_variant) =
        let%bind res' = fold_expression f init' body in
        ok res' in
      let%bind res = bind_fold_list aux init lst in
      ok res
    )

type 'err exp_mapper = expression -> (expression , 'err) result
type 'err ty_exp_mapper = type_expression -> (type_expression , 'err) result
type 'err abs_mapper =
  | Expression of 'err exp_mapper
  | Type_expression of 'err ty_exp_mapper 
let rec map_expression : 'err exp_mapper -> expression -> (expression , 'err) result = fun f e ->
  let self = map_expression f in
  let%bind e' = f e in
  let return content = ok { e' with content } in
  match e'.content with
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
  | E_lambda { binder ; result } -> (
      let%bind result = self result in
      return @@ E_lambda { binder ; result }
    )
  | E_recursive { fun_name; fun_type; lambda} ->
      let%bind result = self lambda.result in
      let lambda = {lambda with result} in
      return @@ E_recursive { fun_name; fun_type; lambda}
  | E_constant c -> (
      let%bind args = bind_map_list self c.arguments in
      return @@ E_constant {c with arguments=args}
    )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'

and map_type_expression : 'err ty_exp_mapper -> type_expression -> (type_expression , 'err) result =
    fun f ({content ; sugar; location } as te) ->
  let self = map_type_expression f in
  let%bind te' = f te in
  let return content = ok @@ ({ content; sugar; location}: type_expression) in
  match content with
  | T_sum temap ->
    let%bind temap' = bind_map_lmap_t self temap in
    return @@ (T_sum temap')
  | T_record temap ->
    let%bind temap' = bind_map_lmap_t self temap in
    return @@ (T_record temap')
  | T_arrow {type1 ; type2} ->
    let%bind type1' = self type1 in
    let%bind type2' = self type2 in
    return @@ (T_arrow {type1=type1' ; type2=type2'})
  | T_variable _ | T_wildcard  | T_constant _ -> ok te'

and map_cases : 'err exp_mapper -> matching_expr -> (matching_expr , 'err) result = fun f m ->
  match m with
  | Match_list { match_nil ; match_cons = {hd ; tl ; body} } -> (
      let%bind match_nil = map_expression f match_nil in
      let%bind body = map_expression f body in
      ok @@ Match_list { match_nil ; match_cons = {hd ; tl ; body} }
    )
  | Match_option { match_none ; match_some = {opt ; body} } -> (
      let%bind match_none = map_expression f match_none in
      let%bind body = map_expression f body in
      ok @@ Match_option { match_none ; match_some = { opt ; body } }
    )
  | Match_variant lst -> (
      let aux (match_variant:match_variant) =
        let%bind body = map_expression f match_variant.body in
        ok { match_variant with body }
      in
      let%bind lst' = bind_map_list aux lst in
      ok @@ Match_variant lst'
    )

and map_program : 'err abs_mapper -> program -> (program , 'err) result = fun m p ->
  let aux = fun (x : declaration) ->
    match x,m with
    | (Declaration_constant decl_cst, Expression m') -> (
        let%bind expr = map_expression m' decl_cst.expr in
        ok (Declaration_constant {decl_cst with expr})
      )
    | (Declaration_type decl_ty, Type_expression m') -> (
        let%bind type_expr = map_type_expression m' decl_ty.type_expr in
        ok (Declaration_type {decl_ty with type_expr})
      )
    | decl,_ -> ok decl
  (* | Declaration_type of (type_variable * type_expression) *)
  in
  bind_map_list (bind_map_location aux) p

type ('a , 'err) fold_mapper = 'a -> expression -> (bool * 'a * expression , 'err) result
let rec fold_map_expression : ('a , 'err) fold_mapper -> 'a -> expression -> ('a * expression , 'err) result = fun f a e ->
  let self = fold_map_expression f in
  let%bind (continue, init',e') = f a e in
  if (not continue) then ok(init',e')
  else
  let return content = { e' with content } in
  match e'.content with
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
  | E_lambda { binder ; result } -> (
      let%bind (res,result) = self init' result in
      ok ( res, return @@ E_lambda { binder ; result })
    )
  | E_recursive { fun_name; fun_type; lambda} ->
      let%bind (res, result) = self init' lambda.result in
      let lambda = {lambda with result} in
      ok ( res, return @@ E_recursive { fun_name; fun_type; lambda})
  | E_constant c -> (
      let%bind (res,args) = bind_fold_map_list self init' c.arguments in
      ok (res, return @@ E_constant {c with arguments=args})
    )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> ok (init', return e')

and fold_map_cases : ('a , 'err) fold_mapper -> 'a -> matching_expr -> ('a * matching_expr , 'err) result =
    fun f init m ->
  match m with
  | Match_list { match_nil ; match_cons = {hd ; tl ; body} } -> (
      let%bind (init, match_nil) = fold_map_expression f init match_nil in
      let%bind (init, body ) = fold_map_expression f init body in
      ok @@ (init, Match_list { match_nil ; match_cons = { hd ; tl ; body } })
    )
  | Match_option { match_none ; match_some = {opt ; body} } -> (
      let%bind (init, match_none) = fold_map_expression f init match_none in
      let%bind (init, body) = fold_map_expression f init body in
      ok @@ (init, Match_option { match_none ; match_some = {opt ; body} })
    )
  | Match_variant lst -> (
      let aux init (match_variant:match_variant) =
        let%bind (init,body) = fold_map_expression f init match_variant.body in
        ok (init, {match_variant with body})
      in
      let%bind (init,lst') = bind_fold_map_list aux init lst in
      ok @@ (init, Match_variant lst')
    )  
