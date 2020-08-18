open Ast_imperative
open Trace
open Stage_common.Helpers

let bind_map_lmap_t f map = bind_lmap (
  LMap.map 
    (fun ({associated_type;_} as field) -> 
      let%bind associated_type = f associated_type in
      ok {field with associated_type }) 
    map)

type ('a,'err) folder = 'a -> expression -> ('a, 'err) result
let rec fold_expression : ('a, 'err) folder -> 'a -> expression -> ('a, 'err) result = fun f init e ->
  let self = fold_expression f in 
  let%bind init' = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip -> ok init'
  | E_list lst | E_set lst | E_constant {arguments=lst} -> (
    let%bind res = bind_fold_list self init' lst in
    ok res
  )
  | E_map lst | E_big_map lst -> (
    let%bind res = bind_fold_list (bind_fold_pair self) init' lst in
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
  | E_update {record;path;update} -> (
    let%bind res = self init' record in
    let aux res a = match a with
    | Access_map e -> self res e 
    | _ -> ok res
    in
    let%bind res = bind_fold_list aux res path in
    let%bind res = fold_expression self res update in
    ok res 
  )
  | E_accessor {record;path} -> (
    let%bind res = self init' record in
    let aux res a = match a with
    | Access_map e -> self res e 
    | _ -> ok res
    in
    let%bind res = bind_fold_list aux res path in
    ok res
  )
  | E_tuple t -> (
    let aux init'' expr =
      let%bind res = fold_expression self init'' expr in
      ok res
    in
    let%bind res = bind_fold_list aux (init') t in
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
  | E_cond {condition; then_clause; else_clause} ->
      let%bind res = self init' condition in
      let%bind res = self res then_clause in
      let%bind res = self res else_clause in
      ok res
  | E_sequence {expr1;expr2} ->
      let ab = (expr1,expr2) in
      let%bind res = bind_fold_pair self init' ab in
      ok res
  | E_assign {variable=_;access_path;expression} ->
      let aux res a = match a with
      | Access_map e -> self res e 
      | _ -> ok res
      in
      let%bind res = bind_fold_list aux init' access_path in
      let%bind res = self res expression in
      ok res
  | E_for {body; _} ->
      let%bind res = self init' body in
      ok res
  | E_for_each {collection; body; _} ->
      let%bind res = self init' collection in
      let%bind res = self res body in
      ok res
  | E_while {condition; body} ->
      let%bind res = self init' condition in
      let%bind res = self res body in
      ok res  

and fold_cases : ('a , 'b) folder -> 'a -> matching_expr -> ('a , 'b) result = fun f init m ->
  match m with
  | Match_variant lst -> (
      let aux init' ((_ , _) , e) =
        let%bind res' = fold_expression f init' e in
        ok res' in
      let%bind res = bind_fold_list aux init lst in
      ok res
    )
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
  | Match_record (_, e) -> (
      let%bind res = fold_expression f init e in
      ok res
    )
  | Match_tuple (_, e) -> (
      let%bind res = fold_expression f init e in
      ok res
    )
  | Match_variable (_, e) -> (
      let%bind res = fold_expression f init e in
      ok res
    )

type 'err exp_mapper = expression -> (expression , 'err) result
type 'err ty_exp_mapper = type_expression -> (type_expression, 'err) result
type 'err abs_mapper =
  | Expression of 'err exp_mapper
  | Type_expression of 'err ty_exp_mapper 
let rec map_expression : 'err exp_mapper -> expression -> (expression, 'err) result = fun f e ->
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
  | E_ascription ascr -> (
      let%bind e' = self ascr.anno_expr in
      return @@ E_ascription {ascr with anno_expr=e'}
    )
  | E_matching {matchee=e;cases} -> (
      let%bind e' = self e in
      let%bind cases' = map_cases f cases in
      return @@ E_matching {matchee=e';cases=cases'}
    )
  | E_record m -> (
    let%bind m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_accessor {record; path} -> (
    let%bind record = self record in
    let aux a = match a with
    | Access_map e -> 
      let%bind e = self e in
      ok @@ Access_map e
    | e -> ok @@ e
    in
    let%bind path = bind_map_list aux path in
    return @@ E_accessor {record; path}
  )
  | E_update {record; path; update} -> (
    let%bind record = self record in
    let aux a = match a with
    | Access_map e -> 
      let%bind e = self e in
      ok @@ Access_map e
    | e -> ok @@ e
    in
    let%bind path = bind_map_list aux path in
    let%bind update = self update in
    return @@ E_update {record;path;update}
  )
  | E_tuple t -> (
    let%bind t' = bind_map_list self t in
    return @@ E_tuple t'
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
  | E_cond {condition; then_clause; else_clause} ->
      let%bind condition   = self condition in
      let%bind then_clause = self then_clause in
      let%bind else_clause = self else_clause in
      return @@ E_cond {condition;then_clause;else_clause}
  | E_sequence {expr1;expr2} -> (
      let%bind (expr1,expr2) = bind_map_pair self (expr1,expr2) in
      return @@ E_sequence {expr1;expr2}
    )
  | E_assign {variable;access_path;expression} -> (
      let aux a = match a with
      | Access_map e -> 
        let%bind e = self e in
        ok @@ Access_map e
      | e -> ok @@ e
      in
      let%bind access_path = bind_map_list aux access_path in
      let%bind expression = self expression in
      return @@ E_assign {variable;access_path;expression}
  )
  | E_for {binder; start; final; increment; body} ->
      let%bind body = self body in
      return @@ E_for {binder; start; final; increment; body}
  | E_for_each {binder; collection; collection_type; body} ->
      let%bind collection = self collection in
      let%bind body = self body in
      return @@ E_for_each {binder; collection; collection_type; body}
  | E_while {condition; body} ->
      let%bind condition = self condition in
      let%bind body = self body in
      return @@ E_while {condition; body}

  | E_literal _ | E_variable _ | E_raw_code _ | E_skip as e' -> return e'

and map_type_expression : 'err ty_exp_mapper -> type_expression -> (type_expression , _) result = fun f te ->
  let self = map_type_expression f in
  let%bind te' = f te in
  let return type_content = ok { type_content; location=te.location } in
  match te'.type_content with
  | T_sum temap ->
    let%bind temap' = bind_map_lmap_t self temap in
    return @@ (T_sum temap')
  | T_record temap ->
    let%bind temap' = bind_map_lmap_t self temap in
    return @@ (T_record temap')
  | T_tuple telst ->
    let%bind telst' = bind_map_list self telst in
    return @@ (T_tuple telst')
  | T_arrow {type1 ; type2} ->
    let%bind type1' = self type1 in
    let%bind type2' = self type2 in
    return @@ (T_arrow {type1=type1' ; type2=type2'})
  | T_annoted (ty, str) -> 
    let%bind ty = self ty in
    return @@ T_annoted (ty, str)
  | T_variable _ | T_wildcard | T_constant _ -> ok te'

and map_cases : 'err exp_mapper -> matching_expr -> (matching_expr , _) result = fun f m ->
  match m with
  | Match_variant lst -> (
      let aux ((a , b) , e) =
        let%bind e' = map_expression f e in
        ok ((a , b) , e')
      in
      let%bind lst' = bind_map_list aux lst in
      ok @@ Match_variant lst'
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
  | Match_record (binder, e) -> (
      let%bind e' = map_expression f e in
      ok @@ Match_record (binder, e')
    )
  | Match_tuple (binder, e) -> (
      let%bind e' = map_expression f e in
      ok @@ Match_tuple (binder, e')
    )
  | Match_variable (binder, e) -> (
      let%bind e' = map_expression f e in
      ok @@ Match_variable (binder, e')
    )

and map_program : 'err abs_mapper -> program -> (program , _) result = fun m p ->
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

type ('a, 'err) fold_mapper = 'a -> expression -> ((bool * 'a * expression), 'err) result
let rec fold_map_expression : ('a, 'err) fold_mapper -> 'a -> expression -> ('a * expression , 'err) result = fun f a e ->
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
  | E_ascription ascr -> (
      let%bind (res,e') = self init' ascr.anno_expr in
      ok (res, return @@ E_ascription {ascr with anno_expr=e'})
    )
  | E_matching {matchee=e;cases} -> (
      let%bind (res, e') = self init' e in
      let%bind (res,cases') = fold_map_cases f res cases in
      ok (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record m -> (
    let%bind (res, lst') = bind_fold_map_list (fun res (k,e) -> let%bind (res,e) = self res e in ok (res,(k,e))) init' (LMap.to_kv_list m) in
    let m' = LMap.of_list lst' in
    ok (res, return @@ E_record m')
  )
  | E_accessor {record;path} -> (
    let%bind (res, record) = self init' record in
    let aux res a = match a with
    | Access_map e -> 
      let%bind (res,e) = self res e in
      ok @@ (res,Access_map e)
    | e -> ok @@ (res,e)
    in
    let%bind (res, path)   = bind_fold_map_list aux res path in
    ok (res, return @@ E_accessor {record; path})
  )
  | E_update {record; path; update} -> (
    let%bind (res, record) = self init' record in
    let aux res a = match a with
    | Access_map e -> 
      let%bind (res,e) = self res e in
      ok @@ (res,Access_map e)
    | e -> ok @@ (res,e)
    in
    let%bind (res, path)   = bind_fold_map_list aux res path in
    let%bind (res, update) = self res update in
    ok (res, return @@ E_update {record;path;update})
  )
  | E_tuple t -> (
    let%bind (res, t') = bind_fold_map_list self init' t in
    ok (res, return @@ E_tuple t')
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
  | E_cond {condition; then_clause; else_clause} ->
      let%bind res,condition   = self init' condition in
      let%bind res,then_clause = self res then_clause in
      let%bind res,else_clause = self res else_clause in
      ok (res, return @@ E_cond {condition;then_clause;else_clause})
  | E_sequence {expr1;expr2} -> (
      let%bind (res,(expr1,expr2)) = bind_fold_map_pair self init' (expr1,expr2) in
      ok (res, return @@ E_sequence {expr1;expr2})
    )
  | E_assign {variable;access_path;expression} ->
      let aux res a = match a with
      | Access_map e -> 
        let%bind (res,e) = self res e in
        ok @@ (res,Access_map e)
      | e -> ok @@ (res,e)
      in
      let%bind (res, access_path)   = bind_fold_map_list aux init' access_path in
      let%bind (res, expression) = self res expression in
      ok (res, return @@ E_assign {variable;access_path;expression})
  | E_for {binder; start; final; increment; body} ->
      let%bind (res, body) = self init' body in
      ok (res, return @@ E_for {binder; start; final; increment; body})
  | E_for_each {binder; collection; collection_type; body} ->
      let%bind res,collection = self init' collection in
      let%bind res,body = self res body in
      ok (res, return @@ E_for_each {binder; collection; collection_type; body})
  | E_while {condition; body} ->
      let%bind res,condition = self init' condition in
      let%bind res,body = self res body in
      ok (res, return @@ E_while {condition; body})
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip as e' -> ok (init', return e')

and fold_map_cases : ('a , 'err) fold_mapper -> 'a -> matching_expr -> ('a * matching_expr , 'err) result = fun f init m ->
  match m with
  | Match_variant lst -> (
      let aux init ((a , b) , e) =
        let%bind (init,e') = fold_map_expression f init e in
        ok (init, ((a , b) , e'))
      in
      let%bind (init,lst') = bind_fold_map_list aux init lst in
      ok @@ (init, Match_variant lst')
    )
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
  | Match_record (binder, e) -> (
      let%bind (init, e') = fold_map_expression f init e in
      ok @@ (init, Match_record (binder, e'))
    )
  | Match_tuple (binder, e) -> (
      let%bind (init, e') = fold_map_expression f init e in
      ok @@ (init, Match_tuple (binder, e'))
    )
  | Match_variable (binder, e) -> (
      let%bind (init, e') = fold_map_expression f init e in
      ok @@ (init, Match_variable (binder, e'))
    )
