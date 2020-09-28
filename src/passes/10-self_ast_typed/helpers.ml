open Errors
open Ast_typed
open Trace
open Ast_typed.Helpers

type ('a ,'err) folder = 'a -> expression -> ('a , 'err) result
let rec fold_expression : ('a , self_ast_typed_error) folder -> 'a -> expression -> ('a , self_ast_typed_error) result = fun f init e ->
  let self = fold_expression f in 
  let%bind init' = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ -> ok init'
  | E_constant {arguments=lst} -> (
    let%bind res = bind_fold_list self init' lst in
    ok res
  )
  | E_application {lamb; args} -> (
      let ab = (lamb, args) in
      let%bind res = bind_fold_pair self init' ab in
      ok res
    )
  | E_lambda { binder = _ ; result = e }
  | E_recursive {lambda= {result=e}}
  | E_constructor {element=e} -> (
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

and fold_cases : ('a , 'err) folder -> 'a -> matching_expr -> ('a , 'err) result = fun f init m ->
  match m with
  | Match_list { match_nil ; match_cons = {hd=_; tl=_ ; body; tv=_} } -> (
      let%bind res = fold_expression f init match_nil in
      let%bind res = fold_expression f res body in
      ok res
    )
  | Match_option { match_none ; match_some = {opt=_; body; tv=_} } -> (
      let%bind res = fold_expression f init match_none in
      let%bind res = fold_expression f res body in
      ok res
    )
  | Match_variant {cases;tv=_} -> (
      let aux init' {constructor=_; pattern=_ ; body} =
        let%bind res' = fold_expression f init' body in
        ok res' in
      let%bind res = bind_fold_list aux init cases in
      ok res
    )

type 'err mapper = expression -> (expression , 'err) result
let rec map_expression : self_ast_typed_error mapper -> expression -> (expression , self_ast_typed_error) result = fun f e ->
  let self = map_expression f in
  let%bind e' = f e in
  let return expression_content = ok { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
      let%bind e' = self e in
      let%bind cases' = map_cases f cases in
      return @@ E_matching {matchee=e';cases=cases'}
    )
  | E_record_accessor {record; path} -> (
      let%bind record = self record in
      return @@ E_record_accessor {record; path}
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
  | E_application {lamb; args} -> (
      let ab = (lamb, args) in
      let%bind (a,b) = bind_map_pair self ab in
      return @@ E_application {lamb=a;args=b}
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
  | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
      let%bind result = self result in
      return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}
    )
  | E_constant c -> (
      let%bind args = bind_map_list self c.arguments in
      return @@ E_constant {c with arguments=args}
    )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'


and map_cases : self_ast_typed_error mapper -> matching_expr -> (matching_expr , self_ast_typed_error) result = fun f m ->
  match m with
  | Match_list { match_nil ; match_cons = {hd ; tl ; body ; tv} } -> (
      let%bind match_nil = map_expression f match_nil in
      let%bind body = map_expression f body in
      ok @@ Match_list { match_nil ; match_cons = {hd ; tl ; body; tv} }
    )
  | Match_option { match_none ; match_some = {opt ; body ; tv } } -> (
      let%bind match_none = map_expression f match_none in
      let%bind body = map_expression f body in
      ok @@ Match_option { match_none ; match_some = { opt ; body ; tv } }
    )
  | Match_variant {cases;tv} -> (
      let aux { constructor ; pattern ; body } =
        let%bind body = map_expression f body in
        ok {constructor;pattern;body}
      in
      let%bind cases = bind_map_list aux cases in
      ok @@ Match_variant {cases ; tv}
    )

and map_program : self_ast_typed_error mapper -> program -> (program, self_ast_typed_error) result = fun m p ->
  let aux = fun (x : declaration) ->
    match x with
    | Declaration_constant {binder; expr ; inline} -> (
        let%bind expr = map_expression m expr in
        ok (Declaration_constant {binder; expr ; inline})
    )
    | Declaration_type t -> ok (Declaration_type t)
  in
  bind_map_list (bind_map_location aux) p

type ('a , 'err) fold_mapper = 'a -> expression -> (bool * 'a * expression , 'err) result
let rec fold_map_expression : ('a , 'err) fold_mapper -> 'a -> expression -> ('a * expression , 'err) result = fun f a e ->
  let self = fold_map_expression f in
  let%bind (continue, init',e') = f a e in
  if (not continue) then ok(init',e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
      let%bind (res, e') = self init' e in
      let%bind (res,cases') = fold_map_cases f res cases in
      ok (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record_accessor {record; path} -> (
      let%bind (res, record) = self init' record in
      ok (res, return @@ E_record_accessor {record; path})
    )
  | E_record m -> (
    let%bind (res, lst') = bind_fold_map_list (fun res (k,e) -> let%bind (res,e) = self res e in ok (res,(k,e))) init' (LMap.to_kv_list_rev m) in
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
      let ab = (lamb, args) in
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
  | E_recursive { fun_name; fun_type; lambda={binder;result}} -> (
      let%bind (res,result) = self init' result in
      ok (res, return @@ E_recursive {fun_name; fun_type; lambda={binder;result}})
    )
  | E_constant c -> (
      let%bind (res,args) = bind_fold_map_list self init' c.arguments in
      ok (res, return @@ E_constant {c with arguments=args})
    )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> ok (init', return e')

and fold_map_cases : ('a , self_ast_typed_error) fold_mapper -> 'a -> matching_expr -> ('a * matching_expr , self_ast_typed_error) result = fun f init m ->
  match m with
  | Match_list { match_nil ; match_cons = { hd ; tl ; body ; tv } } -> (
      let%bind (init, match_nil) = fold_map_expression f init match_nil in
      let%bind (init, body) = fold_map_expression f init body in
      ok @@ (init, Match_list { match_nil ; match_cons = { hd ; tl ; body ; tv } })
    )
  | Match_option { match_none ; match_some = { opt ; body ; tv } } -> (
      let%bind (init, match_none) = fold_map_expression f init match_none in
      let%bind (init, body) = fold_map_expression f init body in
      ok @@ (init, Match_option { match_none ; match_some = { opt ; body ; tv } })
    )
  | Match_variant {cases ; tv} -> (
      let aux init {constructor ; pattern ; body} =
        let%bind (init, body) = fold_map_expression f init body in
        ok (init, {constructor; pattern ; body})
      in
      let%bind (init,cases) = bind_fold_map_list aux init cases in
      ok @@ (init, Match_variant {cases ; tv})
    )

and fold_map_program : ('a, self_ast_typed_error) fold_mapper -> 'a -> program -> ('a * program , self_ast_typed_error) result = fun m init p ->
  let aux = fun (acc,acc_prg) (x : declaration Location.wrap) ->
    match Location.unwrap x with
    | Declaration_constant {binder ; expr ; inline} -> (
        let%bind (acc', expr) = fold_map_expression m acc expr in
        let wrap_content = Declaration_constant {binder ; expr ; inline} in
        ok (acc', List.append acc_prg [{x with wrap_content}])
      )
    | Declaration_type t -> (
        let wrap_content = Declaration_type t in
        ok (acc, List.append acc_prg [{x with wrap_content}])
      )
  in
  bind_fold_list aux (init,[]) p

type contract_type = {
  parameter : Ast_typed.type_expression ;
  storage : Ast_typed.type_expression ;
}

let fetch_contract_type : string -> program -> (contract_type, self_ast_typed_error) result = fun main_fname program ->
  let aux declt = match Location.unwrap declt with
    | Declaration_constant ({ binder ; expr=_ ; inline=_ } as p) ->
       if Var.equal binder.wrap_content (Var.of_name main_fname)
       then Some p
       else None
    | Declaration_type _ -> None
  in
  let main_decl_opt = List.find_map aux @@ List.rev program in
  let%bind main_decl =
    trace_option (corner_case ("Entrypoint '"^main_fname^"' does not exist")) @@
      main_decl_opt
    in
  let { binder=_ ; expr ; inline=_ } = main_decl in
  match expr.type_expression.type_content with
  | T_arrow {type1 ; type2} -> (
    match type1.type_content , type2.type_content with
    | T_record tin , T_record tout when (is_tuple_lmap tin.content) && (is_tuple_lmap tout.content) ->
       let%bind (parameter,storage) = trace_option (expected_pair_in expr.location) @@ Ast_typed.Helpers.get_pair tin.content in
       let%bind (listop,storage') = trace_option (expected_pair_out expr.location) @@ Ast_typed.Helpers.get_pair tout.content in
       let%bind () = trace_option (expected_list_operation main_fname listop expr) @@
                       Ast_typed.assert_t_list_operation listop in
       let%bind () = trace_option (expected_same main_fname storage storage' expr) @@
                       Ast_typed.assert_type_expression_eq (storage,storage') in
       (* TODO: on storage/parameter : assert_storable, assert_passable ? *)
       ok { parameter ; storage }
    |  _ -> fail @@ bad_contract_io main_fname expr
  )
  | _ -> fail @@ bad_contract_io main_fname expr
