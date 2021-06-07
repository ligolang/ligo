open Errors
open Ast_typed
open Trace
open Ast_typed.Helpers
open Stage_common

type ('a ,'err) decl_folder = 'a -> declaration -> ('a, 'err) result
type ('a ,'err) folder = 'a -> expression -> ('a , 'err) result
let rec fold_expression : ('a , 'err) folder -> 'a -> expression -> ('a , 'err) result = fun f init e ->
  let self = fold_expression f in
  let idle = fun acc _ -> ok @@ acc in
  let* init' = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ -> ok init'
  | E_constant {arguments=lst} -> (
    let* res = bind_fold_list self init' lst in
    ok res
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let* res = bind_fold_pair self init' ab in
    ok res
  )
  | E_lambda { binder = _ ; result = e }
  | E_recursive {lambda= {result=e}}
  | E_constructor {element=e} -> (
    let* res = self init' e in
    ok res
  )
  | E_matching {matchee=e; cases} -> (
    let* res = self init' e in
    let* res = fold_cases f res cases in
    ok res
  )
  | E_record m -> (
    let aux init'' _ expr =
      let* res = fold_expression self init'' expr in
      ok res
    in
    let* res = bind_fold_lmap aux (ok init') m in
    ok res
  )
  | E_record_update {record;update} -> (
    let* res = self init' record in
    let* res = fold_expression self res update in
    ok res
  )
  | E_record_accessor {record} -> (
    let* res = self init' record in
    ok res
  )
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let* res = self init' rhs in
      let* res = self res let_result in
      ok res
    )
  | E_type_in ti -> Folds.type_in self idle init' ti
  | E_mod_in { module_binder = _ ; rhs ; let_result } -> (
      let* res = fold_module f init' rhs in
      let* res = self res let_result in
      ok res
    )
  | E_mod_alias { alias = _ ; binders = _ ; result } -> (
      let* res = self init' result in
      ok res
    )
  | E_module_accessor { module_name = _ ; element } -> (
    let* res = self init' element in
    ok res
  )

and fold_cases : ('a , 'err) folder -> 'a -> matching_expr -> ('a , 'err) result = fun f init m ->
  match m with
  | Match_variant {cases;tv=_} -> (
      let aux init' {constructor=_; pattern=_ ; body} =
        let* res' = fold_expression f init' body in
        ok res' in
      let* res = bind_fold_list aux init cases in
      ok res
    )
  | Match_record {fields = _; body; tv = _} ->
    fold_expression f init body

and fold_module : ('a,'err) folder -> 'a -> module_fully_typed -> ('a, 'err) result = fun f init (Module_Fully_Typed p) ->
  let aux = fun acc (x : declaration) ->
    let return (d : 'a) = ok @@ d in
    match x with
    | Declaration_constant {binder=_; expr ; inline=_} -> (
        let* res = fold_expression f acc expr in
        return @@ res
    )
    | Declaration_type _t -> return @@ acc
    | Declaration_module {module_binder=_;module_} ->
      let* res = fold_module f acc module_ in
      return @@ res
    | Module_alias _ -> return @@ acc
  in
  let* res = bind_fold_list (bind_fold_location aux) init p in
  ok @@ res

type 'err mapper = expression -> (expression , 'err) result
let rec map_expression : 'err mapper -> expression -> (expression , 'err) result = fun f e ->
  let self = map_expression f in
  let* e' = f e in
  let return expression_content = ok { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
    let* e' = self e in
    let* cases' = map_cases f cases in
    return @@ E_matching {matchee=e';cases=cases'}
  )
  | E_record_accessor {record; path} -> (
    let* record = self record in
    return @@ E_record_accessor {record; path}
  )
  | E_record m -> (
    let* m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_record_update {record; path; update} -> (
    let* record = self record in
    let* update = self update in
    return @@ E_record_update {record;path;update}
  )
  | E_constructor c -> (
    let* e' = self c.element in
    return @@ E_constructor {c with element = e'}
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let* (a,b) = bind_map_pair self ab in
    return @@ E_application {lamb=a;args=b}
  )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
    let* rhs = self rhs in
    let* let_result = self let_result in
    return @@ E_let_in { let_binder ; rhs ; let_result; inline }
  )
  | E_type_in ti -> (
    let* ti = Maps.type_in self ok ti in
    return @@ E_type_in ti
  )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
    let* rhs = map_module f rhs in
    let* let_result = self let_result in
    return @@ E_mod_in { module_binder ; rhs ; let_result }
  )
  | E_mod_alias { alias ; binders ; result } -> (
    let* result = self result in
    return @@ E_mod_alias { alias ; binders ; result }
  )
  | E_lambda { binder ; result } -> (
    let* result = self result in
    return @@ E_lambda { binder ; result }
  )
  | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
    let* result = self result in
    return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}
  )
  | E_constant c -> (
    let* args = bind_map_list self c.arguments in
    return @@ E_constant {c with arguments=args}
  )
  | E_module_accessor { module_name; element } -> (
    let* element = self element in
    return @@ E_module_accessor { module_name; element }
  )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'


and map_cases : 'err mapper -> matching_expr -> (matching_expr , 'err) result = fun f m ->
  match m with
  | Match_variant {cases;tv} -> (
      let aux { constructor ; pattern ; body } =
        let* body = map_expression f body in
        ok {constructor;pattern;body}
      in
      let* cases = bind_map_list aux cases in
      ok @@ Match_variant {cases ; tv}
    )
  | Match_record {fields; body; tv} ->
    let* body = map_expression f body in
    ok @@ Match_record {fields; body; tv}

and map_module : 'err mapper -> module_fully_typed -> (module_fully_typed, 'err) result = fun m (Module_Fully_Typed p) ->
  let aux = fun (x : declaration) ->
    let return (d : declaration) = ok @@ d in
    match x with
    | Declaration_constant {name; binder; expr ; inline} -> (
        let* expr = map_expression m expr in
        return @@ Declaration_constant {name; binder; expr ; inline}
    )
    | Declaration_type t -> return @@ Declaration_type t
    | Declaration_module {module_binder;module_} ->
      let* module_ = map_module m module_ in
      return @@ Declaration_module {module_binder; module_}
    | Module_alias _ -> return x
  in
  let* p = bind_map_list (bind_map_location aux) p in
  ok @@ Module_Fully_Typed p

type ('a , 'err) fold_mapper = 'a -> expression -> (bool * 'a * expression , 'err) result
let rec fold_map_expression : ('a , 'err) fold_mapper -> 'a -> expression -> ('a * expression , 'err) result = fun f a e ->
  let self = fold_map_expression f in
  let* (continue, init',e') = f a e in
  if (not continue) then ok(init',e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
      let* (res, e') = self init' e in
      let* (res,cases') = fold_map_cases f res cases in
      ok (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record_accessor {record; path} -> (
      let* (res, record) = self init' record in
      ok (res, return @@ E_record_accessor {record; path})
    )
  | E_record m -> (
    let* (res, lst') = bind_fold_map_list (fun res (k,e) -> let* (res,e) = self res e in ok (res,(k,e))) init' (LMap.to_kv_list_rev m) in
    let m' = LMap.of_list lst' in
    ok (res, return @@ E_record m')
  )
  | E_record_update {record; path; update} -> (
    let* (res, record) = self init' record in
    let* (res, update) = self res update in
    ok (res, return @@ E_record_update {record;path;update})
  )
  | E_constructor c -> (
      let* (res,e') = self init' c.element in
      ok (res, return @@ E_constructor {c with element = e'})
  )
  | E_application {lamb;args} -> (
      let ab = (lamb, args) in
      let* (res,(a,b)) = bind_fold_map_pair self init' ab in
      ok (res, return @@ E_application {lamb=a;args=b})
    )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let* (res,rhs) = self init' rhs in
      let* (res,let_result) = self res let_result in
      ok (res, return @@ E_let_in { let_binder ; rhs ; let_result ; inline })
    )
  | E_type_in { type_binder ; rhs ; let_result } -> (
      let* (res,let_result) = self init' let_result in
      ok (res, return @@ E_type_in { type_binder ; rhs ; let_result })
    )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
      let* (res,let_result) = self init' let_result in
      ok (res, return @@ E_mod_in { module_binder ; rhs ; let_result })
    )
  | E_mod_alias { alias ; binders ; result } -> (
      let* (res,result) = self init' result in
      ok (res, return @@ E_mod_alias { alias ; binders ; result })
    )
  | E_lambda { binder ; result } -> (
      let* (res,result) = self init' result in
      ok ( res, return @@ E_lambda { binder ; result })
    )
  | E_recursive { fun_name; fun_type; lambda={binder;result}} -> (
      let* (res,result) = self init' result in
      ok (res, return @@ E_recursive {fun_name; fun_type; lambda={binder;result}})
    )
  | E_constant c -> (
      let* (res,args) = bind_fold_map_list self init' c.arguments in
      ok (res, return @@ E_constant {c with arguments=args})
    )
  | E_module_accessor { module_name; element } -> (
    let* (res,element) = self init' element in
    ok (res, return @@ E_module_accessor { module_name; element })
  )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> ok (init', return e')

and fold_map_cases : ('a , 'err) fold_mapper -> 'a -> matching_expr -> ('a * matching_expr , 'err) result = fun f init m ->
  match m with
  | Match_variant {cases ; tv} -> (
      let aux init {constructor ; pattern ; body} =
        let* (init, body) = fold_map_expression f init body in
        ok (init, {constructor; pattern ; body})
      in
      let* (init,cases) = bind_fold_map_list aux init cases in
      ok @@ (init, Match_variant {cases ; tv})
    )
  | Match_record { fields; body; tv } ->
      let* (init, body) = fold_map_expression f init body in
      ok @@ (init, Match_record { fields ; body ; tv })

and fold_map_module : ('a, 'err) fold_mapper -> 'a -> module_fully_typed -> ('a * module_fully_typed , 'err) result = fun m init (Module_Fully_Typed p) ->
  let aux = fun acc (x : declaration Location.wrap) ->
    match Location.unwrap x with
    | Declaration_constant {name; binder ; expr ; inline} -> (
      let* (acc', expr) = fold_map_expression m acc expr in
      let wrap_content : declaration = Declaration_constant {name; binder ; expr ; inline} in
      ok (acc', {x with wrap_content})
    )
    | Declaration_type t -> (
      let wrap_content : declaration = Declaration_type t in
      ok (acc, {x with wrap_content})
    )
    | Declaration_module m -> (
      let wrap_content : declaration = Declaration_module m in
      ok (acc, {x with wrap_content})
    )
    | Module_alias _ -> ok (acc,x)
  in
  let* (a,p) = bind_fold_map_list aux init p in
  ok (a, Module_Fully_Typed p)

and fold_module_decl : ('a, 'err) folder -> ('a, 'err) decl_folder -> 'a -> module_fully_typed -> ('a, 'err) result = fun m m_decl init (Module_Fully_Typed p) ->
  let aux = fun acc (x : declaration Location.wrap) ->
      match Location.unwrap x with
      | Declaration_constant {binder=_ ; expr ; inline=_} as d ->
        let* acc = m_decl acc d in
        fold_expression m acc expr
      | Declaration_type _t -> ok acc
      | Declaration_module _m -> ok acc
      | Module_alias _m -> ok acc
    in
    bind_fold_list aux (init) p

type contract_type = {
  parameter : Ast_typed.type_expression ;
  storage : Ast_typed.type_expression ;
}

let fetch_contract_type : string -> module_fully_typed -> (contract_type, 'err) result = fun main_fname (Module_Fully_Typed m) ->
  let aux (declt : declaration Location.wrap) = match Location.unwrap declt with
    | Declaration_constant ({ binder ; expr=_ ; inline=_ } as p) ->
       if Var.equal binder.wrap_content (Var.of_name main_fname)
       then Some p
       else None
    | Declaration_type   _
    | Declaration_module _
    | Module_alias _ -> None
  in
  let main_decl_opt = List.find_map ~f:aux @@ List.rev m in
  let* main_decl =
    trace_option (corner_case ("Entrypoint '"^main_fname^"' does not exist")) @@
      main_decl_opt
    in
  let { binder=_ ; expr ; inline=_ } = main_decl in
  match expr.type_expression.type_content with
  | T_arrow {type1 ; type2} -> (
    match type1.type_content , type2.type_content with
    | T_record tin , T_record tout when (is_tuple_lmap tin.content) && (is_tuple_lmap tout.content) ->
       let* (parameter,storage) = trace_option (expected_pair_in expr.location) @@ Ast_typed.Helpers.get_pair tin.content in
       let* (listop,storage') = trace_option (expected_pair_out expr.location) @@ Ast_typed.Helpers.get_pair tout.content in
       let* () = trace_option (expected_list_operation main_fname listop expr) @@
                       Ast_typed.assert_t_list_operation listop in
       let* () = trace_option (expected_same main_fname storage storage' expr) @@
                       Ast_typed.assert_type_expression_eq (storage,storage') in
       (* TODO: on storage/parameter : a| Some (typed_prg,_,_) ->
        let b = extract_variable_types typed_prg in
        let () = Format.printf "\n EXTRACT \n" in
        let () = List.iter ~f: (fun (v,te) -> Format.printf "%a  --  %a\n" Ast_typed.PP.expression_variable v Ast_typed.PP.type_expression te) b in
        let () = Format.printf "length : %d\n" (List.length b) in
      ssert_storable, assert_passable ? *)
       ok { parameter ; storage }
    |  _ -> fail @@ bad_contract_io main_fname expr
  )
  | _ -> fail @@ bad_contract_io main_fname expr
