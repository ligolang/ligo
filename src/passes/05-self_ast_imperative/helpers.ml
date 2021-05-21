open Ast_imperative
open Trace
open Stage_common.Helpers
open Stage_common

let bind_map_lmap_t f map = bind_lmap (
  LMap.map
    (fun ({associated_type;_} as field : _ row_element) ->
      let* associated_type = f associated_type in
      ok {field with associated_type })
    map)

type ('a,'err) folder = 'a -> expression -> ('a, 'err) result
let rec fold_expression : ('a, 'err) folder -> 'a -> expression -> ('a, 'err) result = fun f init e ->
  let self = fold_expression f in
  let* init' = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip -> ok init'
  | E_list lst | E_set lst | E_constant {arguments=lst} -> (
    let* res = bind_fold_list self init' lst in
    ok res
  )
  | E_map lst | E_big_map lst -> (
    let* res = bind_fold_list (bind_fold_pair self) init' lst in
    ok res
  )
  | E_application app -> Folds.application self init' app
  | E_lambda l -> Folds.lambda self (fun _ -> ok) init' l
  | E_ascription a -> Folds.ascription self (fun _ -> ok) init' a
  | E_constructor c -> Folds.constructor self init' c
  | E_matching {matchee=e; cases} -> (
      let* res = self init' e in
      let aux acc ({body ; _ }: _ Ast_imperative.match_case) = self acc body in
      let* res = bind_fold_list aux res cases in
      ok res
    )
  | E_record m -> Folds.record self init' m
  | E_update u -> Folds.update self init' u
  | E_accessor a -> Folds.accessor self init' a
  | E_tuple t -> Folds.tuple self init' t
  | E_let_in  li -> Folds.let_in self (fun _ -> ok) init' li
  | E_type_in ti -> Folds.type_in self (fun _ -> ok) init' ti
  | E_mod_in  mi -> Folds.mod_in  self (fun _ -> ok) init' mi
  | E_mod_alias ma -> Folds.mod_alias self init' ma
  | E_cond       c -> Folds.conditional self init' c
  | E_recursive  r -> Folds.recursive self (fun _ -> ok) init' r
  | E_module_accessor { module_name = _ ; element } -> (
    let* res = self init' element in
    ok res
  )
  | E_sequence s -> Folds.sequence self init' s
  | E_assign a -> Folds.assign self init' a
  | E_for f -> Folds.for_ self init' f
  | E_for_each fe -> Folds.for_each self init' fe
  | E_while w -> Folds.while_loop self init' w

type 'err exp_mapper = expression -> (expression , 'err) result
type 'err ty_exp_mapper = type_expression -> (type_expression, 'err) result
type 'err abs_mapper =
  | Expression of 'err exp_mapper
  | Type_expression of 'err ty_exp_mapper
let rec map_expression : 'err exp_mapper -> expression -> (expression, 'err) result = fun f e ->
  let self = map_expression f in
  let* e' = f e in
  let return expression_content = ok { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let* lst' = bind_map_list self lst in
    return @@ E_list lst'
  )
  | E_set lst -> (
    let* lst' = bind_map_list self lst in
    return @@ E_set lst'
  )
  | E_map lst -> (
    let* lst' = bind_map_list (bind_map_pair self) lst in
    return @@ E_map lst'
  )
  | E_big_map lst -> (
    let* lst' = bind_map_list (bind_map_pair self) lst in
    return @@ E_big_map lst'
  )
  | E_ascription ascr -> (
      let* ascr = Maps.ascription self ok ascr in
      return @@ E_ascription ascr
    )
  | E_matching {matchee=e;cases} ->
    let* e' = self e in
    let aux { pattern ; body } =
      let* body' = self body in
      ok { pattern ; body = body'}
    in
    let* cases' = bind_map_list aux cases in
    return @@ E_matching {matchee=e';cases=cases'}
  | E_record m -> (
    let* m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_accessor acc -> (
      let* acc = Maps.accessor self acc in
      return @@ E_accessor acc
    )
  | E_update u -> (
    let* u = Maps.update self u in
    return @@ E_update u
  )
  | E_tuple t -> (
    let* t' = bind_map_list self t in
    return @@ E_tuple t'
  )
  | E_constructor c -> (
      let* c = Maps.constructor self c in
      return @@ E_constructor c
  )
  | E_application app -> (
    let* app = Maps.application self app in
    return @@ E_application app
  )
  | E_let_in li -> (
      let* li = Maps.let_in self ok li in
      return @@ E_let_in li
    )
  | E_type_in ti -> (
      let* ti = Maps.type_in self ok ti in
      return @@ E_type_in ti
    )
  | E_mod_alias ma -> (
      let* ma = Maps.mod_alias self ma in
      return @@ E_mod_alias ma
    )
  | E_mod_in mi -> (
      let* mi = Maps.mod_in self ok mi in
      return @@ E_mod_in mi
    )
  | E_lambda l -> (
      let* l = Maps.lambda self ok l in
      return @@ E_lambda l
    )
  | E_recursive r ->
      let* r = Maps.recursive self ok r in
      return @@ E_recursive r
  | E_constant c -> (
      let* args = bind_map_list self c.arguments in
      return @@ E_constant {c with arguments=args}
    )
  | E_cond c ->
      let* c = Maps.conditional self c in
      return @@ E_cond c
  | E_sequence s -> (
      let* s = Maps.sequence self s in
      return @@ E_sequence s
    )
  | E_assign a -> (
      let* a = Maps.assign self a in
      return @@ E_assign a
  )
  | E_for f ->
      let* f = Maps.for_ self f in
      return @@ E_for f
  | E_for_each fe ->
      let* fe = Maps.for_each self fe in
      return @@ E_for_each fe
  | E_while w ->
      let* w = Maps.while_loop self w in
      return @@ E_while w
  | E_module_accessor { module_name; element } -> (
    let* element = self element in
    return @@ E_module_accessor { module_name; element }
  )
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip as e' -> return e'

and map_type_expression : 'err ty_exp_mapper -> type_expression -> (type_expression , _) result = fun f te ->
  let module SSet = Set.Make (String) in
  let self = map_type_expression f in
  let* te' = f te in
  let return type_content = ok { type_content; location=te.location } in
  match te'.type_content with
  | T_sum temap ->
    let* temap' = Maps.rows self temap in
    return @@ T_sum temap'
  | T_record temap ->
    let* temap' = Maps.rows self temap in
    return @@ T_record temap'
  | T_tuple telst ->
    let* telst' = bind_map_list self telst in
    return @@ T_tuple telst'
  | T_arrow arr ->
    let* arr = Maps.arrow self arr in
    return @@ T_arrow arr
  | T_annoted (ty, str) ->
    let* ty = self ty in
    return @@ T_annoted (ty, str)
  | T_app {type_operator;arguments} ->
    let* arguments = bind_map_list self arguments in
    return @@ T_app {type_operator;arguments}
  | T_variable _ -> ok te'
  | T_module_accessor ma ->
    let* ma = Maps.module_access self ma in
    return @@ T_module_accessor ma
  | T_singleton _ -> ok te'

and map_module : 'err abs_mapper -> module_ -> (module_ , _) result = fun m p ->
  let aux = fun (x : declaration) ->
    match x,m with
    | (Declaration_constant dc, Expression m') -> (
        let* dc = Maps.declaration_constant (map_expression m') ok dc in
        ok (Declaration_constant dc)
      )
    | (Declaration_type dt, Type_expression m') -> (
        let* dt = Maps.declaration_type (map_type_expression m') dt in
        ok (Declaration_type dt)
      )
    | decl,_ -> ok decl
  (* | Declaration_type of (type_variable * type_expression) *)
  in
  bind_map_list (bind_map_location aux) p

type ('a, 'err) fold_mapper = 'a -> expression -> ((bool * 'a * expression), 'err) result
let rec fold_map_expression : ('a, 'err) fold_mapper -> 'a -> expression -> ('a * expression , 'err) result = fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = ok @@ (acc,a) in
  let* (continue, init',e') = f a e in
  if (not continue) then ok(init',e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let* (res, lst') = bind_fold_map_list self init' lst in
    ok (res, return @@ E_list lst')
  )
  | E_set lst -> (
    let* (res, lst') = bind_fold_map_list self init' lst in
    ok (res, return @@ E_set lst')
  )
  | E_map lst -> (
    let* (res, lst') = bind_fold_map_list (bind_fold_map_pair self) init' lst in
    ok (res, return @@ E_map lst')
  )
  | E_big_map lst -> (
    let* (res, lst') = bind_fold_map_list (bind_fold_map_pair self) init' lst in
    ok (res, return @@ E_big_map lst')
  )
  | E_ascription ascr -> (
      let* (res,ascr) = Fold_maps.ascription self idle init' ascr in
      ok (res, return @@ E_ascription ascr)
    )
  | E_matching {matchee=e;cases} ->
    let* (res,e') = self init' e in
    let aux acc { pattern ; body } =
      let* (res,body') = self acc body in
      ok (res,{ pattern ; body = body'})
    in
    let* (res, cases') = bind_fold_map_list aux res cases in
    ok @@ (res, return @@ E_matching {matchee=e';cases=cases'})
  | E_record m -> (
    let* (res, m') = bind_fold_map_lmap (fun res _ e -> self res e) init' m in
    ok (res, return @@ E_record m')
  )
  | E_accessor acc -> (
      let* (res, acc) = Fold_maps.accessor self init' acc in
      ok (res, return @@ E_accessor acc)
    )
  | E_update u -> (
    let* res,u = Fold_maps.update self init' u in
    ok (res, return @@ E_update u)
  )
  | E_tuple t -> (
    let* (res, t') = bind_fold_map_list self init' t in
    ok (res, return @@ E_tuple t')
  )
  | E_constructor c -> (
      let* (res,c) = Fold_maps.constructor self init' c in
      ok (res, return @@ E_constructor c)
  )
  | E_application app -> (
      let* res,app = Fold_maps.application self init' app in
      ok (res, return @@ E_application app)
    )
  | E_let_in li -> (
      let* res,li = Fold_maps.let_in self idle init' li in
      ok (res, return @@ E_let_in li)
    )
  | E_type_in ti -> (
      let* res,ti = Fold_maps.type_in self idle init' ti in
      ok (res, return @@ E_type_in ti)
    )
  | E_mod_in mi -> (
      let* res,mi = Fold_maps.mod_in self idle init' mi in
      ok (res, return @@ E_mod_in mi)
    )
  | E_mod_alias ma -> (
      let* res,ma = Fold_maps.mod_alias self init' ma in
      ok (res, return @@ E_mod_alias ma)
    )
  | E_lambda l -> (
      let* res,l = Fold_maps.lambda self idle init' l in
      ok ( res, return @@ E_lambda l)
    )
  | E_recursive r ->
      let* res,r = Fold_maps.recursive self idle init' r in
      ok ( res, return @@ E_recursive r)
  | E_constant c -> (
      let* (res,args) = bind_fold_map_list self init' c.arguments in
      ok (res, return @@ E_constant {c with arguments=args})
    )
  | E_cond c ->
      let* res,c = Fold_maps.conditional self init' c in
      ok (res, return @@ E_cond c)
  | E_sequence s -> (
      let* res,s = Fold_maps.sequence self init' s in
      ok (res, return @@ E_sequence s)
    )
  | E_assign a ->
      let* res,a = Fold_maps.assign self init' a in
      ok (res, return @@ E_assign a)
  | E_for f ->
      let* res,f = Fold_maps.for_ self init' f in
      ok (res, return @@ E_for f)
  | E_for_each fe ->
      let* res,fe = Fold_maps.for_each self init' fe in
      ok (res, return @@ E_for_each fe)
  | E_while w ->
      let* res,w = Fold_maps.while_loop self init' w in
      ok (res, return @@ E_while w)
  | E_module_accessor { module_name; element } -> (
    let* (res,element) = self init' element in
    ok (res, return @@ E_module_accessor { module_name; element })
  )
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip as e' -> ok (init', return e')

let compare_vars e e' =
  Location.compare_content ~compare:Var.compare e e'

let in_vars var vars =
  List.mem ~compare:compare_vars var vars

let remove_from var vars =
  let f v vars = if compare_vars var v = 0 then vars else v :: vars in
  List.fold_right f vars []

let get_pattern ?(pred = fun _ -> true) pattern =
  Stage_common.Helpers.fold_pattern (fun vars p ->
      match p.wrap_content with
      | P_var {var;attributes} when pred attributes ->
         var :: vars
      | _ -> vars) [] pattern

let rec get_fv ?(exclude = []) (expr : expression) =
  let self = get_fv ~exclude in
  let* (fv, _) = fold_map_expression
    (fun (fv : expression_variable list) (expr : expression) ->
      match expr.expression_content with
      | E_variable v when not (in_vars v exclude) && not (in_vars v fv) ->
         ok (false, v :: fv, expr)
      | E_let_in {let_binder={var};rhs;let_result} ->
         let* rhs_vars = self rhs in
         let* result_vars = self let_result in
         let result_vars = remove_from var result_vars in
         ok (false, rhs_vars @ result_vars, expr)
      | E_lambda {binder={var};result} ->
         let* result_vars = self result in
         ok (false, remove_from var result_vars, expr)
      | E_matching {matchee=e;cases} ->
         let* res = self e in
         let aux acc ({ pattern ; body } : (expression, type_expression) match_case) =
           let* cur = self body in
           let cur = List.fold_right remove_from (get_pattern pattern) cur in
           ok (acc @ cur)
         in
         let* res = bind_fold_list aux res cases in
         ok @@ (false, res, expr)
      | E_recursive {lambda={binder={var};result};fun_name} ->
         let* result_vars = self result in
         ok (false, remove_from fun_name @@ remove_from var @@ result_vars, expr)
      | _ -> ok (true,fv, expr)
    ) [] expr in
  ok @@ fv
