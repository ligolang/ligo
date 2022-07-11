open Ast_core
open Simple_utils.Trace
open Stage_common

include Ast_core.PP

let map_lmap_t f map =
  LMap.map
    (fun ({associated_type;_} as field) ->
      let field' = f associated_type in
      {field with associated_type = field'})
    map

type ('a,'err,'warn) folder = raise:('err,'warn) raise -> 'a -> expression -> 'a

let rec fold_expression ~raise : ('a, 'err, 'warn) folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression ~raise f in
  let idle = fun acc _ -> acc in
  let init = f ~raise init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ | E_module_accessor _ -> init
  | E_constant c -> Folds.constant self init c
  | E_application app -> Folds.application self init app
  | E_lambda l -> Folds.lambda self idle init l
  | E_type_abstraction ta -> Folds.type_abs self init ta
  | E_ascription a -> Folds.ascription self idle init a
  | E_constructor c -> Folds.constructor self init c
  | E_matching {matchee=e; cases} -> (
    let res = self init e in
    let aux acc ({body ; _ }: _ match_case) = self acc body in
    let res = List.fold ~f:aux ~init:res cases in
    res
  )
  | E_record m -> Folds.record self init m
  | E_record_update ru -> Folds.record_update self init ru
  | E_record_accessor ra -> Folds.record_accessor self init ra
  | E_let_in { let_binder = _ ; rhs ; let_result ; attr=_ } -> (
      let res = self init rhs in
      let res = self res let_result in
      res
    )
  | E_type_in { type_binder = _; rhs = _ ; let_result } ->
    let res = self init let_result in
    res
  | E_mod_in  mi ->
    let res = fold_expression_in_module_expr self init mi.rhs in
    let res = self res mi.let_result in
    res
  | E_recursive r -> Folds.recursive self idle init r
  | E_assign a -> Folds.assign self idle init a

and fold_expression_in_module_expr : ('a -> expression -> 'a)  -> 'a -> module_expr -> 'a = fun self acc x ->
  match x.wrap_content with
  | M_struct decls ->
    List.fold
      ~f:( fun acc (x: declaration) ->
        match x.wrap_content with
        | Declaration_constant x -> self acc x.expr
        | Declaration_module x -> fold_expression_in_module_expr self acc x.module_
        | Declaration_type _ ->  acc
      )
      ~init:acc
      decls
  | M_module_path _
  | M_variable _ -> acc

type ('err,'warn) exp_mapper = raise:('err,'warn) raise -> expression -> expression
type ('err,'warn) ty_exp_mapper = raise:('err,'warn) raise -> type_expression -> type_expression

let rec map_expression ~raise : ('err,'warn) exp_mapper -> expression -> expression = fun f e ->
  let self = map_expression ~raise f in
  let e' = f ~raise e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_ascription ascr -> (
      let ascr = Maps.ascription self (fun a -> a) ascr in
      return @@ E_ascription ascr
    )
  | E_matching {matchee=e;cases} -> (
    let e' = self e in
    let aux { pattern ; body } =
      let body' = self body in
      { pattern ; body = body'}
    in
    let cases' = List.map ~f:aux cases in
    return @@ E_matching {matchee=e';cases=cases'}
  )
  | E_record_accessor acc -> (
      let acc = Maps.record_accessor self acc in
      return @@ E_record_accessor acc
    )
  | E_record m -> (
    let m' = LMap.map self m in
    return @@ E_record m'
  )
  | E_record_update ru -> (
    let ru = Maps.record_update self ru in
    return @@ E_record_update ru
  )
  | E_constructor c -> (
    let c = Maps.constructor self c in
    return @@ E_constructor c
  )
  | E_application app -> (
    let app = Maps.application self app in
    return @@ E_application app
  )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
      let rhs = self rhs in
      let let_result = self let_result in
      return @@ E_let_in { let_binder ; rhs ; let_result; attr }
    )
  | E_type_in {type_binder; rhs; let_result} -> (
      let let_result = self let_result in
      return @@ E_type_in { type_binder ; rhs; let_result }
    )
  | E_mod_in  mi ->
    let rhs = map_expression_in_module_expr self mi.rhs in
    let let_result = self mi.let_result in
    return @@ E_mod_in {mi with rhs;let_result}
  | E_lambda l -> (
      let l = Maps.lambda self (fun a -> a) l in
      return @@ E_lambda l
    )
  | E_type_abstraction ta -> (
      let ta = Maps.type_abs self ta in
      return @@ E_type_abstraction ta
  )
  | E_recursive r ->
      let r = Maps.recursive self (fun a -> a) r in
      return @@ E_recursive r
  | E_constant c -> (
      let c = Maps.constant self c in
      return @@ E_constant c
    )
  | E_assign a ->
    let a = Maps.assign self (fun a -> a) a in
    return @@ E_assign a
  | E_literal _ | E_variable _ | E_raw_code _ | E_module_accessor _ as e' -> return e'

and map_expression_in_declarations : (expression -> expression) -> module_ -> module_ = fun self xs ->
  List.map
    ~f:( fun (x: declaration) ->
      let return wrap_content = { x with wrap_content } in
      match x.wrap_content with
      | Declaration_constant x ->
        let expr = self x.expr in
        return (Declaration_constant { x with expr })
      | Declaration_module x ->
        let module_ = map_expression_in_module_expr self x.module_ in
        return (Declaration_module { x with module_ })
      | Declaration_type _ -> x
    )
    xs

and map_expression_in_module_expr : (expression -> expression) -> module_expr -> module_expr = fun self x ->
  let return wrap_content = { x with wrap_content } in
  match x.wrap_content with
  | M_struct decls ->
    let decls = map_expression_in_declarations self decls in
    return (M_struct decls)
  | M_module_path _
  | M_variable _ -> x


and map_type_expression ~raise : ('err,'warn) ty_exp_mapper -> type_expression -> type_expression =
    fun f te ->
  let self = map_type_expression ~raise f in
  let te' = f ~raise te in
  let return type_content = { type_content; location=te.location ; sugar = te.sugar } in
  match te'.type_content with
  | T_sum { fields ; layout } ->
    let fields = map_lmap_t self fields in
    return @@ (T_sum { fields ; layout })
  | T_record {fields ; layout} ->
    let fields = map_lmap_t self fields in
    return @@ T_record {fields;layout}
  | T_arrow arr ->
    let arr = Maps.arrow self arr in
    return @@ T_arrow arr
  | T_app a ->
    let a' = Maps.type_app self a in
    return @@ T_app a'
  | T_variable _ -> te'
  | T_module_accessor _ -> te'
  | T_singleton _ -> te'
  | T_abstraction x ->
    let x = Maps.for_all self x in
    return (T_abstraction x)
  | T_for_all x ->
    let x = Maps.for_all self x in
    return (T_for_all x)



type 'a fold_mapper = 'a -> expression -> bool * 'a * expression
let rec fold_map_expression : type a . a fold_mapper -> a -> expression -> a * expression = fun f a e ->
  let self = fold_map_expression f in
  let idle acc a =  (acc,a) in
  let (continue, init,e') = f a e in
  if (not continue) then (init,e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_ascription ascr -> (
      let (res,ascr) = Fold_maps.ascription self idle init ascr in
      (res, return @@ E_ascription ascr)
    )
  | E_matching {matchee=e;cases} -> (
      let (res,e') = self init e in
      let aux acc { pattern ; body } =
        let (res,body') = self acc body in
        (res,{ pattern ; body = body'})
      in
      let (res, cases') = List.fold_map ~f:aux ~init:res cases in
       (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record m -> (
    let (res, m') = LMap.fold_map ~f:(fun _ e res -> self res e) ~init m in
    (res, return @@ E_record m')
  )
  | E_record_accessor acc -> (
      let (res, acc) = Fold_maps.record_accessor self init acc in
      (res, return @@ E_record_accessor acc)
    )
  | E_record_update ru -> (
    let res,ru = Fold_maps.record_update self init ru in
    (res, return @@ E_record_update ru)
  )
  | E_constructor c -> (
      let (res,c) = Fold_maps.constructor self init c in
      (res, return @@ E_constructor c)
  )
  | E_application app -> (
      let res,app = Fold_maps.application self init app in
      (res, return @@ E_application app)
    )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
      let (res,rhs) = self init rhs in
      let (res,let_result) = self res let_result in
      (res, return @@ E_let_in { let_binder ; rhs ; let_result ; attr })
    )
  | E_type_abstraction ta -> (
      let res, ta = Fold_maps.type_abs self init ta in
      res, return @@ E_type_abstraction ta
  )
  | E_type_in { type_binder; rhs; let_result } -> (
      let res, let_result = self init let_result in
      (res, return @@ E_type_in {type_binder; rhs; let_result})
    )
  | E_mod_in  mi ->
    let res,rhs = fold_map_expression_in_module_expr self init mi.rhs in
    let res,let_result = self res mi.let_result in
    (res, return @@ E_mod_in {mi with rhs;let_result})
  | E_lambda l -> (
      let res,l = Fold_maps.lambda self idle init l in
      ( res, return @@ E_lambda l)
    )
  | E_recursive r ->
      let res,r = Fold_maps.recursive self idle init r in
      ( res, return @@ E_recursive r)
  | E_constant c -> (
      let res,c = Fold_maps.constant self init c in
      (res, return @@ E_constant c)
    )
  | E_assign a ->
    let (res,a) = Fold_maps.assign self idle init a in
    (res, return @@ E_assign a)
  | E_literal _ | E_variable _ | E_raw_code _ | E_module_accessor _ as e' -> (init, return e')

and fold_map_expression_in_module_expr : type a . (a -> expression -> a * expression) -> a -> module_expr -> a * module_expr = fun self acc x ->
  let return r wrap_content = (r, { x with wrap_content }) in
  match x.wrap_content with
  | M_struct decls ->
    let res,decls = List.fold_map
      ~f:( fun acc (x: declaration) ->
        let return r wrap_content = (r, { x with wrap_content }) in
        match x.wrap_content with
        | Declaration_constant x ->
          let res,expr = self acc x.expr in
          return res (Declaration_constant { x with expr })
        | Declaration_module x ->
          let res,module_ = fold_map_expression_in_module_expr self acc x.module_ in
          return res (Declaration_module { x with module_ })
        | Declaration_type _ -> (acc,x)
      )
      ~init:acc
      decls
    in
    return res (M_struct decls)
  | M_module_path _ as x -> return acc x
  | M_variable _ as x ->
    return acc x
