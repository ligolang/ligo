open Ast_core
open Trace
open Ast_core.Helpers
open Stage_common

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
  let idle = fun acc _ -> ok @@ acc in
  let%bind init' = f init e in
  match e.content with
  | E_literal _ | E_variable _ | E_raw_code _ -> ok init'
  | E_constant c -> Folds.constant self init' c
  | E_application app -> Folds.application self init' app
  | E_lambda l -> Folds.lambda self idle init' l
  | E_ascription a -> Folds.ascription self idle init' a
  | E_constructor c -> Folds.constructor self init' c
  | E_matching {matchee=e; cases} -> (
      let%bind res = self init' e in
      let%bind res = fold_cases f res cases in
      ok res
    )
  | E_record m -> Folds.record self init' m
  | E_record_update ru -> Folds.record_update self init' ru
  | E_record_accessor ra -> Folds.record_accessor self init' ra
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let%bind res = self init' rhs in
      let%bind res = self res let_result in
      ok res
    )
  | E_type_in ti -> Folds.type_in self idle init' ti
  | E_recursive r -> Folds.recursive self idle init' r

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
      let%bind ascr = Maps.ascription self ok ascr in
      return @@ E_ascription ascr
    )
  | E_matching {matchee=e;cases} -> (
      let%bind e' = self e in
      let%bind cases' = map_cases f cases in
      return @@ E_matching {matchee=e';cases=cases'}
    )
  | E_record_accessor acc -> (
      let%bind acc = Maps.record_accessor self acc in
      return @@ E_record_accessor acc
    )
  | E_record m -> (
    let%bind m' = bind_map_lmap self m in
    return @@ E_record m'
  )
  | E_record_update ru -> (
    let%bind ru = Maps.record_update self ru in
    return @@ E_record_update ru
  )
  | E_constructor c -> (
      let%bind c = Maps.constructor self c in
      return @@ E_constructor c
  )
  | E_application app -> (
    let%bind app = Maps.application self app in
    return @@ E_application app
  )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let%bind rhs = self rhs in
      let%bind let_result = self let_result in
      return @@ E_let_in { let_binder ; rhs ; let_result; inline }
    )
  | E_type_in ti -> (
      let%bind ti = Maps.type_in self ok ti in
      return @@ E_type_in ti
    )
  | E_lambda l -> (
      let%bind l = Maps.lambda self ok l in
      return @@ E_lambda l
    )
  | E_recursive r ->
      let%bind r = Maps.recursive self ok r in
      return @@ E_recursive r
  | E_constant c -> (
      let%bind c = Maps.constant self c in
      return @@ E_constant c
    )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'

and map_type_expression : 'err ty_exp_mapper -> type_expression -> (type_expression , 'err) result =
    fun f te ->
  let self = map_type_expression f in
  let%bind te' = f te in
  let return type_content = ok { type_content; location=te.location ; sugar = te.sugar } in
  match te'.type_content with
  | T_sum { fields ; layout } ->
    let%bind fields = bind_map_lmap_t self fields in
    return @@ (T_sum { fields ; layout })
  | T_record {fields ; layout} ->
    let%bind fields = bind_map_lmap_t self fields in
    return @@ T_record {fields;layout}
  | T_arrow arr ->
    let%bind arr = Maps.arrow self arr in
    return @@ T_arrow arr
  | T_app a ->
    let%bind a' = Maps.type_app self a in
    return @@ T_app a'
  | T_variable _ -> ok te'


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
    | (Declaration_type dt, Type_expression m') -> (
        let%bind dt = Maps.declaration_type (map_type_expression m') dt in
        ok (Declaration_type dt)
      )
    | (Declaration_constant decl_cst, Expression m') -> (
        let%bind expr = map_expression m' decl_cst.expr in
        ok (Declaration_constant {decl_cst with expr})
      )
    | decl,_ -> ok decl
  (* | Declaration_type of (type_variable * type_expression) *)
  in
  bind_map_list (bind_map_location aux) p

type ('a , 'err) fold_mapper = 'a -> expression -> (bool * 'a * expression , 'err) result
let rec fold_map_expression : ('a , 'err) fold_mapper -> 'a -> expression -> ('a * expression , 'err) result = fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = ok @@ (acc,a) in
  let%bind (continue, init',e') = f a e in
  if (not continue) then ok(init',e')
  else
  let return content = { e' with content } in
  match e'.content with
  | E_ascription ascr -> (
      let%bind (res,ascr) = Fold_maps.ascription self idle init' ascr in
      ok (res, return @@ E_ascription ascr)
    )
  | E_matching {matchee=e;cases} -> (
      let%bind (res, e') = self init' e in
      let%bind (res,cases') = fold_map_cases f res cases in
      ok (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record m -> (
    let%bind (res, m') = bind_fold_map_lmap (fun res _ e -> self res e) init' m in
    ok (res, return @@ E_record m')
  )
  | E_record_accessor acc -> (
      let%bind (res, acc) = Fold_maps.record_accessor self init' acc in
      ok (res, return @@ E_record_accessor acc)
    )
  | E_record_update ru -> (
    let%bind res,ru = Fold_maps.record_update self init' ru in
    ok (res, return @@ E_record_update ru)
  )
  | E_constructor c -> (
      let%bind (res,c) = Fold_maps.constructor self init' c in
      ok (res, return @@ E_constructor c)
  )
  | E_application app -> (
      let%bind res,app = Fold_maps.application self init' app in
      ok (res, return @@ E_application app)
    )
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let%bind (res,rhs) = self init' rhs in
      let%bind (res,let_result) = self res let_result in
      ok (res, return @@ E_let_in { let_binder ; rhs ; let_result ; inline })
    )
  | E_type_in ti -> (
      let%bind res,ti = Fold_maps.type_in self idle init' ti in
      ok (res, return @@ E_type_in ti)
    )
  | E_lambda l -> (
      let%bind res,l = Fold_maps.lambda self idle init' l in
      ok ( res, return @@ E_lambda l)
    )
  | E_recursive r ->
      let%bind res,r = Fold_maps.recursive self idle init' r in
      ok ( res, return @@ E_recursive r)
  | E_constant c -> (
      let%bind res,c = Fold_maps.constant self init' c in
      ok (res, return @@ E_constant c)
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
