open Ast_core
open Simple_utils.Trace
open Ligo_prim

include Ast_core.PP

let map_lmap_t f map =
  Record.map
    (fun ({associated_type;_} as field : _ Rows.row_element_mini_c ) ->
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
  | E_constant c -> Constant.fold self init c
  | E_application app -> Application.fold self init app
  | E_lambda l -> Lambda.fold self idle init l
  | E_type_abstraction ta -> Type_abs.fold self init ta
  | E_ascription a -> Ascription.fold self idle init a
  | E_constructor c -> Constructor.fold self init c
  | E_matching {matchee=e; cases} -> (
    let res = self init e in
    let aux acc ({body ; _ }: _ Match_expr.match_case) = self acc body in
    let res = List.fold ~f:aux ~init:res cases in
    res
  )
  | E_record m -> Record.fold self init m
  | E_update   ru -> Types.Update.fold self init ru
  | E_accessor ra -> Types.Accessor.fold self init ra
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
  | E_recursive r -> Recursive.fold self idle init r
  | E_assign a -> Assign.fold self idle init a

and fold_expression_in_module_expr : ('a -> expression -> 'a)  -> 'a -> module_expr -> 'a = fun self acc x ->
  match x.wrap_content with
  | Module_expr.M_struct (decls : Ast_core.decl list) ->
    List.fold
      ~f:( fun acc (Decl x) ->
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
  let self_type = Fun.id in
  let e' = f ~raise e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_ascription ascr -> (
      let ascr = Ascription.map self self_type ascr in
      return @@ E_ascription ascr
    )
  | E_matching match_expr ->
    let match_expr = Match_expr.map self (fun a -> a) match_expr in
    return @@ E_matching match_expr
  | E_accessor acc -> (
      let acc = Types.Accessor.map self acc in
      return @@ E_accessor acc
    )
  | E_record m -> (
    let m' = Record.map self m in
    return @@ E_record m'
  )
  | E_update ru -> (
    let ru = Types.Update.map self ru in
    return @@ E_update ru
  )
  | E_constructor c -> (
    let c = Constructor.map self c in
    return @@ E_constructor c
  )
  | E_application app -> (
    let app = Application.map self app in
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
      let l = Lambda.map self (fun a -> a) l in
      return @@ E_lambda l
    )
  | E_type_abstraction ta -> (
      let ta = Type_abs.map self ta in
      return @@ E_type_abstraction ta
  )
  | E_recursive r ->
      let r = Recursive.map self (fun a -> a) r in
      return @@ E_recursive r
  | E_constant c -> (
      let c = Constant.map self c in
      return @@ E_constant c
    )
  | E_assign a ->
    let a = Assign.map self (fun a -> a) a in
    return @@ E_assign a
  | E_literal _ | E_variable _ | E_raw_code _ | E_module_accessor _ as e' -> return e'

and map_expression_in_declaration : (expression -> expression) -> declaration -> declaration = fun self xs ->
    let return wrap_content : declaration = { xs with wrap_content } in
    match xs.wrap_content with
    | Declaration_constant x ->
      let expr = self x.expr in
      return (Declaration_constant { x with expr })
    | Declaration_module x ->
      let module_ = map_expression_in_module_expr self x.module_ in
      return (Declaration_module { x with module_ })
    | Declaration_type _ -> xs

and map_expression_in_declarations : (expression -> expression) -> program -> program = fun self xs ->
  List.map ~f:(map_expression_in_declaration self) xs

and map_expression_in_module_expr : (expression -> expression) -> module_expr -> module_expr = fun self x ->
  let return wrap_content : module_expr = { x with wrap_content } in
  match x.wrap_content with
  | M_struct decls ->
    let decls = List.map ~f:(fun (Decl d) -> Decl (map_expression_in_declaration self d)) decls in
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
    let arr = Arrow.map self arr in
    return @@ T_arrow arr
  | T_app {type_operator;arguments} ->
    let arguments = List.map ~f:self arguments in
    return @@ T_app {type_operator;arguments}
  | T_variable _ -> te'
  | T_module_accessor _ -> te'
  | T_singleton _ -> te'
  | T_abstraction x ->
    let x = Abstraction.map self x in
    return @@ T_abstraction x
  | T_for_all x ->
    let x = Abstraction.map self x in
    return @@ T_for_all x


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
      let (res,ascr) = Ascription.fold_map self idle init ascr in
      (res, return @@ E_ascription ascr)
    )
  | E_matching m ->
    let res,m = Match_expr.fold_map self idle init m in
    (res, return @@ E_matching m)
  | E_record m -> (
    let (res, m') = Record.fold_map self init m in
    (res, return @@ E_record m')
  )
  | E_accessor acc -> (
      let (res, acc) = Types.Accessor.fold_map self init acc in
      (res, return @@ E_accessor acc)
    )
  | E_update u -> (
    let res,u = Types.Update.fold_map self init u in
    (res, return @@ E_update u)
  )
  | E_constructor c -> (
      let (res,c) = Constructor.fold_map self init c in
      (res, return @@ E_constructor c)
  )
  | E_application app -> (
      let res,app = Application.fold_map self init app in
      (res, return @@ E_application app)
    )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
      let (res,rhs) = self init rhs in
      let (res,let_result) = self res let_result in
      (res, return @@ E_let_in { let_binder ; rhs ; let_result ; attr })
    )
  | E_type_in ti -> (
      let res,ti = Type_in.fold_map self idle init ti in
      (res, return @@ E_type_in ti)
    )
  | E_mod_in mi -> (
      let res,mi = Mod_in.fold_map self idle init mi in
      (res, return @@ E_mod_in mi)
    )
  | E_lambda l -> (
      let res,l = Lambda.fold_map self idle init l in
      ( res, return @@ E_lambda l)
    )
  | E_type_abstraction ta -> (
      let res, ta = Type_abs.fold_map self init ta in
      res, return @@ E_type_abstraction ta
    )
  | E_recursive r ->
      let res,r = Recursive.fold_map self idle init r in
      ( res, return @@ E_recursive r)
  | E_constant c -> (
      let res,c = Constant.fold_map self init c in
      (res, return @@ E_constant c)
    )
  | E_assign a ->
    let (res,a) = Assign.fold_map self idle init a in
    (res, return @@ E_assign a)
  | E_literal _ | E_variable _ | E_raw_code _ | E_module_accessor _ as e' -> (init, return e')

and fold_map_expression_in_module_expr : type a . (a -> expression -> a * expression) -> a -> module_expr -> a * module_expr = fun self acc x ->
  let return r wrap_content : a * module_expr = (r, { x with wrap_content }) in
  match x.wrap_content with
  | M_struct decls ->
    let res,decls = List.fold_map
      ~f:( fun acc (Decl x: decl) ->
        let return r wrap_content : a * decl = (r, Decl { x with wrap_content }) in
        match x.wrap_content with
        | Declaration_constant x ->
          let res,expr = self acc x.expr in
          return res (Types.Declaration.Declaration_constant { x with expr })
        | Declaration_module x ->
          let res,module_ = fold_map_expression_in_module_expr self acc x.module_ in
          return res (Types.Declaration.Declaration_module { x with module_ })
        | Declaration_type _ -> return acc x.wrap_content
      )
      ~init:acc
      decls
    in
    return res (M_struct decls)
  | M_module_path _ as x -> return acc x
  | M_variable _ as x ->
    return acc x
