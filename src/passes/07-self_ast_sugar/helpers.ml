module Pair = Simple_utils.Pair
open Ast_sugar
open Ligo_prim

type ('a , 'err) folder = 'a -> expression -> 'a
let rec fold_expression : ('a, 'err) folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let self_type = Fun.const in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip _ | E_module_accessor _ -> init
  | E_list lst | E_set lst  -> (
    let res = List.fold ~f:self ~init lst in
    res
  )
  | E_map lst | E_big_map lst -> (
    let res = List.fold ~f:(fun init -> Pair.fold ~f:self ~init) ~init lst in
    res
  )
  | E_constant c -> Constant.fold self init c
  | E_application app -> Application.fold self init app
  | E_lambda l -> Lambda.fold self self_type init l
  | E_type_abstraction ta -> Type_abs.fold self init ta
  | E_ascription a -> Ascription.fold self self_type init a
  | E_constructor c -> Constructor.fold self init c
  | E_matching  m -> Match_expr.fold self self_type init m
  | E_record    m -> Record.fold self init m
  | E_update    u -> Types.Update.fold self init u
  | E_accessor  a -> Types.Accessor.fold self init a
  | E_tuple     t -> List.fold ~f:self ~init t
  | E_let_in   li -> Let_in.fold self self_type init li
  | E_type_in  ti -> Type_in.fold self self_type init ti
  | E_mod_in   mi -> Mod_in.fold self self_type init mi
  | E_cond      c -> Conditional.fold self init c
  | E_recursive r -> Recursive.fold self self_type init r
  | E_sequence  s -> Sequence.fold self init s
  | E_assign    a -> Assign.fold self self_type init a

type exp_mapper = expression -> expression
type ty_exp_mapper = type_expression -> type_expression
type abs_mapper =
  | Expression of exp_mapper
  | Type_expression of ty_exp_mapper
let rec map_expression : exp_mapper -> expression -> expression = fun f e ->
  let self = map_expression f in
  let self_type = Fun.id in
  let e' = f e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let lst' = List.map ~f:self lst in
    return @@ E_list lst'
  )
  | E_set lst -> (
    let lst' = List.map ~f:self lst in
    return @@ E_set lst'
  )
  | E_map lst -> (
    let lst' = List.map ~f:(Pair.map ~f:self) lst in
    return @@ E_map lst'
  )
  | E_big_map lst -> (
    let lst' = List.map ~f:(Pair.map ~f:self) lst in
    return @@ E_big_map lst'
  )
  | E_ascription ascr -> (
      let ascr = Ascription.map self (fun a -> a) ascr in
      return @@ E_ascription ascr
  )
  | E_matching match_expr ->
    let match_expr = Match_expr.map self (fun a -> a) match_expr in
    return @@ E_matching match_expr
  | E_record m -> (
    let m' = Record.map self m in
    return @@ E_record m'
  )
  | E_accessor acc -> (
      let acc = Types.Accessor.map self acc in
      return @@ E_accessor acc
    )
  | E_update u -> (
    let u = Types.Update.map self u in
    return @@ E_update u
  )
  | E_constructor c -> (
      let c = Constructor.map self c in
      return @@ E_constructor c
  )
  | E_application app -> (
    let app = Application.map self app in
    return @@ E_application app
  )
  | E_let_in li -> (
      let li = Let_in.map self (fun a -> a) li in
      return @@ E_let_in li
    )
  | E_type_in ti -> (
      let ti = Type_in.map self self_type ti in
      return @@ E_type_in ti
    )
  | E_mod_in mi -> (
      let mi = Mod_in.map self self_type mi in
      return @@ E_mod_in mi
    )
  | E_lambda l -> (
      let l = Lambda.map self self_type l in
      return @@ E_lambda l
    )
  | E_type_abstraction ta -> (
      let ta = Type_abs.map self ta in
      return @@ E_type_abstraction ta
    )
  | E_recursive r ->
      let r = Recursive.map self self_type r in
      return @@ E_recursive r
  | E_constant c -> (
      let c = Constant.map self c in
      return @@ E_constant c
    )
  | E_cond c ->
      let c = Conditional.map self c in
      return @@ E_cond c
  | E_sequence s -> (
      let s = Sequence.map self s in
      return @@ E_sequence s
    )
  | E_tuple t -> (
    let t' = List.map ~f:self t in
    return @@ E_tuple t'
  )
  | E_assign a ->
    let a = Assign.map self (fun a -> a) a in
    return @@ E_assign a
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip _ | E_module_accessor _ as e' -> return e'

and map_type_expression : ty_exp_mapper -> type_expression -> type_expression = fun f te ->
  let self = map_type_expression f in
  let te' = f te in
  let return type_content = { type_content; location=te.location } in
  match te'.type_content with
  | T_sum temap ->
    let temap' = Rows.map self temap in
    return @@ T_sum temap'
  | T_record temap ->
    let temap' = Rows.map self temap in
    return @@ T_record temap'
  | T_tuple telst ->
    let telst' = List.map ~f:self telst in
    return @@ (T_tuple telst')
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

and map_module_expr : abs_mapper -> module_expr -> module_expr = fun f m ->
  let return wrap_content : module_expr = { m with wrap_content } in
  match m.wrap_content with
  | M_struct decls ->
    let decls = (map_module f) decls in
    return (M_struct decls)
  | M_module_path _ -> m
  | M_variable _ -> m

and map_declaration m : declaration -> declaration = fun d ->
  let aux : declaration_content ->declaration_content = fun x ->
    match x,m with
    | (D_value dc, Expression m') -> (
        let dc = Types.ValueDecl.map (map_expression m') Fun.id dc in
        (D_value dc)
      )
    | (D_type dt, Type_expression m') -> (
        let dt = Types.TypeDecl.map (map_type_expression m') dt in
        (D_type dt)
      )
    | decl,_ -> decl
  (* | Declaration_type of (type_variable * type_expression) *)
  in
  Location.map aux d

and map_decl m = fun (Decl d) ->
  let d = map_declaration m d in
  (Decl d)
and map_module : abs_mapper -> module_ -> module_ = fun m p ->
  List.map ~f:(map_decl m) p

let map_program : abs_mapper -> program -> program = fun m p ->
  List.map ~f:(map_declaration m) p

type ('a, 'err) fold_mapper = 'a -> expression -> bool * 'a * expression
let rec fold_map_expression : ('a, 'err) fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = (acc,a) in
  let (continue, init,e') = f a e in
  if (not continue) then (init,e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let (res, lst') = List.fold_map ~f:self ~init:init lst in
    (res, return @@ E_list lst')
  )
  | E_set lst -> (
    let (res, lst') = List.fold_map ~f:self ~init:init lst in
    (res, return @@ E_set lst')
  )
  | E_map lst -> (
    let (res, lst') = List.fold_map ~f:(fun init -> Pair.fold_map ~f:self ~init) ~init:init lst in
    (res, return @@ E_map lst')
  )
  | E_big_map lst -> (
    let (res, lst') = List.fold_map ~f:(fun init -> Pair.fold_map ~f:self ~init) ~init:init lst in
    (res, return @@ E_big_map lst')
  )
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
  | E_tuple t -> (
    let (res, t') = List.fold_map ~f:self ~init:init t in
    (res, return @@ E_tuple t')
  )
  | E_constructor c -> (
      let (res,c) = Constructor.fold_map self init c in
      (res, return @@ E_constructor c)
  )
  | E_application app -> (
      let res,app = Application.fold_map self init app in
      (res, return @@ E_application app)
    )
  | E_let_in li -> (
      let res,li = Let_in.fold_map self idle init li in
      (res, return @@ E_let_in li)
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
  | E_cond c ->
      let res,c = Conditional.fold_map self init c in
      (res, return @@ E_cond c)
  | E_sequence s -> (
      let res,s = Sequence.fold_map self init s in
      (res, return @@ E_sequence s)
    )
  | E_assign a ->
    let (res,a) = Assign.fold_map self idle init a in
    (res, return @@ E_assign a)
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip _ | E_module_accessor _ as e' -> (init, return e')
