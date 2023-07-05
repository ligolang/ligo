open Ligo_prim
open Ast_aggregated

type ('a, 'err) folder = 'a -> expression -> 'a

let rec fold_expression : ('a, 'err) folder -> 'a -> expression -> 'a =
 fun f init e ->
  let self = fold_expression f in
  let self_type = Fun.const in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ -> init
  | E_raw_code { language = _; code } -> self init code
  | E_constant c -> Constant.fold self init c
  | E_application app -> Application.fold self init app
  | E_lambda l -> Lambda.fold self self_type init l
  | E_type_abstraction ta -> Type_abs.fold self init ta
  | E_constructor c -> Constructor.fold self init c
  | E_matching { matchee = e; cases } ->
    let res = self init e in
    let res = fold_cases f res cases in
    res
  | E_record m -> Record.fold ~f:self ~init m
  | E_update u -> Types.Update.fold self init u
  | E_accessor a -> Types.Accessor.fold self init a
  | E_let_in { let_binder = _; rhs; let_result } ->
    let res = self init rhs in
    let res = self res let_result in
    res
  | E_recursive r -> Recursive.fold self self_type init r
  | E_type_inst { forall = e; type_ = _ } ->
    let res = self init e in
    res
  | E_let_mut_in { let_binder = _; rhs; let_result; attributes = _ } ->
    let res = self init rhs in
    let res = self res let_result in
    res
  | E_deref _ -> init
  | E_assign a -> Assign.fold self self_type init a
  | E_coerce a -> Ascription.fold self self_type init a
  | E_for f -> For_loop.fold self init f
  | E_for_each fe -> For_each_loop.fold self init fe
  | E_while w -> While_loop.fold self init w


and fold_cases
    :  ('a, 'err) folder -> 'a -> (expression, type_expression) Match_expr.match_case list
    -> 'a
  =
 fun f init m ->
  List.fold m ~init ~f:(fun init { body; _ } -> fold_expression f init body)
