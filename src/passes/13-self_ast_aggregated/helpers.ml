open Ast_aggregated
open Ligo_prim

type ('a ,'err) folder = 'a -> expression -> 'a
let rec fold_expression : ('a , 'err) folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let self_type = Fun.const in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ -> init
  | E_constant c -> Constant.fold self init c
  | E_application app -> Application.fold self init app
  | E_lambda l -> Lambda.fold self self_type init l
  | E_type_abstraction ta -> Type_abs.fold self init ta
  | E_constructor c -> Constructor.fold self init c
  | E_matching {matchee=e; cases} -> (
    let res = self init e in
    let res = fold_cases f res cases in
    res
  )
  | E_record    m -> Record.fold self init m
  | E_update    u -> Types.Update.fold self init u
  | E_accessor  a -> Types.Accessor.fold self init a
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let res = self init rhs in
      let res = self res let_result in
      res
    )
  | E_recursive r -> Recursive.fold self self_type init r
  | E_type_inst { forall = e; type_ = _} ->
    let res = self init e in
    res
  | E_let_mut_in { let_binder = _ ; rhs ; let_result ; attr=_} -> (
    let res = self init rhs in
    let res = self res let_result in
    res
  )
  | E_deref _ -> init
  | E_assign a -> Assign.fold self self_type init a
  | E_for f -> For_loop.fold self init f
  | E_for_each fe -> For_each_loop.fold self init fe
  | E_while w -> While_loop.fold self init w

and fold_cases : ('a , 'err) folder -> 'a -> matching_expr -> 'a = fun f init m ->
  match m with
  | Match_variant {cases;tv=_} -> (
      let aux init' {constructor=_; pattern=_ ; body} =
        let res' = fold_expression f init' body in
        res' in
      let res = List.fold ~f:aux ~init cases in
      res
    )
  | Match_record {fields = _; body; tv = _} ->
    fold_expression f init body

type 'err mapper = expression -> expression
let rec map_expression : 'err mapper -> expression -> expression = fun f e ->
  let self = map_expression f in
  let self_type = Fun.id in
  let e' = f e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
    let e' = self e in
    let cases' = map_cases f cases in
    return @@ E_matching {matchee=e';cases=cases'}
  )
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
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let (a,b) = Pair.map ~f:self ab in
    return @@ E_application {lamb=a;args=b}
  )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in { let_binder ; rhs ; let_result; attr }
  )
  | E_lambda l -> (
      let l = Lambda.map self self_type l in
      return @@ E_lambda l
    )
  | E_type_abstraction ta -> (
      let ta = Type_abs.map self ta in
      return @@ E_type_abstraction ta
    )
  | E_type_inst { forall ; type_ } -> (
    let forall = self forall in
    return @@ E_type_inst { forall ; type_ }
  )
  | E_recursive r ->
      let r = Recursive.map self self_type r in
      return @@ E_recursive r
  | E_constant c -> (
    let args = List.map ~f:self c.arguments in
    return @@ E_constant {c with arguments=args}
  )
  | E_assign a ->
    let a = Assign.map self (fun a -> a) a in
    return @@ E_assign a
  | E_for f ->
    let f = For_loop.map self f in
    return @@ E_for f
  | E_for_each fe ->
    let fe = For_each_loop.map self fe in
    return @@ E_for_each fe
  | E_while w ->
    let w = While_loop.map self w in
    return @@ E_while w
  | E_let_mut_in { let_binder; rhs; let_result; attr } ->
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_mut_in { let_binder; rhs; let_result; attr }
  | E_deref _
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'


and map_cases : 'err mapper -> matching_expr -> matching_expr = fun f m ->
  match m with
  | Match_variant {cases;tv} -> (
      let aux { constructor ; pattern ; body } =
        let body = map_expression f body in
        {constructor;pattern;body}
      in
      let cases = List.map ~f:aux cases in
      Match_variant {cases ; tv}
    )
  | Match_record {fields; body; tv} ->
    let body = map_expression f body in
    Match_record {fields; body; tv}

and map_program : 'err mapper -> program -> program = fun g (ctxt, expr) ->
  let f d = Location.map (function D_value { binder ; expr ; attr } -> D_value { binder ; expr = map_expression g expr ; attr }) d in
  let ctxt = List.map ~f ctxt in
  let expr = map_expression g expr in
  (ctxt, expr)

type 'a fold_mapper = 'a -> expression -> bool * 'a * expression
let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = (acc,a) in
  let (continue, init,e') = f a e in
  if (not continue) then (init,e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
      let (res, e') = self init e in
      let (res,cases') = fold_map_cases f res cases in
      (res, return @@ E_matching {matchee=e';cases=cases'})
    )
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
      let (res,e') = self init c.element in
      (res, return @@ E_constructor {c with element = e'})
  )
  | E_application {lamb;args} -> (
      let ab = (lamb, args) in
      let (res,(a,b)) = Pair.fold_map ~f:self ~init ab in
      (res, return @@ E_application {lamb=a;args=b})
    )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
      let (res,rhs) = self init rhs in
      let (res,let_result) = self res let_result in
      (res, return @@ E_let_in { let_binder ; rhs ; let_result ; attr })
    )
  | E_type_inst { forall ; type_ } -> (
    let (res, forall) = self init forall in
    ( res, return @@ E_type_inst { forall ; type_ })
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
  | E_raw_code {language;code} -> (
    let (res,code) = self init code in
    (res, return @@ E_raw_code { language ; code }))
  | E_assign a ->
    let res, a = Assign.fold_map self idle init a in
    res, return @@ E_assign a
  | E_for f ->
    let res, f = For_loop.fold_map self init f in
    res, return @@ E_for f
  | E_for_each fe ->
    let res, fe = For_each_loop.fold_map self init fe in
    res, return @@ E_for_each fe
  | E_while w ->
    let res, w = While_loop.fold_map self init w in
    res, return @@ E_while w
  | E_let_mut_in { let_binder; rhs; let_result; attr } ->
    let res, rhs = self init rhs in
    let res, let_result = self res let_result in
    res, return @@ E_let_mut_in { let_binder; rhs; let_result; attr }
  | E_deref _
  | E_literal _ | E_variable _ as e' -> (init, return e')

and fold_map_cases : 'a fold_mapper -> 'a -> matching_expr -> 'a * matching_expr = fun f init m ->
  match m with
  | Match_variant {cases ; tv} -> (
      let aux init {constructor ; pattern ; body} =
        let (init, body) = fold_map_expression f init body in
        (init, {constructor; pattern ; body})
      in
      let (init,cases) = List.fold_map ~f:aux ~init cases in
      (init, Match_variant {cases ; tv})
    )
  | Match_record { fields; body; tv } ->
      let (init, body) = fold_map_expression f init body in
      (init, Match_record { fields ; body ; tv })

module Free_variables :
  sig
    val expression : expression -> Value_var.t list
  end
  = struct

  module VarSet = Caml.Set.Make(Value_var)
  let unions : VarSet.t list -> VarSet.t =
    fun l -> List.fold l ~init:VarSet.empty
    ~f:VarSet.union
  let rec get_fv_expr : expression -> VarSet.t = fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v ->
      VarSet.singleton v
    | E_literal _ | E_raw_code _ ->
      VarSet.empty
    | E_constant {arguments} ->
      unions @@ List.map ~f:self arguments
    | E_application {lamb; args} ->
      VarSet.union (self lamb) (self args)
    | E_type_inst {forall} ->
      self forall
    | E_lambda {binder ; result} ->
      let fv = self result in
      VarSet.remove (Param.get_var binder) @@ fv
    | E_type_abstraction {type_binder=_ ; result} ->
      self result
    | E_recursive {fun_name; lambda = {binder; result}} ->
      let fv = self result in
      VarSet.remove fun_name @@ VarSet.remove (Param.get_var binder) @@ fv
    | E_constructor {element} ->
      self element
    | E_matching {matchee; cases} ->
      VarSet.union (self matchee)(get_fv_cases cases)
    | E_record m ->
      let res = Record.map self m in
      let res = Record.LMap.to_list res in
      unions res
    | E_accessor {struct_} ->
      self struct_
    | E_update {struct_;update} ->
      VarSet.union (self struct_) (self update)
    | E_let_in { let_binder ; rhs ; let_result } ->
      let fv2 = (self let_result) in
      let fv2 = VarSet.remove (Binder.get_var let_binder) fv2 in
      VarSet.union (self rhs) fv2
    (* HACK? return free mutable variables too, without distinguishing
       them from free immutable variables *)
    | E_let_mut_in { let_binder; rhs; let_result } ->
      let fv2 = (self let_result) in
      let fv2 = VarSet.remove (Binder.get_var let_binder) fv2 in
      VarSet.union (self rhs) fv2
    | E_assign {binder; expression} ->
      VarSet.union (VarSet.singleton (Binder.get_var binder)) (self expression)
    | E_deref v -> VarSet.singleton v
    | E_for { binder; start; final; incr; f_body } ->
      unions
        [ self start
        ; self final
        ; self incr
        ; VarSet.remove binder (self f_body)
        ]
    | E_for_each
        { fe_binder = binder, None; collection; fe_body; collection_type = _ }
      -> unions [ self collection; VarSet.remove binder (self fe_body) ]
    | E_for_each { fe_binder = binder1, Some binder2; collection; fe_body; _ } ->
      unions
        [ self collection
        ; VarSet.remove binder1 @@ VarSet.remove binder2 @@ self fe_body
        ]
    | E_while { cond; body } ->
      VarSet.union (self cond) (self body)


  and get_fv_cases : matching_expr -> VarSet.t = fun m ->
    match m with
    | Match_variant {cases;tv=_} ->
      let aux {constructor=_; pattern ; body} =
        let varSet = get_fv_expr body in
        VarSet.remove pattern @@ varSet in
      unions @@  List.map ~f:aux cases
    | Match_record {fields; body; tv = _} ->
      let pattern = Record.LMap.values fields |> List.map ~f:Binder.get_var in
      let varSet = get_fv_expr body in
      List.fold_right pattern ~f:VarSet.remove ~init:varSet

  let expression e =
    let varSet = get_fv_expr e in
    let fv = VarSet.fold (fun v r -> v :: r) varSet [] in
    fv
end
