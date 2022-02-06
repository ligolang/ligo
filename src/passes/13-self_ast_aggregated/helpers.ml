open Ast_aggregated
open Stage_common

type ('a ,'err) folder = 'a -> expression -> 'a
let rec fold_expression : ('a , 'err) folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ -> init
  | E_constant {arguments=lst} -> (
    let res = List.fold ~f:self ~init lst in
    res
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let res = Pair.fold ~f:self ~init ab in
    res
  )
  | E_type_inst { forall = e; type_ = _}
  | E_lambda { binder = _ ; result = e }
  | E_type_abstraction { type_binder = _ ; result = e}
  | E_recursive {lambda= {result=e}}
  | E_constructor {element=e} -> (
    let res = self init e in
    res
  )
  | E_matching {matchee=e; cases} -> (
    let res = self init e in
    let res = fold_cases f res cases in
    res
  )
  | E_record m -> (
    let aux _ expr init =
      let res = fold_expression self init expr in
      res
    in
    let res = LMap.fold aux m init in
    res
  )
  | E_record_update {record;update} -> (
    let res = self init record in
    let res = fold_expression self res update in
    res
  )
  | E_record_accessor {record} -> (
    let res = self init record in
    res
  )
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let res = self init rhs in
      let res = self res let_result in
      res
    )
  | E_type_in { type_binder=_; rhs = _ ; let_result} ->
    let res = self init let_result in
    res

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

type ty_mapper = type_expression -> unit
let rec iter_type_expression : ty_mapper -> type_expression -> unit = fun f t ->
  let self = iter_type_expression f in
  let () = f t in
  match t.type_content with
  | T_variable _ -> ()
  | T_constant x -> List.iter ~f:self x.parameters
  | T_sum x -> List.iter ~f:(fun x -> self x.associated_type) (LMap.to_list x.content)
  | T_record x -> List.iter ~f:(fun x -> self x.associated_type) (LMap.to_list x.content)
  | T_arrow x ->
    let () = self x.type1 in
    self x.type2
  | T_singleton _ -> ()
  | T_abstraction x -> self x.type_
  | T_for_all x -> self x.type_

type 'err mapper = expression -> expression
let rec map_expression : 'err mapper -> expression -> expression = fun f e ->
  let self = map_expression f in
  let e' = f e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
    let e' = self e in
    let cases' = map_cases f cases in
    return @@ E_matching {matchee=e';cases=cases'}
  )
  | E_record_accessor {record; path} -> (
    let record = self record in
    return @@ E_record_accessor {record; path}
  )
  | E_record m -> (
    let m' = LMap.map self m in
    return @@ E_record m'
  )
  | E_record_update {record; path; update} -> (
    let record = self record in
    let update = self update in
    return @@ E_record_update {record;path;update}
  )
  | E_constructor c -> (
    let e' = self c.element in
    return @@ E_constructor {c with element = e'}
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
  | E_type_in {type_binder; rhs; let_result} -> (
    let let_result = self let_result in
    return @@ E_type_in {type_binder; rhs; let_result}
  )
  | E_lambda { binder ; result } -> (
    let result = self result in
    return @@ E_lambda { binder ; result }
  )
  | E_type_abstraction ta -> (
      let ta = Maps.type_abs self ta in
      return @@ E_type_abstraction ta
  )
  | E_type_inst { forall ; type_ } -> (
    let forall = self forall in
    return @@ E_type_inst { forall ; type_ }
  )
  | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
    let result = self result in
    return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}
  )
  | E_constant c -> (
    let args = List.map ~f:self c.arguments in
    return @@ E_constant {c with arguments=args}
  )
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

type 'a fold_mapper = 'a -> expression -> bool * 'a * expression
let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
  let self = fold_map_expression f in
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
  | E_record_accessor {record; path} -> (
      let (res, record) = self init record in
      (res, return @@ E_record_accessor {record; path})
    )
  | E_record m -> (
    let (res,m') = LMap.fold_map ~f:(fun _ e res -> self res e) ~init m in
    (res, return @@ E_record m')
  )
  | E_record_update {record; path; update} -> (
    let (res, record) = self init record in
    let (res, update) = self res update in
    (res, return @@ E_record_update {record;path;update})
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
  | E_type_in { type_binder ; rhs ; let_result} -> (
      let (res,let_result) = self init let_result in
      (res, return @@ E_type_in { type_binder ; rhs ; let_result })
    )
  | E_type_inst { forall ; type_ } -> (
    let (res, forall) = self init forall in
    ( res, return @@ E_type_inst { forall ; type_ })
  )
  | E_lambda { binder ; result } -> (
      let (res,result) = self init result in
      ( res, return @@ E_lambda { binder ; result })
    )
  | E_type_abstraction ta -> (
      let res, ta = Fold_maps.type_abs self init ta in
      res, return @@ E_type_abstraction ta
    )
  | E_recursive { fun_name; fun_type; lambda={binder;result}} -> (
      let (res,result) = self init result in
      (res, return @@ E_recursive {fun_name; fun_type; lambda={binder;result}})
    )
  | E_constant c -> (
      let (res,args) = List.fold_map ~f:self ~init c.arguments in
      (res, return @@ E_constant {c with arguments=args})
    )
  | E_raw_code {language;code} -> (
    let (res,code) = self init code in
    (res, return @@ E_raw_code { language ; code }))
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

let get_pattern ?(pred = fun _ -> true) pattern =
  Stage_common.Helpers.fold_pattern (fun vars p ->
      match p.wrap_content with
      | P_var {var;attributes} when pred attributes ->
         var :: vars
      | _ -> vars) [] pattern

module Free_variables :
  sig
    val expression : expression -> (module_variable list * expression_variable list)
  end
  = struct

  module VarSet = Caml.Set.Make(ValueVar)
  module ModVarSet = Caml.Set.Make(ModuleVar)
  type moduleEnv' = {modVarSet : ModVarSet.t; moduleEnv: moduleEnv; varSet: VarSet.t}
  and moduleEnv = moduleEnv' SMap.t

  let rec merge =fun {modVarSet=x1;moduleEnv=y1;varSet=z1} {modVarSet=x2;moduleEnv=y2;varSet=z2} ->
    let aux : string -> moduleEnv' -> moduleEnv' -> moduleEnv' option =
      fun _ a b -> Some (merge a b)
    in
      {modVarSet=ModVarSet.union x1 x2;moduleEnv=SMap.union aux y1 y2;varSet=VarSet.union z1 z2}

  let unions : moduleEnv' list -> moduleEnv' =
    fun l -> List.fold l ~init:{modVarSet=ModVarSet.empty;moduleEnv=SMap.empty;varSet=VarSet.empty}
    ~f:merge
  let rec get_fv_expr : expression -> moduleEnv' = fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v ->
      {modVarSet=ModVarSet.empty; moduleEnv=SMap.empty ;varSet=VarSet.singleton v}
    | E_literal _ | E_raw_code _ ->
      {modVarSet=ModVarSet.empty;moduleEnv=SMap.empty;varSet=VarSet.empty}
    | E_constant {arguments} ->
      unions @@ List.map ~f:self arguments
    | E_application {lamb; args} ->
      merge (self lamb) (self args)
    | E_type_inst {forall} ->
      self forall
    | E_lambda {binder ; result} ->
      let {modVarSet=fmv;moduleEnv;varSet=fv} = self result in
      {modVarSet=fmv;moduleEnv;varSet=VarSet.remove binder @@ fv}
    | E_type_abstraction {type_binder=_ ; result} ->
      self result
    | E_recursive {fun_name; lambda = {binder; result}} ->
      let {modVarSet;moduleEnv;varSet=fv} = self result in
      {modVarSet;moduleEnv;varSet=VarSet.remove fun_name @@ VarSet.remove binder @@ fv}
    | E_constructor {element} ->
      self element
    | E_matching {matchee; cases} ->
      merge (self matchee)(get_fv_cases cases)
    | E_record m ->
      let res = LMap.map self m in
      let res = LMap.to_list res in
      unions res
    | E_record_update {record;update} ->
      merge (self record) (self update)
    | E_record_accessor {record} ->
      self record
    | E_let_in { let_binder ; rhs ; let_result } ->
      let {modVarSet;moduleEnv;varSet=fv2} = (self let_result) in
      let fv2 = VarSet.remove let_binder fv2 in
      merge (self rhs) {modVarSet;moduleEnv;varSet=fv2}
    | E_type_in {let_result} ->
      self let_result

  and get_fv_cases : matching_expr -> moduleEnv' = fun m ->
    match m with
    | Match_variant {cases;tv=_} ->
      let aux {constructor=_; pattern ; body} =
        let {modVarSet;moduleEnv;varSet} = get_fv_expr body in
        {modVarSet;moduleEnv;varSet=VarSet.remove pattern @@ varSet} in
      unions @@  List.map ~f:aux cases
    | Match_record {fields; body; tv = _} ->
      let pattern = LMap.values fields |> List.map ~f:fst in
      let {modVarSet;moduleEnv;varSet} = get_fv_expr body in
      {modVarSet;moduleEnv;varSet=List.fold_right pattern ~f:VarSet.remove ~init:varSet}

  let expression e =
    let {modVarSet;moduleEnv=_;varSet} = get_fv_expr e in
    let fmv = ModVarSet.fold (fun v r -> v :: r) modVarSet [] in
    let fv = VarSet.fold (fun v r -> v :: r) varSet [] in
    (fmv, fv)
end
