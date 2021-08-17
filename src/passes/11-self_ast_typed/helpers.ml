open Errors
open Ast_typed
open Trace
open Ast_typed.Helpers
open Stage_common

type ('a ,'err) decl_folder = 'a -> declaration -> 'a
type ('a ,'err) folder = 'a -> expression -> 'a
let rec fold_expression : ('a , 'err) folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let idle = fun acc _ -> acc in
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
  | E_lambda { binder = _ ; result = e }
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
  | E_type_in ti -> Folds.type_in self idle init ti
  | E_mod_in { module_binder = _ ; rhs ; let_result } -> (
      let res = fold_module f init rhs in
      let res = self res let_result in
      res
    )
  | E_mod_alias { alias = _ ; binders = _ ; result } -> (
      let res = self init result in
      res
    )
  | E_module_accessor { module_name = _ ; element } -> (
    let res = self init element in
    res
  )

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

and fold_module : ('a,'err) folder -> 'a -> module_fully_typed -> 'a = fun f init (Module_Fully_Typed p) ->
  let aux = fun acc (x : declaration Location.wrap) ->
    let return (d : 'a) = d in
    match Location.unwrap x with
    | Declaration_constant {binder=_; expr ; inline=_} -> (
        let res = fold_expression f acc expr in
        return @@ res
    )
    | Declaration_type _t -> return @@ acc
    | Declaration_module {module_binder=_;module_} ->
      let res = fold_module f acc module_ in
      return @@ res
    | Module_alias _ -> return @@ acc
  in
  let res = List.fold ~f:aux ~init p in
  res

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
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in { let_binder ; rhs ; let_result; inline }
  )
  | E_type_in ti -> (
    let ti = Maps.type_in self (fun x -> x) ti in
    return @@ E_type_in ti
  )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
    let rhs = map_module f rhs in
    let let_result = self let_result in
    return @@ E_mod_in { module_binder ; rhs ; let_result }
  )
  | E_mod_alias { alias ; binders ; result } -> (
    let result = self result in
    return @@ E_mod_alias { alias ; binders ; result }
  )
  | E_lambda { binder ; result } -> (
    let result = self result in
    return @@ E_lambda { binder ; result }
  )
  | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
    let result = self result in
    return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}
  )
  | E_constant c -> (
    let args = List.map ~f:self c.arguments in
    return @@ E_constant {c with arguments=args}
  )
  | E_module_accessor { module_name; element } -> (
    let element = self element in
    return @@ E_module_accessor { module_name; element }
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

and map_module : 'err mapper -> module_fully_typed -> module_fully_typed = fun m (Module_Fully_Typed p) ->
  let aux = fun (x : declaration) ->
    let return (d : declaration) = d in
    match x with
    | Declaration_constant {name; binder; expr ; inline} -> (
        let expr = map_expression m expr in
        return @@ Declaration_constant {name; binder; expr ; inline}
    )
    | Declaration_type t -> return @@ Declaration_type t
    | Declaration_module {module_binder;module_} ->
      let module_ = map_module m module_ in
      return @@ Declaration_module {module_binder; module_}
    | Module_alias _ -> return x
  in
  let p = List.map ~f:(Location.map aux) p in
  Module_Fully_Typed p

type ('a , 'err) fold_mapper = 'a -> expression -> bool * 'a * expression
let rec fold_map_expression : ('a , 'err) fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
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
  | E_let_in { let_binder ; rhs ; let_result; inline } -> (
      let (res,rhs) = self init rhs in
      let (res,let_result) = self res let_result in
      (res, return @@ E_let_in { let_binder ; rhs ; let_result ; inline })
    )
  | E_type_in { type_binder ; rhs ; let_result } -> (
      let (res,let_result) = self init let_result in
      (res, return @@ E_type_in { type_binder ; rhs ; let_result })
    )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
      let (res,let_result) = self init let_result in
      let (res,rhs) = fold_map_module f res rhs in
      (res, return @@ E_mod_in { module_binder ; rhs ; let_result })
    )
  | E_mod_alias { alias ; binders ; result } -> (
      let (res,result) = self init result in
      (res, return @@ E_mod_alias { alias ; binders ; result })
    )
  | E_lambda { binder ; result } -> (
      let (res,result) = self init result in
      ( res, return @@ E_lambda { binder ; result })
    )
  | E_recursive { fun_name; fun_type; lambda={binder;result}} -> (
      let (res,result) = self init result in
      (res, return @@ E_recursive {fun_name; fun_type; lambda={binder;result}})
    )
  | E_constant c -> (
      let (res,args) = List.fold_map ~f:self ~init c.arguments in
      (res, return @@ E_constant {c with arguments=args})
    )
  | E_module_accessor { module_name; element } -> (
    let (res,element) = self init element in
    (res, return @@ E_module_accessor { module_name; element })
  )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> (init, return e')

and fold_map_cases : ('a , 'err) fold_mapper -> 'a -> matching_expr -> 'a * matching_expr = fun f init m ->
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

and fold_map_module : ('a, 'err) fold_mapper -> 'a -> module_fully_typed -> 'a * module_fully_typed = fun m init (Module_Fully_Typed p) ->
  let aux = fun acc (x : declaration Location.wrap) ->
    match Location.unwrap x with
    | Declaration_constant {name; binder ; expr ; inline} -> (
      let (acc', expr) = fold_map_expression m acc expr in
      let wrap_content : declaration = Declaration_constant {name; binder ; expr ; inline} in
      (acc', {x with wrap_content})
    )
    | Declaration_type t -> (
      let wrap_content : declaration = Declaration_type t in
      (acc, {x with wrap_content})
    )
    | Declaration_module {module_binder; module_} -> (
      let (acc', module_) = fold_map_module m acc module_ in
      let wrap_content : declaration = Declaration_module {module_binder; module_} in
      (acc', {x with wrap_content})
    )
    | Module_alias _ -> (acc,x)
  in
  let (a,p) = List.fold_map ~f:aux ~init p in
  (a, Module_Fully_Typed p)

and fold_module_decl : ('a, 'err) folder -> ('a, 'err) decl_folder -> 'a -> module_fully_typed -> 'a = fun m m_decl init (Module_Fully_Typed p) ->
  let aux = fun acc (x : declaration Location.wrap) ->
      match Location.unwrap x with
      | Declaration_constant {binder=_ ; expr ; inline=_} as d ->
        let acc = m_decl acc d in
        fold_expression m acc expr
      | Declaration_type _t -> acc
      | Declaration_module _m -> acc
      | Module_alias _m -> acc
    in
    List.fold ~f:aux ~init p

type contract_type = {
  parameter : Ast_typed.type_expression ;
  storage : Ast_typed.type_expression ;
}

let fetch_contract_type ~raise : string -> module_fully_typed -> contract_type = fun main_fname (Module_Fully_Typed m) ->
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
  let main_decl =
    trace_option ~raise (corner_case ("Entrypoint '"^main_fname^"' does not exist")) @@
      main_decl_opt
    in
  let { binder=_ ; expr ; inline=_ } = main_decl in
  match expr.type_expression.type_content with
  | T_arrow {type1 ; type2} -> (
    match type1.type_content , type2.type_content with
    | T_record tin , T_record tout when (is_tuple_lmap tin.content) && (is_tuple_lmap tout.content) ->
       let (parameter,storage) = trace_option ~raise (expected_pair_in expr.location) @@ Ast_typed.Helpers.get_pair tin.content in
       let (listop,storage') = trace_option ~raise (expected_pair_out expr.location) @@ Ast_typed.Helpers.get_pair tout.content in
       let () = trace_option ~raise (expected_list_operation main_fname listop expr) @@
                       Ast_typed.assert_t_list_operation listop in
       let () = trace_option ~raise (expected_same main_fname storage storage' expr) @@
                       Ast_typed.assert_type_expression_eq (storage,storage') in
       (* TODO: on storage/parameter : a| Some (typed_prg,_,_) ->
        let b = extract_variable_types typed_prg in
        let () = Format.printf "\n EXTRACT \n" in
        let () = List.iter ~f: (fun (v,te) -> Format.printf "%a  --  %a\n" Ast_typed.PP.expression_variable v Ast_typed.PP.type_expression te) b in
        let () = Format.printf "length : %d\n" (List.length b) in
      ssert_storable, assert_passable ? *)
       { parameter ; storage }
    |  _ -> raise.raise @@ bad_contract_io main_fname expr
  )
  | _ -> raise.raise @@ bad_contract_io main_fname expr

let get_pattern ?(pred = fun _ -> true) pattern =
  Stage_common.Helpers.fold_pattern (fun vars p ->
      match p.wrap_content with
      | P_var {var;attributes} when pred attributes ->
         var :: vars
      | _ -> vars) [] pattern

module Free_variables :
  sig
    val expression : expression -> expression_variable list
  end
  = struct
  module Var = struct
    type t = expression_variable
    let compare e e' = Location.compare_content ~compare:Var.compare e e'
  end

  module VarSet = Set.Make(Var)

  let unions : VarSet.t list -> VarSet.t =
    fun l -> List.fold l ~init:VarSet.empty ~f:VarSet.union

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
    | E_lambda {binder ; result} ->
      VarSet.remove binder @@ self result
    | E_recursive {fun_name; lambda = {binder; result}} ->
      VarSet.remove fun_name @@ VarSet.remove binder @@ self result
    | E_constructor {element} ->
      self element
    | E_matching {matchee; cases} ->
      VarSet.union (self matchee) (get_fv_cases cases)
    | E_record m ->
      let res = LMap.map self m in
      let res = LMap.to_list res in
      unions res
    | E_record_update {record;update} ->
      VarSet.union (self record) (self update)
    | E_record_accessor {record} ->
      self record
    | E_let_in { let_binder ; rhs ; let_result } ->
      VarSet.union (self rhs) (VarSet.remove let_binder (self let_result))
    | E_type_in {let_result} ->
      self let_result
    | E_mod_in { module_binder = _ ; rhs ; let_result } ->
      VarSet.union (get_fv_module rhs) (self let_result)
    | E_mod_alias { alias = _ ; binders = _ ; result } ->
      self result
    | E_module_accessor { module_name = _ ; element } ->
      self element

  and get_fv_cases : matching_expr -> VarSet.t = fun m ->
    match m with
    | Match_variant {cases;tv=_} ->
      let aux {constructor=_; pattern ; body} =
        let res' = get_fv_expr body in
        VarSet.remove pattern @@ res' in
      unions @@  List.map ~f:aux cases
    | Match_record {fields; body; tv = _} ->
      let pattern = LMap.values fields |> List.map ~f:fst in
      let cur = get_fv_expr body in
      List.fold_right pattern ~f:VarSet.remove ~init:cur

  and get_fv_module : module_fully_typed -> VarSet.t = fun (Module_Fully_Typed p) ->
    let aux = fun (x : declaration Location.wrap) ->
      match Location.unwrap x with
      | Declaration_constant {binder=_; expr ; inline=_} ->
        get_fv_expr expr
      | Declaration_module {module_binder=_;module_} ->
        get_fv_module module_
      | Declaration_type _t ->
        VarSet.empty
      | Module_alias _ ->
        VarSet.empty
    in
    unions @@ List.map ~f:aux p

  let expression e = VarSet.fold (fun v r -> v :: r) (get_fv_expr e) []
end

module Free_module_variables :
  sig
    val expression : expression -> module_variable list
    val module' : module_fully_typed -> module_variable list
  end
  = struct
  module Var = struct
    type t = module_variable
    let compare e e' = compare_module_variable e e'
  end

  module VarSet = Set.Make(Var)

  let unions : VarSet.t list -> VarSet.t =
    fun l -> List.fold l ~init:VarSet.empty ~f:VarSet.union

  let rec get_fv_expr : expression -> VarSet.t = fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable _ | E_literal _ | E_raw_code _ ->
      VarSet.empty
    | E_constant {arguments} ->
      unions @@ List.map ~f:self arguments
    | E_application {lamb; args} ->
      VarSet.union (self lamb) (self args)
    | E_lambda {result} ->
      self result
    | E_recursive {lambda = {result}} ->
      self result
    | E_constructor {element} ->
      self element
    | E_matching {matchee; cases} ->
      VarSet.union (self matchee) (get_fv_cases cases)
    | E_record m ->
      let res = LMap.map self m in
      let res = LMap.to_list res in
      unions res
    | E_record_update {record;update} ->
      VarSet.union (self record) (self update)
    | E_record_accessor {record} ->
      self record
    | E_let_in { rhs ; let_result } ->
      VarSet.union (self rhs) (self let_result)
    | E_type_in {let_result} ->
      self let_result
    | E_mod_in { module_binder ; rhs ; let_result } ->
      VarSet.union (get_fv_module rhs) (VarSet.remove module_binder (self let_result))
    | E_mod_alias { alias = _ ; binders = _ ; result } ->
      self result
    | E_module_accessor { module_name ; element } ->
      VarSet.union (VarSet.singleton module_name) (self element)

  and get_fv_cases : matching_expr -> VarSet.t = fun m ->
    match m with
    | Match_variant {cases;tv=_} ->
      let aux {constructor = _; pattern = _ ; body} =
        get_fv_expr body in
      unions @@  List.map ~f:aux cases
    | Match_record {fields = _; body; tv = _} ->
      get_fv_expr body

  and get_fv_module : module_fully_typed -> VarSet.t = fun (Module_Fully_Typed p) ->
    let aux = fun (x : declaration Location.wrap) ->
      match Location.unwrap x with
      | Declaration_constant {binder=_; expr ; inline=_} ->
        get_fv_expr expr
      | Declaration_module {module_binder=_;module_} ->
        get_fv_module module_
      | Declaration_type _t ->
        VarSet.empty
      | Module_alias _ ->
        VarSet.empty
    in
    unions @@ List.map ~f:aux p

  let expression e = VarSet.fold (fun v r -> v :: r) (get_fv_expr e) []
  let module' m = VarSet.fold (fun v r -> v :: r) (get_fv_module m) []
end
