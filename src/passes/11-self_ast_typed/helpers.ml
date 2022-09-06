open Errors
open Ligo_prim
open Ast_typed
open Simple_utils.Trace
open Ast_typed.Helpers
module Pair = Simple_utils.Pair

type 'a decl_folder = 'a -> declaration -> 'a
type 'a folder = 'a -> expression -> 'a
let rec fold_expression : 'a folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let self_type = Fun.const in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ | E_module_accessor _ -> init
  | E_constant {arguments=lst;cons_name=_} -> (
    let res = List.fold ~f:self ~init lst in
    res
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let res = Pair.fold ~f:self ~init ab in
    res
  )
  | E_type_inst { forall = e; type_ = _}
  | E_lambda { binder = _ ; output_type = _; result = e }
  | E_type_abstraction { type_binder = _ ; result = e}
  | E_recursive {lambda= {result=e;output_type=_;binder=_};fun_name=_;fun_type=_}
  | E_constructor {element=e;constructor=_} -> (
    let res = self init e in
    res
  )
  | E_matching {matchee=e; cases} -> (
    let res = self init e in
    let res = fold_cases f res cases in
    res
  )
  | E_record m -> (
    let res = Record.fold self init m in
    res
  )
  | E_update {record;update;path=_} -> (
    let res = self init record in
    let res = fold_expression self res update in
    res
  )
  | E_accessor {record;path=_} -> (
    let res = self init record in
    res
  )
  | E_let_in { let_binder = _ ; rhs ; let_result ; attr=_} -> (
      let res = self init rhs in
      let res = self res let_result in
      res
    )
  | E_mod_in { module_binder = _ ; rhs ; let_result } -> (
    let res = fold_expression_in_module_expr self init rhs in
    let res = self res let_result in
    res
  )
  | E_assign a -> Assign.fold self self_type init a

and fold_expression_in_module_expr : ('a -> expression -> 'a)  -> 'a -> module_expr -> 'a = fun self acc x ->
  match x.wrap_content with
  | M_struct decls ->
    List.fold
      ~f:( fun acc x ->
        match x.wrap_content with
        | D_value  x -> self acc x.expr
        | D_module x -> fold_expression_in_module_expr self acc x.module_
        | D_type   _ ->  acc
      )
      ~init:acc
      decls
  | M_module_path _ -> acc
  | M_variable _ -> acc

and fold_cases : 'a folder -> 'a -> matching_expr -> 'a = fun f init m ->
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

and fold_module : 'a folder -> 'a -> module_ -> 'a = fun f init m ->
  let aux = fun acc x ->
    let return (d : 'a) = d in
    match Location.unwrap x with
    | D_value {binder=_; expr ; attr = { inline=_ ; no_mutation = _ ; view = _ ;public = _ ; hidden = _ ; thunk = _ }} -> (
        let res = fold_expression f acc expr in
        return @@ res
    )
    | D_type _t -> return @@ acc
    | D_module {module_binder=_;module_ ; module_attr=_} ->
      let res = fold_expression_in_module_expr f acc module_ in
      return @@ res
  in
  let res = List.fold ~f:aux ~init m in
  res

type ty_mapper = type_expression -> unit
let rec iter_type_expression : ty_mapper -> type_expression -> unit = fun f t ->
  let self = iter_type_expression f in
  let () = f t in
  match t.type_content with
  | T_variable _ -> ()
  | T_constant x -> List.iter ~f:self x.parameters
  | T_sum x -> List.iter ~f:(fun x -> self x.associated_type) (Record.LMap.to_list x.fields)
  | T_record x -> List.iter ~f:(fun x -> self x.associated_type) (Record.LMap.to_list x.fields)
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
  | E_accessor {record; path} -> (
    let record = self record in
    return @@ E_accessor {record; path}
  )
  | E_record m -> (
    let m' = Record.map self m in
    return @@ E_record m'
  )
  | E_update {record; path; update} -> (
    let record = self record in
    let update = self update in
    return @@ E_update {record;path;update}
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
  | E_mod_in { module_binder ; rhs ; let_result } -> (
    let rhs = map_expression_in_module_expr f rhs in
    let let_result = self let_result in
    return @@ E_mod_in { module_binder ; rhs ; let_result }
  )
  | E_lambda { binder ; output_type ; result } -> (
    let result = self result in
    return @@ E_lambda { binder ; output_type ; result }
  )
  | E_type_abstraction ta -> (
      let ta = Type_abs.map self ta in
      return @@ E_type_abstraction ta
  )
  | E_type_inst { forall ; type_ } -> (
    let forall = self forall in
    return @@ E_type_inst { forall ; type_ }
  )
  | E_recursive { fun_name; fun_type; lambda = {binder;output_type;result}} -> (
    let result = self result in
    return @@ E_recursive { fun_name; fun_type; lambda = {binder;output_type;result}}
  )
  | E_constant c -> (
    let args = List.map ~f:self c.arguments in
    return @@ E_constant {c with arguments=args}
  )
  | E_assign a ->
    let a = Assign.map self (fun a -> a) a in
    return @@ E_assign a
  | E_module_accessor ma-> return @@ E_module_accessor ma
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'

and map_expression_in_module_expr : (expression -> expression) -> module_expr -> module_expr = fun self x ->
  let return wrap_content : module_expr = { x with wrap_content } in
  match x.wrap_content with
  | M_struct decls ->
    let decls = map_module self decls in
    return (M_struct decls)
  | M_module_path _ -> x
  | M_variable _ -> x

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

and map_declaration m = fun (x : declaration) ->
  let return (d : declaration_content) = { x with wrap_content=d} in
  match x.wrap_content with
  | D_value {binder; expr ; attr} -> (
      let expr = map_expression m expr in
      return @@ D_value {binder; expr ; attr}
  )
  | D_type t -> return @@ D_type t
  | D_module {module_binder;module_;module_attr} ->
    let module_ = map_expression_in_module_expr m module_ in
    return @@ D_module {module_binder; module_; module_attr}

and map_decl m d = map_declaration m d
and map_module : 'err mapper -> module_ -> module_ = fun m ->
  List.map ~f:(map_decl m)

and map_program : 'err mapper -> program -> program = fun m ->
  List.map ~f:(map_declaration m)

let fetch_entry_type ~raise : string -> program -> (type_expression * Location.t) = fun main_fname m ->
  let aux (declt : declaration) = match Location.unwrap declt with
    | D_value ({ binder ; expr=_ ; attr=_ } as p) ->
        if Value_var.is_name binder.var main_fname
        then Some p
        else None
    | D_type   _
    | D_module _ ->
      None
  in
  let main_decl_opt = List.find_map ~f:aux @@ List.rev m in
  let main_decl =
    trace_option ~raise (corner_case ("Entrypoint '"^main_fname^"' does not exist")) @@
      main_decl_opt
    in
  let Value_decl.{ binder=_ ; expr ; attr=_} = main_decl in
  expr.type_expression, expr.location

type contract_type = {
  parameter : Ast_typed.type_expression ;
  storage : Ast_typed.type_expression ;
}

let fetch_contract_type ~raise : Value_var.t -> program -> contract_type = fun main_fname m ->
  let aux declt = match Location.unwrap declt with
    | D_value ({ binder ; expr=_ ; attr=_} as p) ->
       if Value_var.equal binder.var main_fname
       then Some p
       else None
    | D_type   _
    | D_module _ ->
      None
  in
  let main_decl_opt = List.find_map ~f:aux @@ List.rev m in
  let main_decl =
    trace_option ~raise (corner_case (Format.asprintf "Entrypoint %a does not exist" Value_var.pp main_fname : string)) @@
      main_decl_opt
  in
  let Value_decl.{ binder ; expr ; attr=_} = main_decl in
  match expr.type_expression.type_content with
  | T_arrow {type1 ; type2} -> (
    match Ast_typed.Combinators.(get_t_pair type1 , get_t_pair type2) with
    | Some (parameter,storage) , Some (listop, storage') ->
      let () = trace_option ~raise (expected_list_operation main_fname listop expr) @@
        Ast_typed.assert_t_list_operation listop in
      let () = trace_option ~raise (expected_same_entry main_fname storage storage' expr) @@
        Ast_typed.assert_type_expression_eq (storage,storage') in
      (* TODO: on storage/parameter : asert_storable, assert_passable ? *)
      { parameter ; storage }
    |  _ -> raise.error @@ bad_contract_io main_fname expr (Value_var.get_location binder.var)
  )
  | _ -> raise.error @@ bad_contract_io main_fname expr (Value_var.get_location binder.var)

(* get_shadowed_decl [prg] [predicate] returns the location of the last shadowed annotated top-level declaration of program [prg] if any
   [predicate] defines the annotation (or set of annotation) you want to match on
*)
let get_shadowed_decl : program -> (ValueAttr.t -> bool) -> Location.t option = fun prg predicate ->
  let aux = fun (seen,shadows : Value_var.t list * Location.t list) (x : declaration) ->
    match Location.unwrap x with
    | D_value { binder ; attr ; _} -> (
      match List.find seen ~f:(Value_var.equal binder.var) with
      | Some x -> (seen , Value_var.get_location x::shadows)
      | None -> if predicate attr then (binder.var::seen , shadows) else seen,shadows
    )
    | _ -> seen,shadows
  in
  let _,shadows = List.fold ~f:aux ~init:([],[]) prg in
  match shadows with [] -> None | hd::_ -> Some hd

(* strip_view_annotations [p] remove all the [@view] annotation in top-level declarations of program [p] *)
let strip_view_annotations : program -> program = fun m ->
  let aux = fun (x:declaration) ->
    match Location.unwrap x with
    | D_value ( {attr ; _} as decl ) when attr.view ->
      { x with wrap_content = D_value { decl with attr = {attr with view = false} } }
    | _ -> x
  in
  List.map ~f:aux m

(* annotate_with_view [p] [binders] for all names in [binders] decorates the top-level declaration of program [p] with the annotation [@view]
  if the name matches with declaration binder. if a name is unmatched, fails.

   e.g:
    annotate_with_view [p] ["a";"b"]
      |->
    [@view] let a = <..>
    let b = <..>
    [@view] let b = <..>
    let c = <..>
*)
let annotate_with_view ~raise : string list -> Ast_typed.program -> Ast_typed.program = fun names prg ->
  let (prg,not_found) = List.fold_right prg ~init:([],names)
    ~f:(
      fun (x:declaration) (prg,views:Ast_typed.program * string list) ->
        let continue = x::prg,views in
        match Location.unwrap x with
        | D_value ({binder ; _} as decl) -> (
          match List.find views ~f:(Value_var.is_name binder.var) with
          | Some found ->
            let decorated = { x with wrap_content = D_value { decl with attr = {decl.attr with view = true} }} in
            decorated::prg, (List.remove_element ~compare:String.compare found views)
          | None -> continue
        )
        | _ -> continue
    )
  in
  let () = match not_found with [] -> () | not_found::_ -> raise.error (corner_case (Format.asprintf "View %s does not exist" not_found : string)) in
  prg

module Free_variables :
  sig
    val expression : expression -> (Module_var.t list * Value_var.t list)
  end
  = struct
  module VarSet    = Caml.Set.Make(Value_var)
  module ModVarSet = Caml.Set.Make(Module_var)
  module VarMap    = Caml.Map.Make(Module_var)

  type moduleEnv' = {modVarSet : ModVarSet.t; moduleEnv: moduleEnv; varSet: VarSet.t}
  and moduleEnv = moduleEnv' VarMap.t

  let rec merge =fun {modVarSet=x1;moduleEnv=y1;varSet=z1} {modVarSet=x2;moduleEnv=y2;varSet=z2} ->
    let aux : Module_var.t -> moduleEnv' -> moduleEnv' -> moduleEnv' option =
      fun _ a b -> Some (merge a b)
    in
      {modVarSet=ModVarSet.union x1 x2;moduleEnv=VarMap.union aux y1 y2;varSet=VarSet.union z1 z2}

  let unions : moduleEnv' list -> moduleEnv' =
    fun l -> List.fold l ~init:{modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}
    ~f:merge
  let rec get_fv_expr : expression -> moduleEnv' = fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v ->
      {modVarSet=ModVarSet.empty; moduleEnv=VarMap.empty ;varSet=VarSet.singleton v}
    | E_literal _ | E_raw_code _ ->
      {modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}
    | E_constant {cons_name=_;arguments} ->
      unions @@ List.map ~f:self arguments
    | E_application {lamb; args} ->
      merge (self lamb) (self args)
    | E_type_inst {forall;type_=_} ->
      self forall
    | E_lambda {binder ; output_type=_ ; result} ->
      let {modVarSet=fmv;moduleEnv;varSet=fv} = self result in
      {modVarSet=fmv;moduleEnv;varSet=VarSet.remove binder.var @@ fv}
    | E_type_abstraction {type_binder=_ ; result} ->
      self result
    | E_recursive {fun_name; lambda = {binder; output_type=_; result};fun_type=_} ->
      let {modVarSet;moduleEnv;varSet=fv} = self result in
      {modVarSet;moduleEnv;varSet=VarSet.remove fun_name @@ VarSet.remove binder.var @@ fv}
    | E_constructor {constructor=_;element} ->
      self element
    | E_matching {matchee; cases} ->
      merge (self matchee)(get_fv_cases cases)
    | E_record m ->
      let res = Record.map self m in
      let res = Record.LMap.to_list res in
      unions res
    | E_update {record;update;path=_} ->
      merge (self record) (self update)
    | E_accessor {record;path=_} ->
      self record
    | E_let_in { let_binder ; rhs ; let_result ; attr=_} ->
      let {modVarSet;moduleEnv;varSet=fv2} = (self let_result) in
      let fv2 = VarSet.remove let_binder.var fv2 in
      merge (self rhs) {modVarSet;moduleEnv;varSet=fv2}
    | E_mod_in { module_binder; rhs ; let_result } ->
      let {modVarSet;moduleEnv;varSet} = (self let_result) in
      let modVarSet = ModVarSet.remove module_binder modVarSet in
      merge (get_fv_module_expr rhs) {modVarSet;moduleEnv;varSet}
    | E_module_accessor { module_path ; element } ->
      ignore element;
      {modVarSet = ModVarSet.of_list module_path (* not sure about that *) ;moduleEnv=VarMap.empty ;varSet=VarSet.empty}
    | E_assign { binder=_; expression } ->
      self expression

  and get_fv_cases : matching_expr -> moduleEnv' = fun m ->
    match m with
    | Match_variant {cases;tv=_} ->
      let aux {constructor=_; pattern ; body} =
        let {modVarSet;moduleEnv;varSet} = get_fv_expr body in
        {modVarSet;moduleEnv;varSet=VarSet.remove pattern @@ varSet} in
      unions @@  List.map ~f:aux cases
    | Match_record {fields; body; tv = _} ->
      let pattern = Record.LMap.values fields |> List.map ~f:(fun b -> b.Binder.var) in
      let {modVarSet;moduleEnv;varSet} = get_fv_expr body in
      {modVarSet;moduleEnv;varSet=List.fold_right pattern ~f:VarSet.remove ~init:varSet}

  and get_fv_module_expr : module_expr -> moduleEnv' =
    fun x ->
      match x.wrap_content with
      | M_struct prg -> get_fv_module prg
      | M_variable _ -> {modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}
      | M_module_path _ -> {modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}

  and get_fv_module : module_ -> moduleEnv' = fun m ->
    let aux = fun x ->
      match Location.unwrap x with
      | D_value {binder=_; expr ; attr=_} ->
        get_fv_expr expr
      | D_module {module_binder=_;module_; module_attr=_} ->
        get_fv_module_expr module_
      | D_type _t ->
        {modVarSet=ModVarSet.empty;moduleEnv=VarMap.empty;varSet=VarSet.empty}
    in
    unions @@ List.map ~f:aux m

  let expression e =
    let {modVarSet;moduleEnv=_;varSet} = get_fv_expr e in
    let fmv = ModVarSet.fold (fun v r -> v :: r) modVarSet [] in
    let fv = VarSet.fold (fun v r -> v :: r) varSet [] in
    (fmv, fv)
end
