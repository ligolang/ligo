module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Pair = Simple_utils.Pair
open Ligo_prim
open Types

let remove_empty_annotation (ann : string option) : string option =
  match ann with
  | Some "" -> None
  | Some ann -> Some ann
  | None -> None


(* These tables are used during inference / for substitution *)
module TMap = Simple_utils.Map.Make (Type_var)

(* Free type variables in a type *)
module VarSet = Caml.Set.Make (Type_var)

(* Substitutes a type variable `v` for a type `t` in the type `u`. In
   principle, variables could be captured. But in case a binder
   (forall, abstraction) is found in `fv`, a new (fresh) binder is
   generated and subtituted to prevent capture. *)
let rec subst_type ?(fv = VarSet.empty) v t (u : type_expression) =
  let self = subst_type ~fv in
  let loc = u.location in
  match u.type_content with
  | T_variable v' when Type_var.equal v v' -> t
  | T_arrow { type1; type2; param_names } ->
    let type1 = self v t type1 in
    let type2 = self v t type2 in
    { u with type_content = T_arrow { type1; type2; param_names } }
  | T_abstraction { ty_binder; kind; type_ } when VarSet.mem ty_binder fv ->
    let ty_binder' = Type_var.fresh ~loc () in
    let type_ = self ty_binder (Combinators.t_variable ~loc ty_binder' ()) type_ in
    let ty_binder = ty_binder' in
    self v t { u with type_content = T_abstraction { ty_binder; kind; type_ } }
  | T_abstraction { ty_binder; kind; type_ } when not (Type_var.equal ty_binder v) ->
    let type_ = self v t type_ in
    { u with type_content = T_abstraction { ty_binder; kind; type_ } }
  | T_for_all { ty_binder; kind; type_ } when VarSet.mem ty_binder fv ->
    let ty_binder' = Type_var.fresh ~loc () in
    let type_ = self ty_binder (Combinators.t_variable ~loc ty_binder' ()) type_ in
    let ty_binder = ty_binder' in
    self v t { u with type_content = T_for_all { ty_binder; kind; type_ } }
  | T_for_all { ty_binder; kind; type_ } when not (Type_var.equal ty_binder v) ->
    let type_ = self v t type_ in
    { u with type_content = T_for_all { ty_binder; kind; type_ } }
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:(self v t) parameters in
    { u with type_content = T_constant { language; injection; parameters } }
  | T_sum (row, orig_label) ->
    let row = Row.map (self v t) row in
    { u with type_content = T_sum (row, orig_label) }
  | T_record row ->
    let row = Row.map (self v t) row in
    { u with type_content = T_record row }
  | _ -> u


type 'a fold_mapper = 'a -> expression -> bool * 'a * expression

let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> 'a * expression =
 fun f a e ->
  let self = fold_map_expression f in
  let self_type acc t = acc, t in
  let continue, init, e' = f a e in
  if not continue
  then init, e'
  else (
    let return expression_content = { e' with expression_content } in
    match e'.expression_content with
    | E_matching { matchee = e; cases } ->
      let res, e' = self init e in
      let res, cases' = fold_map_cases f res cases in
      res, return @@ E_matching { matchee = e'; cases = cases' }
    | E_accessor { struct_; path } ->
      let res, struct_ = self init struct_ in
      res, return @@ E_accessor { struct_; path }
    | E_record m ->
      let res, m' = Record.fold_map ~f:(fun res e -> self res e) ~init m in
      res, return @@ E_record m'
    | E_update { struct_; path; update } ->
      let res, struct_ = self init struct_ in
      let res, update = self res update in
      res, return @@ E_update { struct_; path; update }
    | E_constructor c ->
      let res, e' = self init c.element in
      res, return @@ E_constructor { c with element = e' }
    | E_application { lamb; args } ->
      let ab = lamb, args in
      let res, (a, b) = Pair.fold_map ~f:self ~init ab in
      res, return @@ E_application { lamb = a; args = b }
    | E_let_in { let_binder; rhs; let_result; attributes } ->
      let res, rhs = self init rhs in
      let res, let_result = self res let_result in
      res, return @@ E_let_in { let_binder; rhs; let_result; attributes }
    | E_mod_in { module_binder; rhs; let_result } ->
      let res, let_result = self init let_result in
      let res, rhs = fold_map_expression_in_module_expr f res rhs in
      res, return @@ E_mod_in { module_binder; rhs; let_result }
    | E_type_inst { forall; type_ } ->
      let res, forall = self init forall in
      res, return @@ E_type_inst { forall; type_ }
    | E_lambda l ->
      let res, l = Lambda.fold_map self self_type init l in
      res, return @@ E_lambda l
    | E_type_abstraction ta ->
      let res, ta = Type_abs.fold_map self init ta in
      res, return @@ E_type_abstraction ta
    | E_recursive r ->
      let res, r = Recursive.fold_map self self_type init r in
      res, return @@ E_recursive r
    | E_constant c ->
      let res, args = List.fold_map ~f:self ~init c.arguments in
      res, return @@ E_constant { c with arguments = args }
    | E_raw_code { language; code } ->
      let res, code = self init code in
      res, return @@ E_raw_code { language; code }
    | E_assign a ->
      let res, a = Assign.fold_map self self_type init a in
      res, return @@ E_assign a
    | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
      let res, rhs = self init rhs in
      let res, let_result = self res let_result in
      res, return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
    | E_coerce asc ->
      let res, asc = Ascription.fold_map self self_type init asc in
      res, return @@ E_coerce asc
    | E_for f ->
      let res, f = For_loop.fold_map self init f in
      res, return @@ E_for f
    | E_for_each fe ->
      let res, fe = For_each_loop.fold_map self init fe in
      res, return @@ E_for_each fe
    | E_while w ->
      let res, w = While_loop.fold_map self init w in
      res, return @@ E_while w
    | (E_deref _ | E_literal _ | E_variable _ | E_contract _ | E_module_accessor _) as e'
      -> init, return e')


and fold_map_case
    :  'a fold_mapper -> 'a -> (expression, type_expression) Types.Match_expr.match_case
    -> 'a * (expression, type_expression) Types.Match_expr.match_case
  =
 fun f init { pattern; body } ->
  let init, body = fold_map_expression f init body in
  init, { pattern; body }


and fold_map_cases
    :  'a fold_mapper -> 'a
    -> (expression, type_expression) Types.Match_expr.match_case list
    -> 'a * (expression, type_expression) Types.Match_expr.match_case list
  =
 fun f init ms -> List.fold_map ms ~init ~f:(fold_map_case f)


and fold_map_declaration m acc (x : declaration) =
  match Location.unwrap x with
  | D_value { binder; expr; attr } ->
    let acc', expr = fold_map_expression m acc expr in
    let wrap_content = D_value { binder; expr; attr } in
    acc', { x with wrap_content }
  | D_irrefutable_match { pattern; expr; attr } ->
    let acc', expr = fold_map_expression m acc expr in
    let wrap_content = D_irrefutable_match { pattern; expr; attr } in
    acc', { x with wrap_content }
  | D_type t ->
    let wrap_content = D_type t in
    acc, { x with wrap_content }
  | D_module { module_binder; module_; module_attr; annotation = () } ->
    let acc', module_ = (fold_map_expression_in_module_expr m) acc module_ in
    let wrap_content =
      D_module { module_binder; module_; module_attr; annotation = () }
    in
    acc', { x with wrap_content }
  | D_module_include module_ ->
    let acc', module_ = (fold_map_expression_in_module_expr m) acc module_ in
    acc', { x with wrap_content = D_module_include module_ }
  | D_signature sig_ -> acc, { x with wrap_content = D_signature sig_ }


and fold_map_decl m = fold_map_declaration m

and fold_map_module : 'a fold_mapper -> 'a -> module_ -> 'a * module_ =
 fun m init -> List.fold_map ~f:(fold_map_decl m) ~init


and fold_map_expression_in_module_expr
    : 'a fold_mapper -> 'a -> module_expr -> 'a * module_expr
  =
 fun fold_mapper acc x ->
  let return r module_content = r, { x with module_content } in
  match x.module_content with
  | M_struct decls ->
    let res, decls = fold_map_module fold_mapper acc decls in
    return res (Module_expr.M_struct decls)
  | M_module_path _ as x -> return acc x
  | M_variable _ as x -> return acc x


let fold_map_program : 'a fold_mapper -> 'a -> program -> 'a * program =
 fun m init p ->
  let res, module_ = List.fold_map ~f:(fold_map_declaration m) ~init p.pr_module in
  res, { p with pr_module = module_ }


let rec fold_type_expression
    : type a. type_expression -> init:a -> f:(a -> type_expression -> a) -> a
  =
 fun te ~init ~f ->
  let self te = fold_type_expression te ~f in
  let init = f init te in
  match te.type_content with
  | T_variable _ -> init
  | T_constant { parameters; _ } -> List.fold parameters ~init ~f
  | T_sum (row, _) | T_record row -> Row.fold f init row
  | T_arrow { type1; type2; param_names = _ } -> self type2 ~init:(self type1 ~init)
  | T_singleton _ -> init
  | T_abstraction { type_; _ } | T_for_all { type_; _ } -> self type_ ~init


let map_expression f prg = snd @@ fold_map_program (fun () exp -> true, (), f exp) () prg

(* An [IdMap] is a [Map] augmented with an [id] field (which is wrapped around the map [value] field).
  Using a map instead of a list makes shadowed modules inaccessible,
  since they are overwritten from the map when adding the shadower, whilst they were kept when using lists.
  The [id] field in the map values is used to infer the type to which a constructor belong when they are not annotated
  e.g. we need to keep the declaration order to infer that 'c' has type z in:

    type x = A of int | B
    module M = struct
      type y = A of int
    end
    type z = A of int | AA

    let c = A 2
*)

let global_id = ref 0

module IdMap = struct
  module type OrderedType = Caml.Map.OrderedType

  module type IdMapSig = sig
    type key
    type 'a t
    type 'a kvi_list = (key * 'a * int) list
  end
  (* of module type S *)

  module Make (Ord : OrderedType) : IdMapSig with type key = Ord.t = struct
    module Map = Simple_utils.Map.Make (Ord)

    type key = Ord.t

    type 'a id_wrapped =
      { id : int
            (* This is is used in [filter_values], to return a list of matching values in chronological order *)
      ; value : 'a
      }
    [@@warning "-69"]

    type 'a t = 'a id_wrapped Map.t
    type 'a kvi_list = (key * 'a * int) list
  end
  (* of module IdMap.Make*)
end
(* of module IdMap *)

(* get_views [p] looks for top-level declaration annotated with [@view] in program [p] and return declaration data *)
let get_views : program -> (Value_var.t * Location.t) list =
 fun p ->
  let rec loop module_ =
    let f
        :  declaration -> (Value_var.t * Location.t) list
        -> (Value_var.t * Location.t) list
      =
     fun { wrap_content = decl; location = _ } acc ->
      match decl with
      | D_value { binder; expr = _; attr } when attr.view ->
        let var = Binder.get_var binder in
        (var, Value_var.get_location var) :: acc
      | D_irrefutable_match
          { pattern = { wrap_content = P_var binder; _ }; expr = _; attr }
        when attr.view ->
        let var = Binder.get_var binder in
        (var, Value_var.get_location var) :: acc
      | D_module_include { module_content = M_struct x; _ } -> loop x
      | D_type _
      | D_module _
      | D_value _
      | D_irrefutable_match _
      | D_module_include _
      | D_signature _ -> acc
    in
    (* TODO: This would be easier to use the signature instead of the module *)
    List.fold_right ~init:[] ~f module_
  in
  loop p.pr_module


let fetch_view_type : declaration -> (type_expression * type_expression Binder.t) option =
 fun declt ->
  match Location.unwrap declt with
  | D_value { binder; expr; attr }
  | D_irrefutable_match { pattern = { wrap_content = P_var binder; _ }; expr; attr }
    when attr.view ->
    Some (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
  | D_value _
  | D_irrefutable_match _
  | D_type _
  | D_module _
  | D_module_include _
  | D_signature _ -> None
