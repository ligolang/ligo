open Types
open Ligo_prim

type 'a fold_mapper = 'a -> expression -> bool * 'a * expression

let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> 'a * expression =
 fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = acc, a in
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
    | E_record m ->
      let res, m' = Record.fold_map ~f:self ~init m in
      res, return @@ E_record m'
    | E_accessor acc ->
      let res, acc = Types.Accessor.fold_map self init acc in
      res, return @@ E_accessor acc
    | E_update u ->
      let res, u = Types.Update.fold_map self init u in
      res, return @@ E_update u
    | E_constructor c ->
      let res, e' = self init c.element in
      res, return @@ E_constructor { c with element = e' }
    | E_application { lamb; args } ->
      let ab = lamb, args in
      let res, (a, b) = Simple_utils.Pair.fold_map ~f:self ~init ab in
      res, return @@ E_application { lamb = a; args = b }
    | E_let_in { let_binder; rhs; let_result; attributes } ->
      let res, rhs = self init rhs in
      let res, let_result = self res let_result in
      res, return @@ E_let_in { let_binder; rhs; let_result; attributes }
    | E_type_inst { forall; type_ } ->
      let res, forall = self init forall in
      res, return @@ E_type_inst { forall; type_ }
    | E_lambda l ->
      let res, l = Lambda.fold_map self idle init l in
      res, return @@ E_lambda l
    | E_type_abstraction ta ->
      let res, ta = Type_abs.fold_map self init ta in
      res, return @@ E_type_abstraction ta
    | E_recursive r ->
      let res, r = Recursive.fold_map self idle init r in
      res, return @@ E_recursive r
    | E_constant c ->
      let res, c = Constant.fold_map self init c in
      res, return @@ E_constant c
    | E_raw_code { language; code } ->
      let res, code = self init code in
      res, return @@ E_raw_code { language; code }
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
    | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
      let res, rhs = self init rhs in
      let res, let_result = self res let_result in
      res, return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
    | (E_deref _ | E_literal _ | E_variable _) as e' -> init, return e')


and fold_map_cases : 'a fold_mapper -> 'a -> matching_expr -> 'a * matching_expr =
 fun f init m ->
  match m with
  | Match_variant { cases; tv } ->
    let aux init { constructor; pattern; body } =
      let init, body = fold_map_expression f init body in
      init, { constructor; pattern; body }
    in
    let init, cases = List.fold_map ~f:aux ~init cases in
    init, Match_variant { cases; tv }
  | Match_record { fields; body; tv } ->
    let init, body = fold_map_expression f init body in
    init, Match_record { fields; body; tv }


let map_expression f exp = fold_map_expression (fun () expr -> true, (), f expr) () exp
let fold_expression f init exp = fst @@ fold_map_expression f init exp

module Free_variables : sig
  val expression : expression -> Value_var.t list
end = struct
  open Ligo_prim
  module VarSet = Caml.Set.Make (Value_var)

  let unions : VarSet.t list -> VarSet.t =
   fun l -> List.fold l ~init:VarSet.empty ~f:VarSet.union


  let rec get_fv_expr : expression -> VarSet.t =
   fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v -> VarSet.singleton v
    | E_literal _ -> VarSet.empty
    | E_raw_code { language = _; code } -> self code
    | E_constant { arguments; _ } -> unions @@ List.map ~f:self arguments
    | E_application { lamb; args } -> VarSet.union (self lamb) (self args)
    | E_type_inst { forall; _ } -> self forall
    | E_lambda { binder; result; _ } ->
      let fv = self result in
      VarSet.remove (Param.get_var binder) @@ fv
    | E_type_abstraction { type_binder = _; result } -> self result
    | E_recursive { fun_name; lambda = { binder; result; _ }; _ } ->
      let fv = self result in
      VarSet.remove fun_name @@ VarSet.remove (Param.get_var binder) @@ fv
    | E_constructor { element; _ } -> self element
    | E_matching { matchee; cases } -> VarSet.union (self matchee) (get_fv_cases cases)
    | E_record m ->
      let res = Record.map ~f:self m in
      let res = Record.values res in
      unions res
    | E_accessor { struct_; _ } -> self struct_
    | E_update { struct_; update; _ } -> VarSet.union (self struct_) (self update)
    | E_let_in { let_binder; rhs; let_result; _ } ->
      let fv2 = self let_result in
      let fv2 = VarSet.remove (Binder.get_var let_binder) fv2 in
      VarSet.union (self rhs) fv2
    (* HACK? return free mutable variables too, without distinguishing
       them from free immutable variables *)
    | E_let_mut_in { let_binder; rhs; let_result; _ } ->
      let fv2 = self let_result in
      let fv2 = VarSet.remove (Binder.get_var let_binder) fv2 in
      VarSet.union (self rhs) fv2
    | E_assign { binder; expression } ->
      VarSet.union (VarSet.singleton (Binder.get_var binder)) (self expression)
    | E_deref v -> VarSet.singleton v
    | E_for { binder; start; final; incr; f_body } ->
      unions [ self start; self final; self incr; VarSet.remove binder (self f_body) ]
    | E_for_each { fe_binder = binder, None; collection; fe_body; collection_type = _ } ->
      unions [ self collection; VarSet.remove binder (self fe_body) ]
    | E_for_each { fe_binder = binder1, Some binder2; collection; fe_body; _ } ->
      unions
        [ self collection
        ; VarSet.remove binder1 @@ VarSet.remove binder2 @@ self fe_body
        ]
    | E_while { cond; body } -> VarSet.union (self cond) (self body)


  and get_fv_cases : matching_expr -> VarSet.t =
   fun m ->
    match m with
    | Match_variant { cases; tv = _ } ->
      let aux { constructor = _; pattern; body } =
        let varSet = get_fv_expr body in
        VarSet.remove pattern @@ varSet
      in
      unions @@ List.map ~f:aux cases
    | Match_record { fields; body; tv = _ } ->
      let pattern = Record.values fields |> List.map ~f:Binder.get_var in
      let varSet = get_fv_expr body in
      List.fold_right pattern ~f:VarSet.remove ~init:varSet


  let expression e =
    let varSet = get_fv_expr e in
    let fv = VarSet.fold (fun v r -> v :: r) varSet [] in
    fv
end
