open Core
open Ligo_prim
open Types

module Declaration_mapper = struct
  type mapper = declaration -> declaration

  let rec map_expression : mapper -> expression -> expression =
   fun f expr ->
    let self = map_expression f in
    let return expression_content = { expr with expression_content } in
    match expr.expression_content with
    | E_application app -> return @@ E_application (Application.map self app)
    | E_lambda lamb -> return @@ E_lambda (Lambda.map self Fun.id lamb)
    | E_recursive rec_ -> return @@ E_recursive (Recursive.map self Fun.id rec_)
    | E_type_abstraction type_abs ->
      return @@ E_type_abstraction (Type_abs.map self type_abs)
    | E_let_in let_in -> return @@ E_let_in (Let_in.map self Fun.id let_in)
    | E_type_in type_in -> return @@ E_type_in (Type_in.map self Fun.id type_in)
    | E_mod_in mod_in -> return @@ E_mod_in (Mod_in.map self Fun.id mod_in)
    | E_raw_code raw_code -> return @@ E_raw_code (Raw_code.map self raw_code)
    | E_constructor constr -> return @@ E_constructor (Constructor.map self constr)
    | E_matching match_ -> return @@ E_matching (Match_expr.map self Fun.id match_)
    | E_record record -> return @@ E_record (Record.map ~f:self record)
    | E_tuple tuple -> return @@ E_tuple (Tuple.map self tuple)
    | E_array array -> return @@ E_array (Array_repr.map self array)
    | E_array_as_list array -> return @@ E_array_as_list (Array_repr.map self array)
    | E_accessor accessor -> return @@ E_accessor (Accessor.map self accessor)
    | E_update update -> return @@ E_update (Update.map self update)
    | E_ascription ascr -> return @@ E_ascription (Ascription.map self Fun.id ascr)
    | E_let_mut_in let_mut_in ->
      return @@ E_let_mut_in (Let_in.map self Fun.id let_mut_in)
    | E_assign assign -> return @@ E_assign (Assign.map self Fun.id assign)
    | E_for for_ -> return @@ E_for (For_loop.map self for_)
    | E_for_each for_each -> return @@ E_for_each (For_each_loop.map self for_each)
    | E_while while_ -> return @@ E_while (While_loop.map self while_)
    | E_constant constant -> return @@ E_constant (Constant.map self constant)
    | E_variable _ | E_contract _ | E_literal _ | E_module_accessor _ -> expr


  and map_expression_in_module_expr
      : (declaration -> declaration) -> module_expr -> module_expr
    =
   fun f module_expr -> Location.map (Module_expr.map (map_declaration f)) module_expr


  and map_declaration f decl =
    let decl = f decl in
    decl
    |> Location.map
       @@ function
       | D_value val_decl -> D_value (Value_decl.map (map_expression f) Fun.id val_decl)
       | D_module mod_decl ->
         D_module (Module_decl.map (map_expression_in_module_expr f) Fun.id mod_decl)
       | D_irrefutable_match pat_decl ->
         D_irrefutable_match (Pattern_decl.map (map_expression f) Fun.id pat_decl)
       | D_module_include module_ ->
         D_module_include (map_expression_in_module_expr f module_)
       | (D_type _ | D_signature _ | D_import _) as decl -> decl


  and map_decl m d = map_declaration m d
  and map_module : mapper -> module_ -> module_ = fun m -> List.map ~f:(map_decl m)
end

module Expression_mapper = struct
  type mapper = expression -> expression

  let rec map_expression : mapper -> expression -> expression =
   fun f expr ->
    let self = map_expression f in
    let return expression_content = { expr with expression_content } in
    f
      (match expr.expression_content with
      | E_application app -> return @@ E_application (Application.map self app)
      | E_lambda lamb -> return @@ E_lambda (Lambda.map self Fun.id lamb)
      | E_recursive rec_ -> return @@ E_recursive (Recursive.map self Fun.id rec_)
      | E_type_abstraction type_abs ->
        return @@ E_type_abstraction (Type_abs.map self type_abs)
      | E_let_in let_in -> return @@ E_let_in (Let_in.map self Fun.id let_in)
      | E_type_in type_in -> return @@ E_type_in (Type_in.map self Fun.id type_in)
      | E_mod_in mod_in -> return @@ E_mod_in (Mod_in.map self Fun.id mod_in)
      | E_raw_code raw_code -> return @@ E_raw_code (Raw_code.map self raw_code)
      | E_constructor constr -> return @@ E_constructor (Constructor.map self constr)
      | E_matching match_ -> return @@ E_matching (Match_expr.map self Fun.id match_)
      | E_record record -> return @@ E_record (Record.map ~f:self record)
      | E_tuple tuple -> return @@ E_tuple (Tuple.map self tuple)
      | E_array array -> return @@ E_array (Array_repr.map self array)
      | E_array_as_list array -> return @@ E_array_as_list (Array_repr.map self array)
      | E_accessor accessor -> return @@ E_accessor (Accessor.map self accessor)
      | E_update update -> return @@ E_update (Update.map self update)
      | E_ascription ascr -> return @@ E_ascription (Ascription.map self Fun.id ascr)
      | E_let_mut_in let_mut_in ->
        return @@ E_let_mut_in (Let_in.map self Fun.id let_mut_in)
      | E_assign assign -> return @@ E_assign (Assign.map self Fun.id assign)
      | E_for for_ -> return @@ E_for (For_loop.map self for_)
      | E_for_each for_each -> return @@ E_for_each (For_each_loop.map self for_each)
      | E_while while_ -> return @@ E_while (While_loop.map self while_)
      | E_constant constant -> return @@ E_constant (Constant.map self constant)
      | E_variable _ | E_contract _ | E_literal _ | E_module_accessor _ -> expr)


  and map_expression_in_module_expr : mapper -> module_expr -> module_expr =
   fun f module_expr -> Location.map (Module_expr.map (map_declaration f)) module_expr


  and map_declaration f decl =
    decl
    |> Location.map
       @@ function
       | D_value val_decl -> D_value (Value_decl.map (map_expression f) Fun.id val_decl)
       | D_module mod_decl ->
         D_module (Module_decl.map (map_expression_in_module_expr f) Fun.id mod_decl)
       | D_irrefutable_match pat_decl ->
         D_irrefutable_match (Pattern_decl.map (map_expression f) Fun.id pat_decl)
       | D_module_include module_ ->
         D_module_include (map_expression_in_module_expr f module_)
       | (D_type _ | D_signature _ | D_import _) as decl -> decl


  and map_decl m d = map_declaration m d
  and map_module : mapper -> module_ -> module_ = fun m -> List.map ~f:(map_decl m)
end

let subst_expr ~var ~by ~in_ =
  Expression_mapper.map_expression
    (fun e ->
      match e.expression_content with
      | E_variable x -> if Value_var.equal x var then by else e
      | _ -> e)
    in_


let subst_var ~old_var ~new_var expr =
  Expression_mapper.map_expression
    (fun e ->
      match e.expression_content with
      | E_variable x ->
        if Value_var.equal x old_var
        then { e with expression_content = E_variable new_var }
        else e
      | _ -> e)
    expr


(* Free type variables in a type *)
module VarSet = Set

let empty_set = VarSet.empty (module Type_var)

(** Substitutes a type variable [v] for a type [t] in the type [u],
    taken from association list [vt]. In principle, variables could
    be captured. But in case a binder (forall, abstraction) is
    found in [fv], a new (fresh) binder is generated and subtituted
    to prevent capture. *)
let rec subst_type
    ?(fv = empty_set)
    (vt : (Type_var.t * type_expression) list)
    (u : type_expression)
  =
  let self = subst_type ~fv in
  let loc = u.location in
  match u.type_content with
  | T_variable v' ->
    (match List.find vt ~f:(fun (v, _) -> Type_var.equal v v') with
    | Some (_, t) -> t
    | None -> u)
  | T_arrow { type1; type2; param_names } ->
    let type1 = self vt type1 in
    let type2 = self vt type2 in
    { u with type_content = T_arrow { type1; type2; param_names } }
  | T_abstraction { ty_binder; kind; type_ } ->
    if VarSet.mem fv ty_binder
    then (
      let ty_binder' = Type_var.fresh ~loc () in
      let type_ =
        self ((ty_binder, Combinators.t_variable ~loc ty_binder' ()) :: vt) type_
      in
      let ty_binder = ty_binder' in
      self vt { u with type_content = T_abstraction { ty_binder; kind; type_ } })
    else (
      match List.find vt ~f:(fun (v, _) -> Type_var.equal v ty_binder) with
      | Some (_, _) -> u
      | None ->
        let type_ = self vt type_ in
        { u with type_content = T_abstraction { ty_binder; kind; type_ } })
  | T_for_all { ty_binder; kind; type_ } ->
    if VarSet.mem fv ty_binder
    then (
      let ty_binder' = Type_var.fresh ~loc () in
      let type_ =
        self ((ty_binder, Combinators.t_variable ~loc ty_binder' ()) :: vt) type_
      in
      let ty_binder = ty_binder' in
      self vt { u with type_content = T_for_all { ty_binder; kind; type_ } })
    else (
      match List.find vt ~f:(fun (v, _) -> Type_var.equal v ty_binder) with
      | Some (_, _) -> u
      | None ->
        let type_ = self vt type_ in
        { u with type_content = T_for_all { ty_binder; kind; type_ } })
  | T_sum row ->
    let row = Row.map (self vt) row in
    { u with type_content = T_sum row }
  | T_union union ->
    let union = Union.map (self vt) union in
    { u with type_content = T_union union }
  | T_record row ->
    let row = Row.map (self vt) row in
    { u with type_content = T_record row }
  | T_app { type_operator; arguments } ->
    { u with
      type_content =
        T_app
          { type_operator
          ; arguments =
              List.fold_right arguments ~init:[] ~f:(fun arg acc -> self vt arg :: acc)
          }
    }
  | T_module_accessor _ | T_singleton _ | T_contract_parameter _ | T_constant (_, _) -> u


let rec remove_ascriptions (expr : expression) =
  match expr.expression_content with
  | E_ascription ascription -> remove_ascriptions ascription.anno_expr
  | _ -> expr


module Condition_accessor_equals_literal = struct
  type ('expr, 'typ) t =
    { struct_ : 'expr
    ; field : Label.t
    ; lit : Literal_value.t
    }

  let decompose_expr_as_eq (expr : expression) =
    match (remove_ascriptions expr).expression_content with
    | E_constant { cons_name = C_EQ; arguments = [ x; y ] } -> Some (x, y)
    | _ -> None


  let decompose_expr_as_or (expr : expression) =
    match (remove_ascriptions expr).expression_content with
    | E_constant { cons_name = C_OR; arguments = [ x; y ] } -> Some (x, y)
    | _ -> None


  let of_expr expr : ('expr, 'typ) t option =
    let open Option.Let_syntax in
    let expr =
      (* Temporary hack to keep the behavior switches on usions had when they were handled by the nanopass *)
      let is_fallthrough (e : expr) =
        match e.expression_content with
        | E_variable var when Value_var.is_name_prefix var "fallthrough" -> true
        | E_constant { cons_name = C_FALSE; _ } -> true
        | _ -> false
      in
      match decompose_expr_as_or expr with
      | Some (expr1, expr2) when is_fallthrough expr2 -> expr1
      | Some (expr1, expr2) when is_fallthrough expr1 -> expr2
      | Some _ | None -> expr
    in
    let%bind x, y = decompose_expr_as_eq expr in
    let%bind Accessor.{ struct_; path = field } =
      Combinators.get_e_accessor (remove_ascriptions x)
    in
    let%bind lit = Combinators.get_e_literal (remove_ascriptions y) in
    return { struct_; field; lit }
end

module Conditional_with_condition_accessor_equals_literal = struct
  (** This type represents expressions of the following shape
(with potential type annotations everywhere):
        {[
        if (struct_.field == lit) {
          then_body
        } else {
          else_body
        }
        ]}*)
  type ('expr, 'typ) t =
    ( ('expr, 'typ) Condition_accessor_equals_literal.t
    , 'expr )
    Conditional.With_custom_condition.t

  let decompose_match_as_conditional match_ =
    let open Option.Let_syntax in
    match match_ with
    | Match_expr.
        { matchee = condition
        ; cases =
            [ { pattern = pattern1; body = body1 }; { pattern = pattern2; body = body2 } ]
        } ->
      let%bind bool1 = Pattern.bool_of_boolean_pattern pattern1 in
      let%bind bool2 = Pattern.bool_of_boolean_pattern pattern2 in
      (match bool1, body1, bool2, body2 with
      | true, then_body, false, else_body | false, else_body, true, then_body ->
        Some Conditional.With_custom_condition.{ condition; then_body; else_body }
      | _ -> None)
    | _ -> None


  let decompose_expr_as_conditional (expr : expression) =
    match (remove_ascriptions expr).expression_content with
    | E_matching match_ -> decompose_match_as_conditional match_
    | _ -> None


  let of_expr expr : (expression, type_expression) t option =
    let open Option.Let_syntax in
    decompose_expr_as_conditional expr
    >>= Conditional.With_custom_condition.filter_map_condition
          Condition_accessor_equals_literal.of_expr
end
