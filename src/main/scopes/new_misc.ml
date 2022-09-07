open New_types

module Bindings_map = Simple_utils.Map.Make ( struct type t = Ast_typed.expression_variable let compare = Ast_typed.Compare.expression_variable end )
type bindings_map = Ast_typed.type_expression Bindings_map.t

let rec extract_variable_types :
  bindings_map -> Ast_typed.declaration_content -> bindings_map =
  fun prev decl ->
    let add env b =
      let aux : Ast_typed.expression_variable *  Ast_typed.type_expression -> Ast_typed.expression_variable * Ast_typed.type_expression = fun (v,t) ->
        let t' = match t.orig_var with Some t' -> 
          { t with type_content = T_variable t'} | None -> t in
        (v,t')
      in
      let b' = List.map ~f:aux b in
      Bindings_map.add_bindings b' env
    in
    let aux : bindings_map -> Ast_typed.expression -> bindings_map = fun env exp ->
      let return = add env in
      match exp.expression_content with
      | E_literal _ | E_application _ | E_raw_code _ | E_constructor _ | E_assign _
      | E_type_abstraction _ | E_mod_in _
      | E_record _ | E_record_accessor _ | E_record_update _ | E_constant _ -> return []
      | E_type_inst _ -> return [] (* TODO *)
      | E_variable v -> return [(v,exp.type_expression)]
      | E_lambda { binder ; _ } ->
        let rec in_t t = match t.Ast_typed.type_content with
          | T_arrow { type1 ; _ } -> type1
          | T_for_all { type_ ; _ } -> in_t type_
          | _ -> failwith "lambda does not have type arrow"
        in
        let in_t = in_t exp.type_expression in
        return [binder.var,in_t]
      | E_recursive { fun_name ; fun_type ; lambda = { binder ; _ } } ->
        let in_t = match fun_type.type_content with
          | T_arrow { type1 ; _ } -> type1
          | _ -> failwith "rec fun does not have type arrow"
        in
        return [ (fun_name , fun_type) ; (binder.var , in_t) ]
      | E_let_in { let_binder ; rhs ; _ } ->
        return [(let_binder.var,rhs.type_expression)]
      | E_matching { matchee ; cases } -> (
        match cases with
        | Match_variant {cases ; tv=_} -> (
          match Ast_typed.get_t_sum matchee.type_expression with
            | Some variant_t ->
              let aux : Ast_typed.matching_content_case -> (Ast_typed.expression_variable * Ast_typed.type_expression) =
                fun { constructor ; pattern ; _ } ->
                  let proj_t = (Ast_typed.LMap.find constructor variant_t.content).associated_type in
                  (pattern,proj_t)
              in
              return (List.map ~f:aux cases)
            | None -> (
              match Ast_typed.get_t_list matchee.type_expression with
              | Some list_proj ->
                let x = List.find_exn ~f:(fun ({constructor=Label l;_}:Ast_typed.matching_content_case) -> String.equal l "Cons") cases in
                let t = Ast_typed.t_pair list_proj matchee.type_expression in
                return [(x.pattern,t)]
              | None -> failwith "matched value in the Match_variant: wrong type"
            )
        )
        | Match_record { fields ; _ }  ->
          let aux = fun Ast_typed.{var;ascr;attributes=_} -> (var, Option.value_exn ~here:[%here] ascr) in
          return (List.map ~f:aux @@ Ast_typed.LMap.to_list fields)
      )
      | E_module_accessor { element=e ; _ } -> return [(e,exp.type_expression)]
    in
    match decl with
    | Declaration_constant { attr = { hidden = true ; _ } ; _ } -> prev
    | Declaration_constant { binder ; expr ; _ } ->
      let prev = add prev [binder.var,expr.type_expression] in
      Self_ast_typed.Helpers.fold_expression aux prev expr
    | Declaration_type _ -> prev
    | Declaration_module { module_ ; _ } ->
      (match module_.wrap_content with
      | M_variable _ -> prev
      | M_module_path _ -> prev
      | M_struct ds -> 
        List.fold_left ds ~init:prev 
          ~f:(fun prev d -> extract_variable_types prev d.wrap_content))

let resolve_if :
  with_types:bool -> bindings_map -> Ast_core.expression_variable -> type_case =
  fun ~with_types bindings var ->
    if with_types then (
      let t_opt = Bindings_map.find_opt var bindings in
      match t_opt with
      | Some t -> Resolved t
      | None -> Unresolved
    )
    else Unresolved

let make_v_def :
  with_types:bool -> ?core_type:Ast_core.type_expression -> bindings_map -> Ast_core.expression_variable -> Location.t -> Location.t -> def =
  fun ~with_types ?core_type bindings var range body_range ->
    let type_case = match core_type with
      | Some t -> Core t
      | None -> resolve_if ~with_types bindings var
    in
    make_v_def (get_binder_name var) type_case range body_range
