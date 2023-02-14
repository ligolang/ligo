open Ligo_prim
open Types
module VarSet = Caml.Set.Make (Value_var)

module Free_variables : sig
  val lambda : (expression, _) Lambda.t -> VarSet.t
  val expression : expression -> Value_var.t list
end = struct
  let unions : VarSet.t list -> VarSet.t =
   fun l -> List.fold l ~init:VarSet.empty ~f:VarSet.union


  let rec get_fv_expr : expression -> VarSet.t =
   fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v -> VarSet.singleton v
    | E_literal _ | E_raw_code _ | E_skip _ | E_module_accessor _ -> VarSet.empty
    | E_list lst -> unions @@ List.map ~f:self lst
    | E_set lst -> unions @@ List.map ~f:self lst
    | E_map lst ->
      unions @@ List.map ~f:(fun (l, r) -> VarSet.union (self l) (self r)) lst
    | E_big_map lst ->
      unions @@ List.map ~f:(fun (l, r) -> VarSet.union (self l) (self r)) lst
    | E_ascription { anno_expr; _ } -> self anno_expr
    | E_matching { matchee; cases } ->
      let aux ({ pattern; body } : _ Match_expr.match_case) =
        let bound = Pattern.binders pattern |> List.map ~f:Binder.get_var in
        List.fold_right bound ~f:VarSet.remove ~init:(self body)
      in
      VarSet.union (self matchee) (unions @@ List.map ~f:aux cases)
    | E_record m ->
      let res = List.map ~f:(fun (_, v) -> self v) m in
      unions res
    | E_accessor { struct_; path } ->
      let aux = function
        | Access_path.Access_tuple _ | Access_record _ -> VarSet.empty
        | Access_map e -> self e
      in
      VarSet.union (self struct_) (unions @@ List.map ~f:aux path)
    | E_update { struct_; path; update } ->
      let aux = function
        | Access_path.Access_tuple _ | Access_record _ -> VarSet.empty
        | Access_map e -> self e
      in
      unions ([ self struct_; self update ] @ List.map ~f:aux path)
    | E_tuple t -> unions @@ List.map ~f:self t
    | E_constructor { element; _ } -> self element
    | E_application { lamb; args } -> VarSet.union (self lamb) (self args)
    | E_let_mut_in { let_binder; rhs; let_result; _ }
    | E_let_in { let_binder; rhs; let_result; _ } ->
      let bound = List.map ~f:Binder.get_var (Pattern.binders let_binder) in
      let fv_let_result =
        List.fold bound ~init:(self let_result) ~f:(fun acc x -> VarSet.remove x acc)
      in
      VarSet.union (self rhs) fv_let_result
    | E_type_in { let_result; type_binder = _; rhs = _ } -> self let_result
    | E_mod_in { rhs; let_result; module_binder = _ } ->
      VarSet.union (get_fv_module_expr rhs.wrap_content) (self let_result)
    | E_lambda { binder; result; output_type = _ } ->
      VarSet.remove (Param.get_var binder) @@ self result
    | E_type_abstraction { type_binder = _; result } -> self result
    | E_recursive
        { fun_name; lambda = { binder; result; _ }; fun_type = _; force_lambdarec = _ } ->
      VarSet.remove fun_name @@ VarSet.remove (Param.get_var binder) @@ self result
    | E_constant { arguments; cons_name = _ } -> unions @@ List.map ~f:self arguments
    | E_cond { condition; then_clause; else_clause } ->
      unions @@ [ self condition; self then_clause; self else_clause ]
    | E_sequence { expr1; expr2 } -> VarSet.union (self expr1) (self expr2)
    | E_assign { binder; expression } ->
      unions @@ [ VarSet.singleton (Binder.get_var binder); self expression ]
    | E_for { binder; start; final; incr; f_body } ->
      VarSet.remove binder @@ unions [ self start; self final; self incr; self f_body ]
    | E_for_each { fe_binder = binder, None; collection; fe_body; collection_type = _ } ->
      unions [ self collection; VarSet.remove binder @@ self fe_body ]
    | E_for_each { fe_binder = binder, Some binder'; collection; fe_body; _ } ->
      unions
        [ self collection; VarSet.remove binder @@ VarSet.remove binder' @@ self fe_body ]
    | E_while { cond; body } -> unions [ self cond; self body ]
    | E_originate { contract = _; storage; key_hash; tez } ->
      unions [ self storage; self key_hash; self tez ]
    | E_contract_call { contract = _; address; method_ = _; params; on_none } ->
      let on_none = Option.value_map on_none ~default:VarSet.empty ~f:self in
      unions ([ self address; on_none ] @ List.map params ~f:self)


  and get_fv_module_expr : module_expr_content -> VarSet.t = function
    | M_struct prg -> get_fv_module prg
    | M_variable _ -> VarSet.empty
    | M_module_path _ -> VarSet.empty


  and get_fv_contract_expr : contract_expr -> VarSet.t =
   fun contract_expr ->
    match Location.unwrap contract_expr with
    | C_struct decls -> get_fv_contract decls
    | C_variable _ -> VarSet.empty
    | C_module_path _ -> VarSet.empty


  and get_fv_module : module_ -> VarSet.t =
   fun p ->
    let aux (x : decl) =
      match Location.unwrap x with
      | D_value { binder = _; expr; attr = _ } -> get_fv_expr expr
      | D_irrefutable_match { pattern = _; expr; attr = _ } -> get_fv_expr expr
      | D_type _t -> VarSet.empty
      | D_module { module_binder = _; module_; module_attr = _ } ->
        get_fv_module_expr module_.wrap_content
      | D_contract { contract_binder = _; contract; contract_attr = _ } ->
        get_fv_contract_expr contract
    in
    unions @@ List.map ~f:aux p


  and get_fv_contract_decl : contract_declaration -> VarSet.t =
   fun decl ->
    match Location.unwrap decl with
    | C_entry { binder = _; expr; attr = _ }
    | C_view { binder = _; expr; attr = _ }
    | C_value { binder = _; expr; attr = _ } -> get_fv_expr expr
    | C_irrefutable_match { pattern = _; expr; attr = _ } -> get_fv_expr expr
    | C_type _t -> VarSet.empty
    | C_module { module_binder = _; module_; module_attr = _ } ->
      get_fv_module_expr module_.wrap_content
    | C_contract { contract_binder = _; contract; contract_attr = _ } ->
      get_fv_contract_expr contract


  and get_fv_contract : contract_declaration list -> VarSet.t =
   fun contract -> unions @@ List.map ~f:get_fv_contract_decl contract


  let expression e = VarSet.fold (fun v r -> v :: r) (get_fv_expr e) []

  let lambda Lambda.{ binder; result; output_type = _ } =
    VarSet.remove (Param.get_var binder) @@ get_fv_expr result
end
