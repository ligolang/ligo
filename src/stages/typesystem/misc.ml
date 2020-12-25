open Core

let pair_map = fun f (x , y) -> (f x , f y)

module Substitution = struct
  module Pattern = struct

    open Trace
    module T = Ast_typed
    (* module TSMap = Trace.TMap(String) *)

    type substs = variable:type_variable -> T.type_content option (* this string is a type_name or type_variable I think *)
    let mk_substs ~v ~expr = (v , expr)

    type ('a, 'err) w = substs:substs -> 'a -> ('a,'err) result

    let rec rec_yes = true
    and s_environment_element_definition ~substs = function
      | T.ED_binder -> ok @@ T.ED_binder
      | T.ED_declaration T.{expression ; free_variables} ->
        let%bind expression = s_expression ~substs expression in
        let%bind free_variables = bind_map_list (s_variable ~substs) free_variables in
        ok @@ T.ED_declaration {expression ; free_variables}
    and s_expr_environment : (T.expression_environment,_) w = fun ~substs env ->
      bind_map_list (fun T.{expr_var=variable ; env_elt={ type_value; source_environment; definition }} ->
          let%bind type_value = s_type_expression ~substs type_value in
          let%bind source_environment = s_environment ~substs source_environment in
          let%bind definition = s_environment_element_definition ~substs definition in
          ok @@ T.{expr_var=variable ; env_elt={ type_value; source_environment; definition }}) env
    and s_type_environment : (T.type_environment,_) w = fun ~substs tenv ->
      bind_map_list (fun T.{type_variable ; type_} ->
        let%bind type_ = s_type_expression ~substs type_ in
        ok @@ T.{type_variable ; type_}) tenv
    and s_module_environment: (T.module_environment,_) w = fun ~substs menv ->
      bind_map_list (fun T.{module_name ; module_} ->
        let%bind module_ = s_environment ~substs module_ in
        ok @@ T.{module_name;module_}) menv
    and s_environment : (T.environment,_) w = fun ~substs T.{expression_environment ; type_environment ; module_environment} ->
      let%bind expression_environment = s_expr_environment ~substs expression_environment in
      let%bind type_environment = s_type_environment ~substs type_environment in
      let%bind module_environment = s_module_environment ~substs module_environment in
      ok @@ T.{ expression_environment ; type_environment ; module_environment}

    and s_variable : (T.expression_variable,_) w = fun ~substs var ->
      let () = ignore @@ substs in
      ok var

    and s_type_variable : (T.type_variable,_) w = fun ~substs var ->
      let () = ignore @@ substs in
      ok var

    and s_label : (T.label,_) w = fun ~substs l ->
      let () = ignore @@ substs in
      ok l

    and s_build_in : (T.constant',_) w = fun ~substs b ->
      let () = ignore @@ substs in
      ok b

    and s_constructor : (T.label,_) w = fun ~substs c ->
      let () = ignore @@ substs in
      ok c

    and s_type_content : (T.type_content,_) w = fun ~substs -> function
        | T.T_sum rows ->
           let aux T.{ associated_type; michelson_annotation ; decl_pos } =
             let%bind associated_type = s_type_expression ~substs associated_type in
             ok @@ T.{ associated_type; michelson_annotation; decl_pos } in
           let%bind content = Ast_typed.Helpers.bind_map_lmap aux rows.content in
           ok @@ T.T_sum {content ; layout = rows.layout }
        | T.T_record rows ->
           let aux T.{ associated_type; michelson_annotation ; decl_pos } =
             let%bind associated_type = s_type_expression ~substs associated_type in
             ok @@ T.{ associated_type; michelson_annotation; decl_pos } in
           let%bind content = Ast_typed.Helpers.bind_map_lmap aux rows.content in
           ok @@ T.T_record {content ; layout = rows.layout }
        | T.T_variable variable ->
           begin
             match substs ~variable with
             | Some expr -> s_type_content ~substs expr (* TODO: is it the right thing to recursively examine this? We mustn't go into an infinite loop. *)
             | None -> ok @@ T.T_variable variable
           end
        | T.T_constant {language;injection;parameters} ->
          let%bind parameters = bind_map_list (s_type_expression ~substs) parameters in
          ok @@ T.T_constant {language;injection;parameters}
        | T.T_arrow { type1; type2 } ->
          let%bind type1 = s_type_expression ~substs type1 in
          let%bind type2 = s_type_expression ~substs type2 in
          ok @@ T.T_arrow { type1; type2 }
        | T.T_module_accessor { module_name; element } ->
          let%bind element = s_type_expression ~substs element in
          ok @@ T.T_module_accessor { module_name; element }
        | T.T_singleton x -> ok @@ T.T_singleton x


    and s_abstr_type_content : (Ast_core.type_content,_) w = fun ~substs -> function
      | Ast_core.T_sum _ -> failwith "TODO: subst: unimplemented case s_type_expression sum"
      | Ast_core.T_record _ -> failwith "TODO: subst: unimplemented case s_type_expression record"
      | Ast_core.T_arrow _ -> failwith "TODO: subst: unimplemented case s_type_expression arrow"
      | Ast_core.T_variable _ -> failwith "TODO: subst: unimplemented case s_type_expression variable"
      | Ast_core.T_module_accessor _ -> failwith "TODO: subst: unimplemented case s_type_expression module_accessor"
      | Ast_core.T_singleton      _  -> failwith "TODO: singleton (used with EDO michelson type 'sapling_transaction' and 'sapling_state')"
      | Ast_core.T_app {type_operator;arguments} ->
        let%bind arguments = bind_map_list
          (s_abstr_type_expression ~substs)
          arguments in
        (* TODO: when we have generalized operators, we might need to subst the operator name itself? *)
        ok @@ Ast_core.T_app {type_operator ; arguments}
    and s_abstr_type_expression : (Ast_core.type_expression,_) w = fun ~substs {type_content;sugar;location} ->
      let%bind type_content = s_abstr_type_content ~substs type_content in
      ok @@ (Ast_core.{type_content;sugar;location} : Ast_core.type_expression)

    and s_type_expression : (T.type_expression,_) w = fun ~substs { type_content; location; type_meta } ->
      let%bind type_content = s_type_content ~substs type_content in
      let%bind type_meta = bind_map_option (s_abstr_type_expression ~substs) type_meta in
      ok @@ T.{ type_content; location; type_meta ; orig_var = None}
    and s_literal : (T.literal,_) w = fun ~substs -> function
      | T.Literal_unit ->
        let () = ignore @@ substs in
        ok @@ T.Literal_unit
      | (T.Literal_int _ as x)
      | (T.Literal_nat _ as x)
      | (T.Literal_timestamp _ as x)
      | (T.Literal_mutez _ as x)
      | (T.Literal_string _ as x)
      | (T.Literal_bytes _ as x)
      | (T.Literal_address _ as x)
      | (T.Literal_signature _ as x)
      | (T.Literal_key _ as x)
      | (T.Literal_key_hash _ as x)
      | (T.Literal_chain_id _ as x)
      | (T.Literal_operation _ as x) ->
        ok @@ x
    and s_matching_expr : (T.matching_expr,_) w = fun ~substs _ ->
      let _TODO = substs in
      failwith "TODO: subst: unimplemented case s_matching"
    and s_accessor  : (T.record_accessor,_) w = fun ~substs _ ->
      let _TODO = substs in
      failwith "TODO: subst: unimplemented case s_access_path"

    and s_expression_content : (T.expression_content,_) w = fun ~(substs : substs) -> function
      | T.E_literal         x ->
        let%bind x = s_literal ~substs x in
        ok @@ T.E_literal x
      | T.E_constant   {cons_name;arguments} ->
        let%bind cons_name = s_build_in ~substs cons_name in
        let%bind arguments = bind_map_list (s_expression ~substs) arguments in
        ok @@ T.E_constant {cons_name;arguments}
      | T.E_variable        tv ->
        let%bind tv = s_variable ~substs tv in
        ok @@ T.E_variable tv
      | T.E_application {lamb;args} ->
        let%bind lamb = s_expression ~substs lamb in
        let%bind args = s_expression ~substs args in
        ok @@ T.E_application {lamb;args}
      | T.E_lambda          { binder; result } ->
        let%bind binder = s_variable ~substs binder in
        let%bind result = s_expression ~substs result in
        ok @@ T.E_lambda { binder; result }
      | T.E_let_in          { let_binder; rhs; let_result; inline} ->
        let%bind let_binder = s_variable ~substs let_binder in
        let%bind rhs = s_expression ~substs rhs in
        let%bind let_result = s_expression ~substs let_result in
        ok @@ T.E_let_in { let_binder; rhs; let_result; inline}
      | T.E_type_in          { type_binder; rhs; let_result} ->
        let%bind type_binder = s_type_variable ~substs type_binder in
        let%bind rhs = s_type_expression ~substs rhs in
        let%bind let_result = s_expression ~substs let_result in
        ok @@ T.E_type_in { type_binder; rhs; let_result}
      | T.E_raw_code {language; code} ->
        let%bind code = s_expression ~substs code in
        ok @@ T.E_raw_code {language; code}
      | T.E_recursive { fun_name; fun_type; lambda} ->
        let%bind fun_name = s_variable ~substs fun_name in
        let%bind fun_type = s_type_expression ~substs fun_type in
        let%bind sec = s_expression_content ~substs (T.E_lambda lambda) in
        let lambda = match sec with E_lambda l -> l | _ -> failwith "impossible case" in
        ok @@ T.E_recursive { fun_name; fun_type; lambda}
      | T.E_constructor  {constructor;element} ->
        let%bind constructor = s_constructor ~substs constructor in
        let%bind element = s_expression ~substs element in
        ok @@ T.E_constructor {constructor;element}
      | T.E_record          aemap ->
        let _TODO = aemap in
        failwith "TODO: subst in record"
        (* let%bind aemap = TSMap.bind_map_Map (fun ~k:key ~v:val_ ->
         *     let key = s_type_variable ~v ~expr key in
         *     let val_ = s_expression ~v ~expr val_ in
         *     ok @@ (key , val_)) aemap in
         * ok @@ T.E_record aemap *)
      | T.E_record_accessor {record=e;path} ->
        let%bind record = s_expression ~substs e in
        let%bind path = s_label ~substs path in
        ok @@ T.E_record_accessor {record;path}
      | T.E_record_update {record;path;update}->
        let%bind record = s_expression ~substs record in
        let%bind update = s_expression ~substs update in
        ok @@ T.E_record_update {record;path;update}
      | T.E_matching   {matchee;cases} ->
        let%bind matchee = s_expression ~substs matchee in
        let%bind cases = s_matching_expr ~substs cases in
        ok @@ T.E_matching {matchee;cases}
      | T.E_module_accessor { module_name; element } ->
        let%bind element = s_expression ~substs element in
        ok @@ T.E_module_accessor { module_name; element }

    and s_expression : (T.expression,_) w = fun ~(substs:substs) { expression_content; type_expression; location } ->
      let%bind expression_content = s_expression_content ~substs expression_content in
      let%bind type_expr = s_type_expression ~substs type_expression in
      let location = location in
      ok T.{ expression_content;type_expression=type_expr; location }

    and s_declaration : (T.declaration,_) w = fun ~substs ->
      function
      | Ast_typed.Declaration_constant {binder ; expr ; inline} ->
         let%bind binder = s_variable ~substs binder in
         let%bind expr = s_expression ~substs expr in
         ok @@ Ast_typed.Declaration_constant {binder; expr; inline}
      | Declaration_type t -> ok (Ast_typed.Declaration_type t)

    and s_declaration_wrap : (T.declaration Location.wrap,_) w = fun ~substs d ->
      Trace.bind_map_location (s_declaration ~substs) d

    (* Replace the type variable ~v with ~expr everywhere within the
       program ~p. TODO: issues with scoping/shadowing. *)
    and s_program : (Ast_typed.program_with_unification_vars,_) w = fun ~substs (Ast_typed.Program_With_Unification_Vars p) ->
      let%bind p = Trace.bind_map_list (s_declaration_wrap ~substs) p in
      ok @@ Ast_typed.Program_With_Unification_Vars p

    (*
       Computes `P[v := expr]`.
    *)
    and type_value : tv:type_value -> substs:T.type_ Var.t * type_value -> type_value = fun ~tv ~substs ->
      let open T.Reasons in
      let self tv = type_value ~tv ~substs in
      let (v, expr) = substs in
      match tv.wrap_content with
      | P_variable v' when Var.equal v' v -> expr
      | P_variable _ -> tv
      | P_constant {p_ctor_tag=x ; p_ctor_args=lst} -> (
          let lst' = List.map self lst in
          wrap (Todo "1") @@ T.P_constant {p_ctor_tag=x ; p_ctor_args=lst'}
        )
      | P_apply { tf; targ } -> (
          T.Reasons.(wrap (Todo "2") @@ T.P_apply { tf = self tf ; targ = self targ})
        )
      | P_row {p_row_tag; p_row_args} ->
        let p_row_args = T.LMap.map self p_row_args in
        wrap (Todo "3") @@ T.P_row {p_row_tag ; p_row_args}
      | P_forall p -> (
          let aux c = constraint_ ~c ~substs in
          let constraints = List.map aux p.constraints in
          if (Var.equal p.binder v) then (
            (* The variable v is shadowed by the forall's binder, so
               we don't substitute inside the body. This should be
               handled in a more elegant manner once we have a proper
               environment and scopes. *)
            wrap (Todo "4") @@ T.P_forall { p with constraints }
          ) else (
            (* The variable v is still visible within the forall, so
               substitute also within the body *)
            let body = self p.body in
            wrap (Todo "5") @@ T.P_forall { p with constraints ; body }
          )
        )

    and constraint_ ~c:{c;reason} ~substs =
      {c = constraint__ ~c ~substs;reason}

    and constraint__ ~c ~substs =
      match c with
      | C_equation { aval; bval } -> (
        let aux tv = type_value ~tv ~substs in
          C_equation { aval = aux aval ; bval = aux bval }
        )
      | C_typeclass { tc_args; original_id; typeclass=tc } -> (
          let tc_args = List.map (fun tv -> type_value ~tv ~substs) tc_args in
          let tc = typeclass ~tc ~substs in
          C_typeclass {tc_args ; original_id; typeclass=tc}
        )
      | C_access_label { c_access_label_tval; accessor; c_access_label_tvar } -> (
          let c_access_label_tval = type_value ~tv:c_access_label_tval ~substs in
          C_access_label {c_access_label_tval ; accessor ; c_access_label_tvar}
        )

    and typeclass ~tc ~substs =
      List.map (List.map (fun tv -> type_value ~tv ~substs)) tc

    (* let program = s_program *)

    (* Performs beta-reduction at the root of the type *)
    let eval_beta_root ~(tv : type_value) =
      match tv.wrap_content with
      | P_apply {tf = { location = _ ; wrap_content = P_forall { binder; constraints; body } }; targ} ->
        let constraints = List.map (fun c -> constraint_ ~c ~substs:(mk_substs ~v:binder ~expr:targ)) constraints in
        (* TODO: indicate in the result's tsrc that it was obtained via beta-reduction of the original type *)
        (type_value ~tv:body ~substs:(mk_substs ~v:binder ~expr:targ) , constraints)
      | _ -> (tv , [])
  end

end
