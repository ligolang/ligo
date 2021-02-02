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
      bind_map_list (fun T.{expr_var=variable ; env_elt={ type_value; definition }} ->
          let%bind type_value = s_type_expression ~substs type_value in
          let%bind definition = s_environment_element_definition ~substs definition in
          ok @@ T.{expr_var=variable ; env_elt={ type_value; definition }}) env
    and s_type_environment : (T.type_environment,_) w = fun ~substs tenv ->
      bind_map_list (fun T.{type_variable ; type_} ->
        let%bind type_ = s_type_expression ~substs type_ in
        ok @@ T.{type_variable ; type_}) tenv
    and s_module_environment: (T.module_environment,_) w = fun ~substs menv ->
      bind_map_list (fun T.{module_variable ; module_} ->
        let%bind module_ = s_environment ~substs module_ in
        ok @@ T.{module_variable;module_}) menv
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

    and s_rows : (T.rows,_) w = fun ~substs rows ->
      let aux T.{ associated_type; michelson_annotation ; decl_pos } =
        let%bind associated_type = s_type_expression ~substs associated_type in
        ok @@ T.{ associated_type; michelson_annotation; decl_pos } in
      let%bind content = Ast_typed.Helpers.bind_map_lmap aux rows.content in
      ok @@ { rows with content }

    and s_type_content : (T.type_content,_) w = fun ~substs -> function
        | T.T_sum rows ->
          let%bind rows = s_rows ~substs rows in
          ok @@ T.T_sum rows
        | T.T_record rows ->
          let%bind rows = s_rows ~substs rows in
          ok @@ T.T_record rows
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

    and s_type_expression : (T.type_expression,_) w = fun ~substs { type_content; location; type_meta } ->
      let%bind type_content = s_type_content ~substs type_content in
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
    and s_matching_expr : (T.matching_expr,_) w = fun ~(substs : substs) -> function
      | Match_list {match_nil;match_cons={hd;tl;body;tv}} ->
        let%bind match_nil = s_expression ~substs match_nil in
        let%bind body      = s_expression ~substs body in
        let%bind tv        = s_type_expression ~substs tv in
        ok @@ T.Match_list {match_nil;match_cons={hd;tl;body;tv}}
      | Match_option {match_none;match_some={opt;body;tv}} ->
        let%bind match_none = s_expression ~substs match_none in
        let%bind body      = s_expression ~substs body in
        let%bind tv        = s_type_expression ~substs tv in
        ok @@ T.Match_option {match_none;match_some={opt;body;tv}}
      | Match_variant {cases;tv} ->
        let%bind cases = bind_map_list (
          fun ({constructor;pattern;body} : T.matching_content_case) ->
            let%bind body = s_expression ~substs body in
            ok @@ ({constructor;pattern;body} : T.matching_content_case)
        ) cases in
        let%bind tv = s_type_expression ~substs tv in
        ok @@ T.Match_variant {cases;tv}
      | Match_record {fields; body; tv}  ->
        let%bind fields = T.Helpers.bind_map_lmap (fun (a,b) -> let%bind b = s_type_expression ~substs b in ok (a,b)) fields in
        let%bind body   = s_expression ~substs body in
        let%bind tv     = s_type_expression ~substs tv in
        ok @@ T.Match_record {fields; body; tv}

    and s_accessor  : (T.record_accessor,_) w = fun ~substs {record;path} ->
      let%bind record = s_expression ~substs record in
      ok @@ ({record;path} : T.record_accessor)


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
      | T.E_mod_in          { module_binder; rhs; let_result} ->
        let%bind rhs = s_module' ~substs rhs in
        let%bind let_result = s_expression ~substs let_result in
        ok @@ T.E_mod_in { module_binder; rhs; let_result}
      | T.E_mod_alias          { alias; binders; result} ->
        let%bind result  = s_expression ~substs result in
        ok @@ T.E_mod_alias { alias; binders; result}
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
        let%bind aemap = bind_map_list (fun (key,val_) ->
          let%bind val_ = s_expression ~substs val_ in
          ok @@ (key , val_)) @@ T.LMap.to_kv_list aemap in
        let aemap = T.LMap.of_list aemap in
        ok @@ T.E_record aemap
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

    and s_declaration : (T.declaration,_) w =
    let return (d : T.declaration) = ok @@ d in
    fun ~substs ->
      function
      | Ast_typed.Declaration_constant {name ; binder ; expr ; inline} ->
        let%bind binder = s_variable ~substs binder in
        let%bind expr = s_expression ~substs expr in
        return @@ Declaration_constant {name; binder; expr; inline}
      | Declaration_type t -> return @@ Declaration_type t
      | Declaration_module {module_binder;module_} ->
        let%bind module_       = s_module' ~substs module_ in
        return @@ Declaration_module {module_binder;module_}
      | Module_alias {alias;binders} ->
        return @@ Module_alias {alias; binders}

    and s_declaration_wrap : (T.declaration Location.wrap,_) w = fun ~substs d ->
      Trace.bind_map_location (s_declaration ~substs) d

    (* Replace the type variable ~v with ~expr everywhere within the
       module ~p. TODO: issues with scoping/shadowing. *)
    and s_module : (Ast_typed.module_with_unification_vars,_) w = fun ~substs (Ast_typed.Module_With_Unification_Vars p) ->
      let%bind p = Trace.bind_map_list (s_declaration_wrap ~substs) p in
      ok @@ Ast_typed.Module_With_Unification_Vars p

    and s_module' : (Ast_typed.module_fully_typed,_) w = fun ~substs (T.Module_Fully_Typed p) ->
      let%bind p = Trace.bind_map_list (s_declaration_wrap ~substs) p in
      ok @@ T.Module_Fully_Typed p

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
        let p_row_args = T.LMap.map (fun ({associated_value;michelson_annotation;decl_pos}: T.row_value ) -> ({associated_value=self associated_value;michelson_annotation;decl_pos} : T.row_value)) p_row_args in
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

    (* let module = s_module *)

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
