open Core

let pair_map = fun f (x , y) -> (f x , f y)

module Substitution = struct
  module Pattern = struct

    open Trace
    module T = Ast_typed
    (* module TSMap = Trace.TMap(String) *)

    type substs = variable:type_variable -> T.type_value' option (* this string is a type_name or type_variable I think *)
    let mk_substs ~v ~expr = (v , expr)

    type 'a w = substs:substs -> 'a -> 'a result

    let rec rec_yes = true
    and s_environment_element_definition ~substs = function
      | T.ED_binder -> ok @@ T.ED_binder
      | T.ED_declaration (val_, free_variables) ->
        let%bind val_ = s_annotated_expression ~substs  val_ in
        let%bind free_variables = bind_map_list (s_variable ~substs) free_variables in
        ok @@ T.ED_declaration (val_, free_variables)
    and s_environment : T.environment w = fun ~substs env ->
      bind_map_list (fun (variable, T.{ type_value; source_environment; definition }) ->
          let%bind variable = s_variable ~substs variable in
          let%bind type_value = s_type_value ~substs type_value in
          let%bind source_environment = s_full_environment ~substs source_environment in
          let%bind definition = s_environment_element_definition ~substs definition in
          ok @@ (variable, T.{ type_value; source_environment; definition })) env
    and s_type_environment : T.type_environment w = fun ~substs tenv ->
      bind_map_list (fun (type_variable , type_value) ->
        let%bind type_variable = s_type_variable ~substs type_variable in
        let%bind type_value = s_type_value ~substs type_value in
        ok @@ (type_variable , type_value)) tenv
    and s_small_environment : T.small_environment w = fun ~substs (environment, type_environment) ->
      let%bind environment = s_environment ~substs environment in
      let%bind type_environment = s_type_environment ~substs type_environment in
      ok @@ (environment, type_environment)
    and s_full_environment : T.full_environment w = fun ~substs (a , b) ->
      let%bind a = s_small_environment ~substs a in
      let%bind b = bind_map_list (s_small_environment ~substs) b in
      ok (a , b)

    and s_variable : T.expression_variable w = fun ~substs var ->
      let () = ignore @@ substs in
      ok var

    and s_type_variable : T.type_variable w = fun ~substs tvar ->
      let _TODO = ignore @@ substs in
      Printf.printf "TODO: subst: unimplemented case s_type_variable";
      ok @@ tvar
      (* if String.equal tvar v then
       *   expr
       * else
       *   ok tvar *)
    and s_label : T.label w = fun ~substs l ->
      let () = ignore @@ substs in
      ok l
    
    and s_build_in : T.constant w = fun ~substs b ->
      let () = ignore @@ substs in
      ok b

    and s_constructor : T.constructor w = fun ~substs c ->
      let () = ignore @@ substs in
      ok c

    and s_type_name_constant : T.type_constant w = fun ~substs type_name ->
      (* TODO: we don't need to subst anything, right? *)
      let () = ignore @@ substs in
      ok @@ type_name

    and s_type_value' : T.type_value' w = fun ~substs -> function
        | T.T_tuple type_value_list ->
          let%bind type_value_list = bind_map_list (s_type_value ~substs) type_value_list in
          ok @@ T.T_tuple type_value_list
        | T.T_sum _ -> failwith "TODO: T_sum"
        | T.T_record _ -> failwith "TODO: T_record"
        | T.T_constant type_name ->
          let%bind type_name = s_type_name_constant ~substs type_name in
          ok @@ T.T_constant (type_name)
        | T.T_variable variable ->
           begin
             match substs ~variable with
             | Some expr -> s_type_value' ~substs expr (* TODO: is it the right thing to recursively examine this? We mustn't go into an infinite loop. *)
             | None -> ok @@ T.T_variable variable
           end
        | T.T_operator type_name_and_args ->
          let bind_map_type_operator = Stage_common.Misc.bind_map_type_operator in (* TODO: write T.Misc.bind_map_type_operator, but it doesn't work *)
          let%bind type_name_and_args = bind_map_type_operator (s_type_value ~substs) type_name_and_args in
          ok @@ T.T_operator type_name_and_args
        | T.T_arrow _ ->
          let _TODO = substs in
          failwith "TODO: T_function"

    and s_type_expression' : _ Ast_simplified.type_expression' w = fun ~substs -> function
      | Ast_simplified.T_tuple _ -> failwith "TODO: subst: unimplemented case s_type_expression tuple"
      | Ast_simplified.T_sum _ -> failwith "TODO: subst: unimplemented case s_type_expression sum"
      | Ast_simplified.T_record _ -> failwith "TODO: subst: unimplemented case s_type_expression record"
      | Ast_simplified.T_arrow (_, _) -> failwith "TODO: subst: unimplemented case s_type_expression arrow"
      | Ast_simplified.T_variable _ -> failwith "TODO: subst: unimplemented case s_type_expression variable"
      | Ast_simplified.T_operator op ->
         let%bind op =
           Stage_common.Misc.bind_map_type_operator (* TODO: write Ast_simplified.Misc.type_operator_name *)
             (s_type_expression ~substs)
             op in
         (* TODO: when we have generalized operators, we might need to subst the operator name itself? *)
         ok @@ Ast_simplified.T_operator op
      | Ast_simplified.T_constant constant ->
         ok @@ Ast_simplified.T_constant constant

    and s_type_expression : Ast_simplified.type_expression w = fun ~substs {type_expression'} ->
      let%bind type_expression' = s_type_expression' ~substs type_expression' in
      ok @@ Ast_simplified.{type_expression'}

    and s_type_value : T.type_value w = fun ~substs { type_value'; simplified } ->
      let%bind type_value' = s_type_value' ~substs type_value' in
      let%bind simplified = bind_map_option (s_type_expression ~substs) simplified in
      ok @@ T.{ type_value'; simplified }
    and s_literal : T.literal w = fun ~substs -> function
      | T.Literal_unit ->
        let () = ignore @@ substs in
        ok @@ T.Literal_unit
      | (T.Literal_bool _ as x)
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
    and s_matching_expr : T.matching_expr w = fun ~substs _ ->
      let _TODO = substs in
      failwith "TODO: subst: unimplemented case s_matching"
    and s_named_type_value : T.named_type_value w = fun ~substs _ ->
      let _TODO = substs in
      failwith "TODO: subst: unimplemented case s_named_type_value"
    and s_access_path  : T.access_path w = fun ~substs _ ->
      let _TODO = substs in
      failwith "TODO: subst: unimplemented case s_access_path"

    and s_expression : T.expression w = fun ~(substs : substs) -> function
      | T.E_literal         x ->
        let%bind x = s_literal ~substs x in
        ok @@ T.E_literal x
      | T.E_constant        (var, vals) ->
        let%bind var = s_build_in ~substs var in
        let%bind vals = bind_map_list (s_annotated_expression ~substs) vals in
        ok @@ T.E_constant (var, vals)
      | T.E_variable        tv ->
        let%bind tv = s_variable ~substs tv in
        ok @@ T.E_variable tv
      | T.E_application     (val1 , val2) ->
        let%bind val1 = s_annotated_expression ~substs val1 in
        let%bind val2 = s_annotated_expression ~substs val2 in
        ok @@ T.E_application (val1 , val2)
      | T.E_lambda          { binder; body } ->
        let%bind binder = s_variable ~substs binder in
        let%bind body = s_annotated_expression ~substs body in
        ok @@ T.E_lambda { binder; body }
      | T.E_let_in          { binder; rhs; result; inline } ->
        let%bind binder = s_variable ~substs binder in
        let%bind rhs = s_annotated_expression ~substs rhs in
        let%bind result = s_annotated_expression ~substs result in
        ok @@ T.E_let_in { binder; rhs; result; inline }
      | T.E_tuple           vals ->
        let%bind vals = bind_map_list (s_annotated_expression ~substs) vals in
        ok @@ T.E_tuple vals
      | T.E_tuple_accessor  (val_, i) ->
        let%bind val_ = s_annotated_expression ~substs val_ in
        let i = i in
        ok @@ T.E_tuple_accessor (val_, i)
      | T.E_constructor     (tvar, val_) ->
        let%bind tvar = s_constructor ~substs tvar in
        let%bind val_ = s_annotated_expression ~substs val_ in
        ok @@ T.E_constructor (tvar, val_)
      | T.E_record          aemap ->
        let _TODO = aemap in
        failwith "TODO: subst in record"
        (* let%bind aemap = TSMap.bind_map_Map (fun ~k:key ~v:val_ ->
         *     let key = s_type_variable ~substs key in
         *     let val_ = s_annotated_expression ~substs val_ in
         *     ok @@ (key , val_)) aemap in
         * ok @@ T.E_record aemap *)
      | T.E_record_accessor (val_, l) ->
        let%bind val_ = s_annotated_expression ~substs val_ in
        let l = l in            (* Nothing to substitute, this is a label, not a type *)
        ok @@ T.E_record_accessor (val_, l)
      | T.E_record_update (r, ups) ->
        let%bind r = s_annotated_expression ~substs r in
        let%bind ups = bind_map_list (fun (l,e) -> let%bind e = s_annotated_expression ~substs e in ok (l,e)) ups in
        ok @@ T.E_record_update (r,ups)
      | T.E_map             val_val_list ->
        let%bind val_val_list = bind_map_list (fun (val1 , val2) ->
            let%bind val1 = s_annotated_expression ~substs val1 in
            let%bind val2 = s_annotated_expression ~substs val2 in
            ok @@ (val1 , val2)
          ) val_val_list in
        ok @@ T.E_map val_val_list
      | T.E_big_map         val_val_list ->
        let%bind val_val_list = bind_map_list (fun (val1 , val2) ->
            let%bind val1 = s_annotated_expression ~substs val1 in
            let%bind val2 = s_annotated_expression ~substs val2 in
            ok @@ (val1 , val2)
          ) val_val_list in
        ok @@ T.E_big_map val_val_list
      | T.E_list            vals ->
        let%bind vals = bind_map_list (s_annotated_expression ~substs) vals in
        ok @@ T.E_list vals
      | T.E_set             vals ->
        let%bind vals = bind_map_list (s_annotated_expression ~substs) vals in
        ok @@ T.E_set vals
      | T.E_look_up         (val1, val2) ->
        let%bind val1 = s_annotated_expression ~substs val1 in
        let%bind val2 = s_annotated_expression ~substs val2 in
        ok @@ T.E_look_up (val1 , val2)
      | T.E_matching         (val_ , matching_expr) ->
        let%bind val_ = s_annotated_expression ~substs val_ in
        let%bind matching = s_matching_expr ~substs matching_expr in
        ok @@ T.E_matching (val_ , matching)
      | T.E_sequence        (val1, val2) ->
        let%bind val1 = s_annotated_expression ~substs val1 in
        let%bind val2 = s_annotated_expression ~substs val2 in
        ok @@ T.E_sequence (val1 , val2)
      | T.E_loop            (val1, val2) ->
        let%bind val1 = s_annotated_expression ~substs val1 in
        let%bind val2 = s_annotated_expression ~substs val2 in
        ok @@ T.E_loop (val1 , val2)
      | T.E_assign          (named_tval, access_path, val_) ->
        let%bind named_tval = s_named_type_value ~substs named_tval in
        let%bind access_path = s_access_path ~substs access_path in
        let%bind val_ = s_annotated_expression ~substs val_ in
        ok @@ T.E_assign (named_tval, access_path, val_)

    and s_annotated_expression : T.annotated_expression w = fun ~substs { expression; type_annotation; environment; location } ->
      let%bind expression = s_expression ~substs expression in
      let%bind type_annotation = s_type_value ~substs type_annotation in
      let%bind environment = s_full_environment ~substs environment in
      let location = location in
      ok T.{ expression; type_annotation; environment; location }

    and s_named_expression : T.named_expression w = fun ~substs { name; annotated_expression } ->
      let name = name in (* Nothing to substitute, this is a variable name *)
      let%bind annotated_expression = s_annotated_expression ~substs annotated_expression in
      ok T.{ name; annotated_expression }

    and s_declaration : T.declaration w = fun ~substs ->
      function
        Ast_typed.Declaration_constant (e, inline, (env1, env2)) ->
        let%bind e = s_named_expression ~substs e in
        let%bind env1 = s_full_environment ~substs env1 in
        let%bind env2 = s_full_environment ~substs env2 in
        ok @@ Ast_typed.Declaration_constant (e, inline, (env1, env2))

    and s_declaration_wrap : T.declaration Location.wrap w = fun ~substs d ->
      Trace.bind_map_location (s_declaration ~substs) d

    (* Replace the type variable ~v with ~expr everywhere within the
       program ~p. TODO: issues with scoping/shadowing. *)
    and s_program : Ast_typed.program w = fun ~substs p ->
      Trace.bind_map_list (s_declaration_wrap ~substs) p

    (*
       Computes `P[v := expr]`.
    *)
    and type_value ~tv ~substs =
      let self tv = type_value ~tv ~substs in
      let (v, expr) = substs in
      match tv with
      | P_variable v' when String.equal v' v -> expr
      | P_variable _ -> tv
      | P_constant (x , lst) -> (
          let lst' = List.map self lst in
          P_constant (x , lst')
        )
      | P_apply ab -> (
          let ab' = pair_map self ab in
          P_apply ab'
        )
      | P_forall p -> (
          let aux c = constraint_ ~c ~substs in
          let constraints = List.map aux p.constraints in
          if (p.binder = v) then (
            P_forall { p with constraints }
          ) else (
            let body = self p.body in
            P_forall { p with constraints ; body }
          )
        )

    and constraint_ ~c ~substs =
      match c with
      | C_equation ab -> (
          let ab' = pair_map (fun tv -> type_value ~tv ~substs) ab in
          C_equation ab'
        )
      | C_typeclass (tvs , tc) -> (
          let tvs' = List.map (fun tv -> type_value ~tv ~substs) tvs in
          let tc' = typeclass ~tc ~substs in
          C_typeclass (tvs' , tc')
        )
      | C_access_label (tv , l , v') -> (
          let tv' = type_value ~tv ~substs in
          C_access_label (tv' , l , v')
        )

    and typeclass ~tc ~substs =
      List.map (List.map (fun tv -> type_value ~tv ~substs)) tc

    let program = s_program

    (* Performs beta-reduction at the root of the type *)
    let eval_beta_root ~(tv : type_value) =
      match tv with
        P_apply (P_forall { binder; constraints; body }, arg) ->
        let constraints = List.map (fun c -> constraint_ ~c ~substs:(mk_substs ~v:binder ~expr:arg)) constraints in
        (type_value ~tv:body ~substs:(mk_substs ~v:binder ~expr:arg) , constraints)
      | _ -> (tv , [])
  end

end
