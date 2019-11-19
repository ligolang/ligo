open Core

let pair_map = fun f (x , y) -> (f x , f y)

module Substitution = struct

  module Pattern = struct

    open Trace
    module T = Ast_typed
    module TSMap = Trace.TMap(String)

    type 'a w = 'a -> 'a result

    let rec rec_yes = true
    and s_environment_element_definition ~v ~expr = function
      | T.ED_binder -> ok @@ T.ED_binder
      | T.ED_declaration (val_, free_variables) ->
        let%bind val_ = s_annotated_expression ~v ~expr  val_ in
        let%bind free_variables = bind_map_list (s_type_variable ~v ~expr) free_variables in
        ok @@ T.ED_declaration (val_, free_variables)
    and s_environment ~v ~expr = fun lst ->
      bind_map_list (fun (type_variable, T.{ type_value; source_environment; definition }) ->
          let _ = type_value in
          let%bind type_variable = s_type_variable ~v ~expr type_variable in
          let%bind type_value = s_type_value ~v ~expr type_value in
          let%bind source_environment = s_full_environment ~v ~expr source_environment in
          let%bind definition = s_environment_element_definition ~v ~expr definition in
          ok @@ (type_variable, T.{ type_value; source_environment; definition })
        ) lst
    and s_type_environment ~v ~expr : T.type_environment w = fun tenv ->
      bind_map_list (fun (type_variable , type_value) ->
        let%bind type_variable = s_type_variable ~v ~expr type_variable in
        let%bind type_value = s_type_value ~v ~expr type_value in
        ok @@ (type_variable , type_value)) tenv
    and s_small_environment ~v ~expr : T.small_environment w = fun (environment, type_environment) ->
      let%bind environment = s_environment ~v ~expr environment in
      let%bind type_environment = s_type_environment ~v ~expr type_environment in
      ok @@ (environment, type_environment)
    and s_full_environment ~v ~expr : T.full_environment w = fun (a , b) ->
      let%bind a = s_small_environment ~v ~expr a in
      let%bind b = bind_map_list (s_small_environment ~v ~expr) b in
      ok (a , b)

    and s_variable ~v ~expr : T.name w = fun var ->
      let () = ignore (v, expr) in
      ok var

    and s_type_variable ~v ~expr : T.name w = fun tvar ->
      let _TODO = ignore (v, expr) in
      Printf.printf "TODO: subst: unimplemented case s_type_variable";
      ok @@ tvar
      (* if String.equal tvar v then
       *   expr
       * else
       *   ok tvar *)

    and s_type_name_constant ~v ~expr : T.type_name w = fun type_name ->
      (* TODO: we don't need to subst anything, right? *)
      let () = ignore (v , expr) in
      ok @@ type_name

    and s_type_value' ~v ~expr : T.type_value' w = function
        | T.T_tuple type_value_list ->
          let%bind type_value_list = bind_map_list (s_type_value ~v ~expr) type_value_list in
          ok @@ T.T_tuple type_value_list
        | T.T_sum _ -> failwith "TODO: T_sum"
        | T.T_record _ -> failwith "TODO: T_record"
        | T.T_constant (type_name, type_value_list) ->
          let%bind type_name = s_type_name_constant ~v ~expr type_name in
          let%bind type_value_list = bind_map_list (s_type_value ~v ~expr) type_value_list in
          ok @@ T.T_constant (type_name, type_value_list)
        | T.T_variable _ -> failwith "TODO: T_variable"
        | T.T_function _ ->
          let _TODO = (v, expr) in
          failwith "TODO: T_function"

    and s_type_expression ~v ~expr : Ast_simplified.type_expression w = function
      | Ast_simplified.T_tuple _ -> failwith "TODO: subst: unimplemented case s_type_expression"
      | Ast_simplified.T_sum _ -> failwith "TODO: subst: unimplemented case s_type_expression"
      | Ast_simplified.T_record _ -> failwith "TODO: subst: unimplemented case s_type_expression"
      | Ast_simplified.T_function (_, _) -> failwith "TODO: subst: unimplemented case s_type_expression"
      | Ast_simplified.T_variable _ -> failwith "TODO: subst: unimplemented case s_type_expression"
      | Ast_simplified.T_constant (_, _) ->
        let _TODO = (v, expr) in
        failwith "TODO: subst: unimplemented case s_type_expression"

    and s_type_value ~v ~expr : T.type_value w = fun { type_value'; simplified } ->
      let%bind type_value' = s_type_value' ~v ~expr type_value' in
      let%bind simplified = bind_map_option (s_type_expression ~v ~expr) simplified in
      ok @@ T.{ type_value'; simplified }
    and s_literal ~v ~expr : T.literal w = function
      | T.Literal_unit ->
        let () = ignore (v, expr) in
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
      | (T.Literal_operation _ as x) ->
        ok @@ x
    and s_matching_expr ~v ~expr : T.matching_expr w = fun _ ->
      let _TODO = v, expr in
      failwith "TODO: subst: unimplemented case s_matching"
    and s_named_type_value ~v ~expr : T.named_type_value w = fun _ ->
      let _TODO = v, expr in
      failwith "TODO: subst: unimplemented case s_named_type_value"
    and s_access_path  ~v ~expr : T.access_path w = fun _ ->
      let _TODO = v, expr in
      failwith "TODO: subst: unimplemented case s_access_path"

    and s_expression ~v ~expr : T.expression w = function
      | T.E_literal         x ->
        let%bind x = s_literal ~v ~expr x in
        ok @@ T.E_literal x
      | T.E_constant        (var, vals) ->
        let%bind var = s_variable ~v ~expr var in
        let%bind vals = bind_map_list (s_annotated_expression ~v ~expr) vals in
        ok @@ T.E_constant (var, vals)
      | T.E_variable        tv ->
        let%bind tv = s_variable ~v ~expr tv in
        ok @@ T.E_variable tv
      | T.E_application     (val1 , val2) ->
        let%bind val1 = s_annotated_expression ~v ~expr val1 in
        let%bind val2 = s_annotated_expression ~v ~expr val2 in
        ok @@ T.E_application (val1 , val2)
      | T.E_lambda          { binder; body } ->
        let%bind binder = s_variable ~v ~expr binder in
        let%bind body = s_annotated_expression ~v ~expr body in
        ok @@ T.E_lambda { binder; body }
      | T.E_let_in          { binder; rhs; result } ->
        let%bind binder = s_variable ~v ~expr binder in
        let%bind rhs = s_annotated_expression ~v ~expr rhs in
        let%bind result = s_annotated_expression ~v ~expr result in
        ok @@ T.E_let_in { binder; rhs; result }
      | T.E_tuple           vals ->
        let%bind vals = bind_map_list (s_annotated_expression ~v ~expr) vals in
        ok @@ T.E_tuple vals
      | T.E_tuple_accessor  (val_, i) ->
        let%bind val_ = s_annotated_expression ~v ~expr val_ in
        let i = i in
        ok @@ T.E_tuple_accessor (val_, i)
      | T.E_constructor     (tvar, val_) ->
        let%bind tvar = s_type_variable ~v ~expr tvar in
        let%bind val_ = s_annotated_expression ~v ~expr val_ in
        ok @@ T.E_constructor (tvar, val_)
      | T.E_record          aemap ->
        let _TODO = aemap in
        failwith "TODO: subst in record"
        (* let%bind aemap = TSMap.bind_map_Map (fun ~k:key ~v:val_ ->
         *     let key = s_type_variable ~v ~expr key in
         *     let val_ = s_annotated_expression ~v ~expr val_ in
         *     ok @@ (key , val_)) aemap in
         * ok @@ T.E_record aemap *)
      | T.E_record_accessor (val_, tvar) ->
        let%bind val_ = s_annotated_expression ~v ~expr val_ in
        let%bind tvar = s_type_variable ~v ~expr tvar in
        ok @@ T.E_record_accessor (val_, tvar)
      | T.E_map             val_val_list ->
        let%bind val_val_list = bind_map_list (fun (val1 , val2) ->
            let%bind val1 = s_annotated_expression ~v ~expr val1 in
            let%bind val2 = s_annotated_expression ~v ~expr val2 in
            ok @@ (val1 , val2)
          ) val_val_list in
        ok @@ T.E_map val_val_list
      | T.E_big_map         val_val_list ->
        let%bind val_val_list = bind_map_list (fun (val1 , val2) ->
            let%bind val1 = s_annotated_expression ~v ~expr val1 in
            let%bind val2 = s_annotated_expression ~v ~expr val2 in
            ok @@ (val1 , val2)
          ) val_val_list in
        ok @@ T.E_big_map val_val_list
      | T.E_list            vals ->
        let%bind vals = bind_map_list (s_annotated_expression ~v ~expr) vals in
        ok @@ T.E_list vals
      | T.E_set             vals ->
        let%bind vals = bind_map_list (s_annotated_expression ~v ~expr) vals in
        ok @@ T.E_set vals
      | T.E_look_up         (val1, val2) ->
        let%bind val1 = s_annotated_expression ~v ~expr val1 in
        let%bind val2 = s_annotated_expression ~v ~expr val2 in
        ok @@ T.E_look_up (val1 , val2)
      | T.E_matching         (val_ , matching_expr) ->
        let%bind val_ = s_annotated_expression ~v ~expr val_ in
        let%bind matching = s_matching_expr ~v ~expr matching_expr in
        ok @@ T.E_matching (val_ , matching)
      | T.E_sequence        (val1, val2) ->
        let%bind val1 = s_annotated_expression ~v ~expr val1 in
        let%bind val2 = s_annotated_expression ~v ~expr val2 in
        ok @@ T.E_sequence (val1 , val2)
      | T.E_loop            (val1, val2) ->
        let%bind val1 = s_annotated_expression ~v ~expr val1 in
        let%bind val2 = s_annotated_expression ~v ~expr val2 in
        ok @@ T.E_loop (val1 , val2)
      | T.E_assign          (named_tval, access_path, val_) ->
        let%bind named_tval = s_named_type_value ~v ~expr named_tval in
        let%bind access_path = s_access_path ~v ~expr access_path in
        let%bind val_ = s_annotated_expression ~v ~expr val_ in
        ok @@ T.E_assign (named_tval, access_path, val_)

    and s_annotated_expression ~v ~expr : T.annotated_expression w = fun { expression; type_annotation; environment; location } ->
      let%bind expression = s_expression ~v ~expr expression in
      let%bind type_annotation = s_type_value ~v ~expr type_annotation in
      let%bind environment = s_full_environment ~v ~expr environment in
      let location = location in
      ok T.{ expression; type_annotation; environment; location }

    and s_named_expression ~v ~expr : T.named_expression w = fun { name; annotated_expression } ->
      let%bind name = s_type_variable ~v ~expr name in
      let%bind annotated_expression = s_annotated_expression ~v ~expr annotated_expression in
      ok T.{ name; annotated_expression }

    and s_declaration ~v ~expr : T.declaration w =
      function
        Ast_typed.Declaration_constant (e, (env1, env2)) ->
        let%bind e = s_named_expression ~v ~expr e in
        let%bind env1 = s_full_environment ~v ~expr env1 in
        let%bind env2 = s_full_environment ~v ~expr env2 in
        ok @@ Ast_typed.Declaration_constant (e, (env1, env2))

    and s_declaration_wrap ~v ~expr : T.declaration Location.wrap w = fun d ->
      Trace.bind_map_location (s_declaration ~v ~expr) d

    (* Replace the type variable ~v with ~expr everywhere within the
       program ~p. TODO: issues with scoping/shadowing. *)
    and program ~(p : Ast_typed.program) ~(v:string (* this string is a type_name or type_variable I think *)) ~expr : Ast_typed.program Trace.result =
      Trace.bind_map_list (s_declaration_wrap ~v ~expr) p

    (*
       Computes `P[v := expr]`.
    *)
    and type_value ~tv ~v ~expr =
      let self tv = type_value ~tv ~v ~expr in
      match tv with
      | P_variable v' when v' = v -> expr
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
          let aux c = constraint_ ~c ~v ~expr in
          let constraints = List.map aux p.constraints in
          if (p.binder = v) then (
            P_forall { p with constraints }
          ) else (
            let body = self p.body in
            P_forall { p with constraints ; body }
          )
        )

    and constraint_ ~c ~v ~expr =
      match c with
      | C_equation ab -> (
          let ab' = pair_map (fun tv -> type_value ~tv ~v ~expr) ab in
          C_equation ab'
        )
      | C_typeclass (tvs , tc) -> (
          let tvs' = List.map (fun tv -> type_value ~tv ~v ~expr) tvs in
          let tc' = typeclass ~tc ~v ~expr in
          C_typeclass (tvs' , tc')
        )
      | C_access_label (tv , l , v') -> (
          let tv' = type_value ~tv ~v ~expr in
          C_access_label (tv' , l , v')
        )

    and typeclass ~tc ~v ~expr =
      List.map (List.map (fun tv -> type_value ~tv ~v ~expr)) tc

    (* Performs beta-reduction at the root of the type *)
    let eval_beta_root ~(tv : type_value) =
      match tv with
        P_apply (P_forall { binder; constraints; body }, arg) ->
        let constraints = List.map (fun c -> constraint_ ~c ~v:binder ~expr:arg) constraints in
        (type_value ~tv:body ~v:binder ~expr:arg , constraints)
      | _ -> (tv , [])
  end

end
