module List = Simple_utils.List
open Simple_utils.Trace
module Errors = Errors
open Errors
open Ligo_prim
module Signature = Context.Signature
module I = Ast_core
module O = Ast_typed
module C = Computation
module E = Elaboration

(* 
let debug = false
let assertions = false *)
let untype_expression = Untyper.untype_expression
let untype_type_expression = Untyper.untype_type_expression
let untype_program = Untyper.untype_program

let assert_type_expression_eq ~raise (loc : Location.t) (type1, type2) : unit =
  trace_option ~raise (assert_equal type1 type2 loc)
  @@ O.assert_type_expression_eq (type1, type2)


(*
  This function operates on the return type of Context.get_sum.
  If type match the constructor label and its argument type, warns user about ambiguous constructor
*)
let warn_ambiguous_constructor ~warning ~tvar:var_chosen ~arg_type ignored
    : (unit, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let ignored_match =
    List.find ~f:(fun (_, _, arg_type', _) -> Type.equal arg_type arg_type') ignored
  in
  match ignored_match with
  | Some (var_ignored, _, _, _) -> warn (warning var_chosen var_ignored)
  | None -> return ()


let warn_ambiguous_constructor_expr ~expr ~tvar ~arg_type ignored =
  warn_ambiguous_constructor
    ignored
    ~tvar
    ~arg_type
    ~warning:(fun var_chosen var_ignored loc ->
      `Checking_ambiguous_constructor_expr (expr, var_chosen, var_ignored, loc))


let warn_ambiguous_constructor_pat ~pat ~tvar ~arg_type ignored =
  warn_ambiguous_constructor
    ignored
    ~tvar
    ~arg_type
    ~warning:(fun var_chosen var_ignored loc ->
      `Checking_ambiguous_constructor_pat (pat, var_chosen, var_ignored, loc))


let check_let_annomalies ~syntax binder type_expr =
  Elaboration.check_anomalies
    ~loc:(Location.get_location binder)
    ~syntax
    [ binder, type_expr ]
    type_expr


let rec evaluate_type ~default_layout (type_ : I.type_expression)
    : (Type.t, 'err, 'wrn) C.t
  =
  let open C in
  let open Let_syntax in
  set_loc type_.location
  @@
  let const content =
    let%bind loc = loc () in
    return @@ Type.make_t ~loc content (Some type_)
  in
  let lift type_ =
    let%bind loc = loc () in
    return @@ Type.{ type_ with location = loc }
  in
  match type_.type_content with
  | T_arrow { type1; type2 } ->
    let%bind type1 = evaluate_type ~default_layout type1 in
    let%bind type2 = evaluate_type ~default_layout type2 in
    const @@ T_arrow { type1; type2 }
  | T_sum row ->
    let%bind row = evaluate_row ~default_layout row in
    const @@ T_sum row
  | T_record row ->
    let%bind row = evaluate_row ~default_layout row in
    const @@ T_record row
  | T_variable tvar ->
    (* Get the closest type or type variable with type var [tvar] *)
    (match%bind
       Context.get_type_or_type_var_exn tvar ~error:(unbound_type_variable tvar)
     with
    | `Type type_ -> lift type_
    | `Type_var _kind -> const @@ T_variable tvar)
  | T_app { type_operator = { module_path; element = type_operator }; arguments } ->
    (* TODO: Remove strong normalization (GA) *)
    (* 1. Find the type of the operator *)
    let%bind operator =
      (* Depending on whether there is a path or not, look up in current context or module sig. *)
      match module_path with
      | [] ->
        Context.get_type_exn type_operator ~error:(unbound_type_variable type_operator)
      | _ ->
        let%bind sig_ =
          Context.get_signature_exn
            (List.Ne.of_list module_path)
            ~error:(unbound_module module_path)
        in
        raise_opt ~error:(unbound_type_variable type_operator)
        @@ Signature.get_type sig_ type_operator
    in
    (* 2. Evaluate arguments *)
    let%bind arguments =
      arguments |> List.map ~f:(evaluate_type ~default_layout) |> all
    in
    (* 3. Kind check (TODO: Improve this, currently just checks if kind is Type for args) *)
    let%bind () =
      arguments
      |> List.map ~f:(fun arg ->
             match%bind Context.Well_formed.type_ arg with
             | Some (Type | Singleton) -> return ()
             | _ -> raise (ill_formed_type arg))
      |> all_unit
    in
    (* 4. Beta-reduce *)
    let vars, ty_body = Type.destruct_type_abstraction operator in
    let%bind vargs =
      match List.zip vars arguments with
      | Unequal_lengths ->
        let actual = List.length arguments in
        let expected = List.length vars in
        raise (type_app_wrong_arity (Some type_operator) expected actual)
      | Ok vargs -> return vargs
    in
    let result =
      List.fold_right vargs ~init:ty_body ~f:(fun (tvar, type_) result ->
          Type.subst result ~tvar ~type_)
    in
    const result.content
  | T_module_accessor { module_path; element } ->
    let%bind sig_ =
      Context.get_signature_exn
        (List.Ne.of_list module_path)
        ~error:(unbound_module module_path)
    in
    raise_opt ~error:(unbound_type_variable element) @@ Signature.get_type sig_ element
  | T_singleton lit -> const @@ T_singleton lit
  | T_abstraction { ty_binder; kind; type_ } ->
    let%bind type_, () =
      def_type_var
        [ ty_binder, kind ]
        ~on_exit:Lift_type
        ~in_:(evaluate_type ~default_layout type_ >>| fun type_ -> type_, ())
    in
    const @@ T_abstraction { ty_binder; kind; type_ }
  | T_for_all { ty_binder; kind; type_ } ->
    let%bind type_, () =
      def_type_var
        [ ty_binder, kind ]
        ~on_exit:Lift_type
        ~in_:(evaluate_type ~default_layout type_ >>| fun type_ -> type_, ())
    in
    const @@ T_for_all { ty_binder; kind; type_ }


and evaluate_row ~default_layout ({ fields; layout } : I.rows)
    : (Type.row, 'err, 'wrn) C.t
  =
  let open C in
  let open Let_syntax in
  let%bind layout =
    match layout with
    | Some layout -> return @@ evaluate_layout layout
    | None -> default_layout ()
  in
  let%bind fields =
    fields |> Record.map ~f:(evaluate_row_elem ~default_layout) |> all_lmap
  in
  return { Type.fields; layout }


and evaluate_layout (layout : Layout.t) : Type.layout =
  match layout with
  | L_tree -> L_tree
  | L_comb -> L_comb


and evaluate_row_elem ~default_layout (row_elem : I.row_element)
    : (Type.row_element, 'err, 'wrn) C.t
  =
  let open C in
  let open Let_syntax in
  let%map associated_type = evaluate_type ~default_layout row_elem.associated_type in
  { row_elem with associated_type }


let evaluate_type_with_lexists type_ =
  let open C in
  evaluate_type ~default_layout:lexists type_


let evaluate_type_with_default_layout type_ =
  let open C in
  evaluate_type ~default_layout:(fun () -> return Type.default_layout) type_


let infer_value_attr : I.ValueAttr.t -> O.ValueAttr.t =
 fun { inline; no_mutation; view; public; hidden; thunk } ->
  { inline; no_mutation; view; public; hidden; thunk }


let infer_literal lit : (Type.t * O.expression E.t, _, _) C.t =
  let open C in
  let open Let_syntax in
  let const constr =
    let%bind loc = loc () in
    let%bind type_ = create_type constr in
    return
      ( type_
      , E.(
          let%bind type_ = decode type_ in
          return @@ O.make_e ~loc (E_literal lit) type_) )
  in
  match lit with
  | Literal_unit -> const Type.t_unit
  | Literal_string _ -> const Type.t_string
  | Literal_key _ -> const Type.t_key
  | Literal_key_hash _ -> const Type.t_key_hash
  | Literal_chain_id _ -> const Type.t_chain_id
  | Literal_signature _ -> const Type.t_signature
  | Literal_bytes _ -> const Type.t_bytes
  | Literal_int _ -> const Type.t_int
  | Literal_nat _ -> const Type.t_nat
  | Literal_timestamp _ -> const Type.t_timestamp
  | Literal_mutez _ -> const Type.t_tez
  | Literal_address _ -> const Type.t_address
  | Literal_operation _ -> const Type.t_operation
  | Literal_bls12_381_g1 _ -> const Type.t_bls12_381_g1
  | Literal_bls12_381_g2 _ -> const Type.t_bls12_381_g2
  | Literal_bls12_381_fr _ -> const Type.t_bls12_381_fr
  | Literal_chest _ | Literal_chest_key _ ->
    raise
      (corner_case
         "chest / chest_key are not allowed in the syntax (only tests need this type)")


let equal_lmap_doms lmap1 lmap2 =
  let open Record in
  let dom lmap = LSet.of_list (LMap.keys lmap) in
  LSet.equal (dom lmap1) (dom lmap2)


let rec check_expression (expr : I.expression) (type_ : Type.t)
    : (O.expression E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let check expr type_ = check_expression expr type_ in
  let infer expr = infer_expression expr in
  let%bind () = hash_context () in
  set_loc expr.location
  @@
  let const content =
    let%bind loc = loc () in
    return
      E.(
        let%bind content = content in
        let%bind type_ = decode type_ in
        return @@ O.make_e ~loc content type_)
  in
  let const_with_type content type_ =
    let%bind loc = loc () in
    return
      E.(
        let%bind content = content in
        let%bind type_ = decode type_ in
        return @@ O.make_e ~loc content type_)
  in
  match expr.expression_content, type_.content with
  | E_literal lit, T_construct _ ->
    let%bind lit_type, expr = infer_literal lit in
    let%bind () =
      assert_ (Type.equal lit_type type_) ~error:(literal_type_mismatch lit_type type_)
    in
    return expr
  | ( E_type_abstraction { type_binder = tvar; result }
    , T_for_all { ty_binder = tvar'; kind; type_ } ) ->
    let type_ = Type.subst_var type_ ~tvar:tvar' ~tvar':tvar in
    let%bind result =
      def_type_var [ tvar, kind ] ~on_exit:Drop ~in_:(check result type_)
    in
    let%bind type_ = create_type @@ Type.t_for_all { ty_binder = tvar; kind; type_ } in
    const_with_type
      E.(
        let%bind result = result in
        return @@ O.E_type_abstraction { type_binder = tvar; result })
      type_
  | _, T_for_all { ty_binder = tvar; kind; type_ } ->
    let%bind tvar' = fresh_type_var () in
    let%bind result =
      def_type_var
        [ tvar', kind ]
        ~on_exit:Drop
        ~in_:(check expr (Type.subst_var type_ ~tvar ~tvar'))
    in
    const
      E.(
        let%bind result = result in
        return @@ O.E_type_abstraction { type_binder = tvar; result })
  | E_lambda lambda, T_arrow { type1 = arg_type; type2 = ret_type } ->
    let%bind lambda = check_lambda lambda arg_type ret_type in
    const
      E.(
        let%bind lambda = lambda in
        return @@ O.E_lambda lambda)
  | E_record record, T_record row ->
    let%bind () =
      assert_ (equal_lmap_doms record row.fields) ~error:(record_mismatch expr type_)
    in
    let%bind record =
      record
      |> Record.LMap.mapi (fun label expr ->
             let type_ = Record.LMap.find label row.fields in
             check expr type_.associated_type)
      |> all_lmap
    in
    const
      E.(
        let%bind record = all_lmap record in
        return @@ O.E_record record)
  | E_update { struct_; path; update }, T_record row ->
    let%bind struct_ = check struct_ type_ in
    let%bind field_row_elem =
      raise_opt ~error:(bad_record_access path) @@ Record.LMap.find_opt path row.fields
    in
    let%bind update = check update field_row_elem.associated_type in
    const
      E.(
        let%bind struct_ = struct_
        and update = update in
        return @@ O.E_update { struct_; path; update })
  | E_constructor { constructor; element }, T_sum row ->
    let%bind constr_row_elem =
      raise_opt ~error:(bad_constructor constructor type_)
      @@ Record.LMap.find_opt constructor row.fields
    in
    let%bind element = check element constr_row_elem.associated_type in
    const
      E.(
        let%bind element = element in
        return @@ O.E_constructor { constructor; element })
  | E_matching { matchee; cases }, _ ->
    let%bind matchee_type, matchee = infer matchee in
    let%bind matchee_type = Context.tapply matchee_type in
    let%bind cases = check_cases cases matchee_type type_ in
    let%bind match_expr = compile_match matchee cases matchee_type in
    const match_expr
  | _ ->
    let%bind type_', expr = infer expr in
    let%bind f =
      let%bind type_' = Context.tapply type_' in
      let%bind type_ = Context.tapply type_ in
      subtype ~received:type_' ~expected:type_
    in
    return
      E.(
        let%bind expr = expr in
        f expr)


and infer_expression (expr : I.expression) : (Type.t * O.expression E.t, _, _) C.t =
  let open C in
  let open Let_syntax in
  let check expr type_ = check_expression expr type_ in
  let infer expr = infer_expression expr in
  let%bind () = hash_context () in
  set_loc expr.location
  @@
  let lift expr type_ =
    let%map loc = loc () in
    E.(
      let%map expr = expr
      and type_ = decode type_ in
      O.make_e ~loc expr.O.expression_content type_)
  in
  let const content type_ =
    let%bind loc = loc () in
    return
      ( type_
      , E.(
          let%bind content = content in
          let%bind type_ = decode type_ in
          return @@ O.make_e ~loc content type_) )
  in
  let%bind syntax = Options.syntax () in
  match expr.expression_content with
  | E_literal lit -> infer_literal lit
  | E_constant { cons_name = const; arguments = args } -> infer_constant const args
  | E_variable var ->
    let%bind mut_flag, type_ =
      Context.get_value_exn var ~error:(function
          | `Not_found -> unbound_variable var
          | `Mut_var_captured -> mut_var_captured var)
    in
    const
      E.(
        match mut_flag with
        | Immutable -> return (O.E_variable var)
        | Mutable -> return (O.E_deref var))
      type_
  | E_lambda lambda -> infer_lambda lambda
  | E_application { lamb; args } ->
    let%bind lamb_type, lamb = infer lamb in
    let%bind ret_type, f, args =
      let%bind lamb_type = Context.tapply lamb_type in
      infer_application lamb_type args
    in
    const
      E.(
        let%bind lamb = lamb >>= f in
        let%bind args = args in
        return (O.E_application { lamb; args }))
      ret_type
  | E_type_abstraction { type_binder = tvar; result } ->
    generalize_expr
    @@ let%bind ret_type, expr =
         def_type_var [ tvar, Type ] ~on_exit:Lift_type ~in_:(infer result)
       in
       let%bind ret_type =
         create_type @@ Type.t_for_all { ty_binder = tvar; kind = Type; type_ = ret_type }
       in
       const
         E.(
           let%bind expr = expr in
           return (O.E_type_abstraction { type_binder = tvar; result = expr }))
         ret_type
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let%bind rhs_type, rhs = infer rhs in
    let%bind rhs_type = Context.tapply rhs_type in
    let%bind frags, let_binder =
      With_frag.run @@ check_pattern ~mut:false let_binder rhs_type
    in
    let%bind res_type, let_result =
      def_frag frags ~on_exit:Lift_type ~in_:(infer let_result)
    in
    let attributes = infer_value_attr attributes in
    const
      E.(
        let%bind rhs_type = decode rhs_type in
        let%bind let_binder = let_binder in
        let%bind rhs = rhs
        and let_result = let_result in
        let%bind () = check_let_annomalies ~syntax let_binder rhs_type in
        return (O.E_let_in { let_binder; rhs; let_result; attributes }))
      res_type
  | E_type_in { type_binder = tvar; rhs; let_result } ->
    let%bind rhs = evaluate_type_with_default_layout rhs in
    let%bind res_type, let_result =
      def_type [ tvar, rhs ] ~on_exit:Lift_type ~in_:(infer let_result)
    in
    let%bind let_result = lift let_result res_type in
    return (res_type, let_result)
  | E_raw_code { language; code = { expression_content; _ } }
    when Option.is_some (I.get_e_tuple expression_content) ->
    let%bind loc = loc () in
    let exprs = Option.value_exn ~here:[%here] @@ I.get_e_tuple expression_content in
    let%bind code, args =
      match exprs with
      | [] -> raise (corner_case "expected non-empty tuple in %Michelson")
      | code :: args -> return (code, args)
    in
    let%bind code, code_type =
      raise_opt ~error:not_annotated @@ I.get_e_ascription code.expression_content
    in
    let%bind code_type = evaluate_type_with_lexists code_type in
    (* TODO: Shouldn't [code] have the type of [string]? *)
    let%bind _, code = infer code in
    let%bind args =
      args
      |> List.map ~f:(fun arg ->
             let%bind arg, arg_type =
               raise_opt ~error:not_annotated
               @@ I.get_e_ascription arg.I.expression_content
             in
             let%bind _, arg = infer arg in
             let%bind arg_type = evaluate_type_with_lexists arg_type in
             lift arg arg_type)
      |> all
    in
    const
      E.(
        let%bind code = code in
        let%bind args = all args in
        let%bind code_type = decode code_type in
        let tuple = O.e_a_record ~loc @@ Record.record_of_tuple (code :: args) in
        return
        @@ O.E_raw_code { language; code = { tuple with type_expression = code_type } })
      code_type
  | E_raw_code { language; code } ->
    let%bind code, code_type =
      raise_opt ~error:not_annotated @@ I.get_e_ascription code.expression_content
    in
    let%bind code_type = evaluate_type_with_lexists code_type in
    let%bind _, code = infer code in
    const
      E.(
        let%bind code = code
        and code_type = decode code_type in
        return
        @@ O.E_raw_code { language; code = { code with type_expression = code_type } })
      code_type
  | E_ascription { anno_expr; type_annotation } ->
    let%bind ascr = evaluate_type_with_lexists type_annotation in
    let%bind expr = check anno_expr ascr in
    let%bind expr = lift expr ascr in
    return (ascr, expr)
  | E_recursive { fun_name; fun_type; lambda } ->
    let%bind fun_type = evaluate_type_with_lexists fun_type in
    let%bind Arrow.{ type1 = arg_type; type2 = ret_type } =
      raise_opt
        ~error:(corner_case "Expected function type annotation for recursive function")
      @@ Type.get_t_arrow fun_type
    in
    let%bind lambda =
      def
        [ fun_name, Immutable, fun_type ]
        ~on_exit:Drop
        ~in_:(check_lambda (Lambda.map Fn.id Option.some lambda) arg_type ret_type)
    in
    const
      E.(
        let%bind lambda = lambda
        and fun_type = decode fun_type in
        return @@ O.E_recursive { fun_name; fun_type; lambda })
      fun_type
  | E_record record ->
    let%bind field_types, record =
      Record.LMap.fold
        (fun label expr result ->
          let%bind fields, record = result in
          let%bind expr_type, expr = infer expr in
          let fields = Record.LMap.add label expr_type fields in
          let record = Record.LMap.add label expr record in
          return (fields, record))
        record
        (return (Record.LMap.empty, Record.LMap.empty))
    in
    let _, fields =
      (* No fold_mapi in utils :cry *)
      Record.LMap.fold_map field_types ~init:0 ~f:(fun label associated_type i ->
          let decl_pos =
            let (Label str) = label in
            match Int.of_string str with
            | i -> i
            | exception _ -> i
          in
          i + 1, { Rows.associated_type; michelson_annotation = None; decl_pos })
    in
    let%bind record_type =
      match%bind Context.get_record fields with
      | None ->
        let%bind layout = lexists () in
        create_type @@ Type.t_record { fields; layout }
      | Some (orig_var, row) -> create_type @@ Type.t_record_with_orig_var row ~orig_var
    in
    const
      E.(
        let%bind record = all_lmap record in
        return @@ O.E_record record)
      record_type
  | E_accessor { struct_; path = field } ->
    let%bind record_type, struct_ = infer struct_ in
    let%bind row =
      let%bind record_type = Context.tapply record_type in
      raise_opt ~error:(expected_record record_type) @@ Type.get_t_record record_type
    in
    let%bind field_row_elem =
      raise_opt ~error:(bad_record_access field) @@ Record.LMap.find_opt field row.fields
    in
    const
      E.(
        let%bind struct_ = struct_ in
        return @@ O.E_accessor { struct_; path = field })
      field_row_elem.associated_type
  | E_update { struct_; path; update } ->
    let%bind record_type, struct_ = infer struct_ in
    let%bind row =
      let%bind record_type = Context.tapply record_type in
      raise_opt ~error:(expected_record record_type) @@ Type.get_t_record record_type
    in
    let%bind field_row_elem =
      raise_opt ~error:(bad_record_access path) @@ Record.LMap.find_opt path row.fields
    in
    let%bind update = check update field_row_elem.associated_type in
    const
      E.(
        let%bind struct_ = struct_
        and update = update in
        return @@ O.E_update { struct_; path; update })
      record_type
  | E_constructor { constructor = Label label as constructor; _ }
    when String.(label = "M_right" || label = "M_left") ->
    raise (michelson_or_no_annotation constructor)
  | E_constructor { constructor; element = arg } ->
    let%bind tvars, arg_type, sum_type =
      match%bind Context.get_sum constructor with
      | [] -> raise (unbound_constructor constructor)
      | (tvar, tvars, arg_type, sum_type) :: other ->
        let%bind () = warn_ambiguous_constructor_expr ~expr ~tvar ~arg_type other in
        return (tvars, arg_type, sum_type)
    in
    let%bind subst =
      tvars
      |> List.map ~f:(fun tvar ->
             let%bind texists = exists Type in
             return (tvar, texists))
      |> all
    in
    let apply_subst type_ =
      List.fold_right subst ~init:type_ ~f:(fun (tvar, texists) type_ ->
          Type.subst type_ ~tvar ~type_:texists)
    in
    let arg_type = apply_subst arg_type in
    let sum_type = apply_subst sum_type in
    let%bind arg = check arg arg_type in
    const
      E.(
        let%bind arg = arg in
        return @@ O.E_constructor { constructor; element = arg })
      sum_type
  | E_matching { matchee; cases } ->
    let%bind matchee_type, matchee = infer matchee in
    let%bind ret_type = exists Type in
    let%bind matchee_type = Context.tapply matchee_type in
    let%bind cases = check_cases cases matchee_type ret_type in
    let%bind match_expr = compile_match matchee cases matchee_type in
    const match_expr ret_type
  | E_mod_in { module_binder = mvar; rhs; let_result } ->
    let%bind sig_, rhs = infer_module_expr rhs in
    let%bind ret_type, let_result =
      def_module [ mvar, sig_ ] ~on_exit:Lift_type ~in_:(infer let_result)
    in
    const
      E.(
        let%bind let_result = let_result
        and rhs = rhs in
        return @@ O.E_mod_in { module_binder = mvar; rhs; let_result })
      ret_type
  | E_module_accessor { module_path; element } ->
    let module_path' = List.Ne.of_list module_path in
    let%bind sig_ =
      Context.get_signature_exn module_path' ~error:(unbound_module module_path)
    in
    let%bind elt_type =
      raise_opt ~error:(unbound_variable element) @@ Signature.get_value sig_ element
    in
    const E.(return @@ O.E_module_accessor { module_path; element }) elt_type
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let%bind rhs_type, rhs = infer rhs in
    let%bind rhs_type = Context.tapply rhs_type in
    let%bind frags, let_binder =
      With_frag.run @@ check_pattern ~mut:true let_binder rhs_type
    in
    let%bind res_type, let_result =
      def_frag frags ~on_exit:Lift_type ~in_:(infer let_result)
    in
    let attributes = infer_value_attr attributes in
    const
      E.(
        let%bind rhs_type = decode rhs_type in
        let%bind let_binder = let_binder in
        let%bind rhs = rhs
        and let_result = let_result in
        let%bind () = check_let_annomalies ~syntax let_binder rhs_type in
        return (O.E_let_mut_in { let_binder; rhs; let_result; attributes }))
      res_type
  | E_assign { binder; expression } ->
    let%bind type_ =
      let var = Binder.get_var binder in
      set_loc (Value_var.get_location var)
      @@ Context.get_mut_exn var ~error:(function
             | `Not_found -> unbound_mut_variable var
             | `Mut_var_captured -> mut_var_captured var)
    in
    let%bind type_ = Context.tapply type_ in
    let%bind expression = check expression type_ in
    let%bind ret_type = create_type Type.t_unit in
    const
      E.(
        let%bind expression = expression
        and type_ = decode type_ in
        return @@ O.E_assign { binder = Binder.set_ascr binder type_; expression })
      ret_type
  | E_for { binder; start; final; incr; f_body } ->
    let%bind t_int = create_type Type.t_int in
    let%bind t_unit = create_type Type.t_unit in
    let%bind start = check start t_int in
    let%bind incr = check incr t_int in
    let%bind final = check final t_int in
    let%bind f_body =
      def [ binder, Immutable, t_int ] ~on_exit:Drop ~in_:(check f_body t_unit)
    in
    const
      E.(
        let%bind start = start
        and final = final
        and incr = incr
        and f_body = f_body in
        return @@ O.E_for { binder; start; final; incr; f_body })
      t_unit
  | E_for_each
      { fe_binder = (key_binder, Some val_binder) as fe_binder
      ; collection
      ; collection_type = (Map | Any) as collection_type
      ; fe_body
      } ->
    let%bind t_unit = create_type Type.t_unit in
    let%bind type_, collection = infer collection in
    let%bind type_ = Context.tapply type_ in
    let%bind key_type, val_type =
      raise_opt ~error:(mismatching_for_each_collection_type collection_type type_)
      @@ Type.get_t_map type_
    in
    let%bind fe_body =
      def
        [ key_binder, Immutable, key_type; val_binder, Immutable, val_type ]
        ~on_exit:Drop
        ~in_:(check fe_body t_unit)
    in
    const
      E.(
        let%bind collection = collection
        and fe_body = fe_body in
        return @@ O.E_for_each { fe_binder; collection; collection_type = Map; fe_body })
      t_unit
  | E_for_each { fe_binder = _, Some _; collection_type = List | Set; _ } ->
    raise (mismatching_for_each_binder_arity 1 2)
  | E_for_each
      { fe_binder = (binder, None) as fe_binder; collection; collection_type; fe_body } ->
    let%bind t_unit = create_type Type.t_unit in
    let%bind type_, collection = infer collection in
    let%bind (binder_type : Type.t) =
      let opt_value_exn opt =
        raise_opt ~error:(mismatching_for_each_collection_type collection_type type_) opt
      in
      (* This is bad -- TODO: get rid of collection type (no-longer required) + use patterns *)
      let get_t_map type_ =
        let%bind type1, type2 = opt_value_exn @@ Type.get_t_map type_ in
        create_type (Type.t_tuple [ type1; type2 ])
      in
      match (collection_type : For_each_loop.collect_type) with
      | Set -> opt_value_exn @@ Type.get_t_set type_
      | List -> opt_value_exn @@ Type.get_t_list type_
      | Map -> get_t_map type_
      | Any ->
        try_
          (opt_value_exn
          @@ Option.first_some (Type.get_t_set type_) (Type.get_t_list type_))
          ~with_:(fun _ -> get_t_map type_)
    in
    let%bind fe_body =
      def [ binder, Immutable, binder_type ] ~on_exit:Drop ~in_:(check fe_body t_unit)
    in
    const
      E.(
        let%bind collection = collection
        and fe_body = fe_body in
        return @@ O.E_for_each { fe_binder; collection; collection_type = Map; fe_body })
      t_unit
  | E_while { cond; body } ->
    let%bind t_unit = create_type Type.t_unit in
    let%bind t_bool = create_type Type.t_bool in
    let%bind cond = check cond t_bool in
    let%bind body = check body t_unit in
    const
      E.(
        let%bind cond = cond
        and body = body in
        return @@ O.E_while { cond; body })
      t_unit
  | E_originate _ | E_contract_call _ ->
    (* TODO: Contracts *)
    assert false


and check_lambda
    ({ binder; output_type = ret_ascr; result } : _ Lambda.t)
    arg_type
    ret_type
    : (_ Lambda.t E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let%bind loc = loc () in
  let result =
    Option.value_map ret_ascr ~default:result ~f:(fun ret_ascr ->
        I.e_ascription ~loc result ret_ascr)
  in
  let%bind arg_type, f =
    match Param.get_ascr binder with
    | Some arg_ascr ->
      let%bind arg_ascr = evaluate_type_with_default_layout arg_ascr in
      (* TODO: Kinding check for ascription *)
      let%bind _f = subtype ~received:arg_type ~expected:arg_ascr in
      (* TODO: Generate let binding for ascription subtyping, will be inlined later on *)
      return (arg_ascr, E.return)
    | None -> return (arg_type, E.return)
  in
  let%bind result =
    Context.lock
      ~on_exit:Drop
      ~in_:
        (def
           [ Param.get_var binder, Param.get_mut_flag binder, arg_type ]
           ~on_exit:Drop
           ~in_:(check_expression result ret_type))
  in
  return
  @@ E.(
       let%bind result = result >>= f
       and arg_type = decode arg_type
       and ret_type = decode ret_type in
       return
       @@ Lambda.
            { binder = Param.set_ascr binder arg_type; result; output_type = ret_type })


and infer_lambda ({ binder; output_type = ret_ascr; result } : _ Lambda.t)
    : (Type.t * O.expression E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let%bind loc = loc () in
  let const content type_ =
    return
      ( type_
      , E.(
          let%bind content = content in
          let%bind type_ = decode type_ in
          return @@ O.make_e ~loc content type_) )
  in
  (* Desugar return ascription to (result : ret_ascr) *)
  let result =
    Option.value_map ret_ascr ~default:result ~f:(fun ret_ascr ->
        I.e_ascription ~loc result ret_ascr)
  in
  generalize_expr
  @@ Context.lock
       ~on_exit:Lift_type
       ~in_:
         (let%bind arg_type =
            match Param.get_ascr binder with
            | Some arg_ascr -> evaluate_type_with_lexists arg_ascr
            | None -> exists Type
          in
          let%bind ret_type, result =
            def
              [ Param.get_var binder, Param.get_mut_flag binder, arg_type ]
              ~on_exit:Lift_type
              ~in_:(infer_expression result)
          in
          let%bind type_ =
            create_type @@ Type.t_arrow { type1 = arg_type; type2 = ret_type }
          in
          const
            E.(
              let%bind result = result
              and arg_type = decode arg_type
              and ret_type = decode ret_type in
              return
              @@ O.E_lambda
                   { binder = Param.set_ascr binder arg_type
                   ; result
                   ; output_type = ret_type
                   })
            type_)


and generalize_expr (in_ : (Type.t * O.expression E.t, 'err, 'wrn) C.t)
    : (Type.t * O.expression E.t, 'err, 'wrn) C.t
  =
  let open C in
  let open Let_syntax in
  let%bind loc = loc () in
  let%bind type_, tvars, expr = generalize in_ in
  let expr =
    E.(
      let%map expr = expr in
      List.fold_right tvars ~init:expr ~f:(fun (tvar, kind) expr ->
          O.e_type_abstraction
            ~loc
            { type_binder = tvar; result = expr }
            (O.t_for_all ~loc tvar kind expr.type_expression)))
  in
  return (type_, expr)


and infer_application (lamb_type : Type.t) (args : I.expression)
    : (Type.t * (O.expression -> O.expression E.t) * O.expression E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let fail () = raise (should_be_a_function_type lamb_type args) in
  match lamb_type.content with
  | T_for_all { ty_binder = tvar; kind; type_ } ->
    let%bind texists = exists kind in
    let lamb_type = Type.subst type_ ~tvar ~type_:texists in
    let%bind ret_type, f, args = infer_application lamb_type args in
    return
      ( ret_type
      , E.(
          fun hole ->
            let%bind texists = decode texists in
            let%bind lamb_type = decode lamb_type in
            f
              (O.make_e
                 ~loc:hole.O.location
                 (E_type_inst { forall = hole; type_ = texists })
                 lamb_type))
      , args )
  | T_arrow { type1 = arg_type; type2 = ret_type } ->
    let%bind args = check_expression args arg_type in
    let%bind ret_type =
      try_
        (let%bind ret_type = Context.tapply ret_type in
         match ret_type.content with
         | T_construct { constructor = External "int"; parameters; _ } ->
           Constant_typers.External_types.int_types parameters
         | T_construct { constructor = External ("ediv" | "u_ediv"); parameters; _ } ->
           Constant_typers.External_types.ediv_types parameters
         | T_construct { constructor = External ("and" | "u_and"); parameters; _ } ->
           Constant_typers.External_types.and_types parameters
         | _ -> return ret_type)
        ~with_:(fun _ -> return ret_type)
    in
    return (ret_type, E.return, args)
  | T_exists tvar ->
    let%bind tvar1 = exists Type in
    let%bind tvar2 = exists Type in
    let%bind arr = create_type @@ Type.t_arrow { type1 = tvar1; type2 = tvar2 } in
    let%bind () = unify_texists tvar arr in
    let%bind args = check_expression args tvar1 in
    return (tvar2, E.return, args)
  | _ -> fail ()


and infer_constant const args : (Type.t * O.expression E.t, _, _) C.t =
  let open C in
  let open Let_syntax in
  let%bind ret_type, args =
    Constant_typers.infer_constant
      ~infer:infer_expression
      ~check:check_expression
      const
      args
  in
  let%bind loc = loc () in
  return
    ( ret_type
    , E.(
        let%bind args = args
        and type_ = decode ret_type in
        return
        @@ O.make_e
             ~loc
             (E_constant { cons_name = const; arguments = args })
             { type_ with location = loc }) )


and check_pattern ~mut (pat : I.type_expression option I.Pattern.t) (type_ : Type.t)
    : (O.type_expression O.Pattern.t E.t, _, _) C.With_frag.t
  =
  let open C.With_frag in
  let open Let_syntax in
  let module P = O.Pattern in
  let check = check_pattern ~mut in
  let infer = infer_pattern ~mut in
  let const content =
    let%bind loc = loc () in
    return
      E.(
        let%bind content = content in
        return @@ (Location.wrap ~loc content : O.type_expression O.Pattern.t))
  in
  let err = pattern_do_not_conform_type pat type_ in
  let fail () = raise err in
  set_loc pat.location
  @@
  match pat.wrap_content, type_.content with
  | P_unit, T_construct { constructor = Literal_types.Unit; _ } ->
    const E.(return P.P_unit)
  | P_unit, _ -> fail ()
  | P_var binder, _ ->
    let%bind () =
      match Binder.get_ascr binder with
      | Some ascr ->
        let%bind ascr = lift @@ evaluate_type_with_lexists ascr in
        unify ascr type_
      | None -> return ()
    in
    let%bind () =
      extend [ Binder.get_var binder, (if mut then Mutable else Immutable), type_ ]
    in
    const
      E.(
        let%bind type_ = decode type_ in
        return @@ P.P_var (Binder.set_ascr binder type_))
  | ( P_list (Cons (hd_pat, tl_pat))
    , T_construct { constructor = Literal_types.List; parameters = [ elt_type ]; _ } ) ->
    let%bind hd_pat = check hd_pat elt_type in
    let%bind tl_pat = check tl_pat type_ in
    const
      E.(
        let%bind hd_pat = hd_pat
        and tl_pat = tl_pat in
        return @@ P.P_list (Cons (hd_pat, tl_pat)))
  | ( P_list (List list_pat)
    , T_construct { constructor = Literal_types.List; parameters = [ elt_type ]; _ } ) ->
    let%bind list_pat = list_pat |> List.map ~f:(fun pat -> check pat elt_type) |> all in
    const
      E.(
        let%bind list_pat = all list_pat in
        return @@ P.P_list (List list_pat))
  | P_variant (label, arg_pat), T_sum row ->
    let%bind label_row_elem =
      raise_opt ~error:err @@ Record.LMap.find_opt label row.fields
    in
    let%bind arg_pat = check arg_pat label_row_elem.associated_type in
    const
      E.(
        let%bind arg_pat = arg_pat in
        return @@ P.P_variant (label, arg_pat))
  | P_tuple tuple_pat, T_record row
    when Record.LMap.cardinal row.fields = List.length tuple_pat ->
    let%bind tuple_pat =
      tuple_pat
      |> List.mapi ~f:(fun i pat ->
             let%bind pat_row_elem =
               raise_opt ~error:err
               @@ Record.LMap.find_opt (Label (Int.to_string i)) row.fields
             in
             let%bind pat_type = Context.tapply pat_row_elem.associated_type in
             check pat pat_type)
      |> all
    in
    const
      E.(
        let%bind tuple_pat = all tuple_pat in
        return @@ P.P_tuple tuple_pat)
  | P_record record_pat, T_record row
    when Record.LMap.cardinal row.fields = Record.LMap.cardinal record_pat ->
    let%bind record_pat =
      record_pat
      |> Record.LMap.mapi (fun label pat ->
             let%bind label_row_elem =
               raise_opt ~error:err @@ Record.LMap.find_opt label row.fields
             in
             let%bind pat_type = Context.tapply label_row_elem.associated_type in
             check pat pat_type)
      |> all_lmap
    in
    const
      E.(
        let%bind record_pat = all_lmap record_pat in
        return @@ P.P_record record_pat)
  | _ ->
    let%bind type_', pat = infer pat in
    let%bind () =
      let%bind type_' = Context.tapply type_' in
      let%bind type_ = Context.tapply type_ in
      unify type_' type_
    in
    return pat


and infer_pattern ~mut (pat : I.type_expression option I.Pattern.t)
    : (Type.t * O.type_expression O.Pattern.t E.t, _, _) C.With_frag.t
  =
  let open C.With_frag in
  let open Let_syntax in
  let module P = O.Pattern in
  let check = check_pattern ~mut in
  let infer = infer_pattern ~mut in
  let const content type_ =
    let%bind loc = loc () in
    return
      ( type_
      , E.(
          let%bind content = content in
          return @@ (Location.wrap ~loc content : O.type_expression O.Pattern.t)) )
  in
  set_loc pat.location
  @@
  match pat.wrap_content with
  | P_unit ->
    let%bind t_unit = create_type Type.t_unit in
    const E.(return P.P_unit) t_unit
  | P_var binder ->
    let var = Binder.get_var binder in
    let%bind type_ =
      match Binder.get_ascr binder with
      | Some ascr -> lift @@ evaluate_type_with_lexists ascr
      | None -> exists Type
    in
    let%bind () = extend [ var, (if mut then Mutable else Immutable), type_ ] in
    const
      E.(
        let%bind type_ = decode type_ in
        return @@ P.P_var (Binder.set_ascr binder type_))
      type_
  | P_list (Cons (hd_pat, tl_pat)) ->
    let%bind elt_type, hd_pat = infer hd_pat in
    let%bind t_list =
      let%bind elt_type = Context.tapply elt_type in
      create_type @@ Type.t_list elt_type
    in
    let%bind tl_pat = check tl_pat t_list in
    const
      E.(
        let%bind hd_pat = hd_pat
        and tl_pat = tl_pat in
        return @@ P.P_list (Cons (hd_pat, tl_pat)))
      t_list
  | P_list (List list_pat) ->
    let%bind elt_type = exists Type in
    let%bind list_pat =
      list_pat
      |> List.map ~f:(fun pat ->
             let%bind elt_type = Context.tapply elt_type in
             check pat elt_type)
      |> all
    in
    let%bind t_list = create_type @@ Type.t_list elt_type in
    const
      E.(
        let%bind list_pat = all list_pat in
        return @@ P.P_list (List list_pat))
      t_list
  | P_tuple tuple_pat ->
    let%bind tuple_types, tuple_pat =
      tuple_pat
      |> List.mapi ~f:(fun i pat ->
             let%bind pat_type, pat = infer pat in
             let row_elem =
               { Rows.associated_type = pat_type
               ; decl_pos = i
               ; michelson_annotation = None
               }
             in
             return ((Label.Label (Int.to_string i), row_elem), pat))
      |> all
      >>| List.unzip
    in
    let%bind tuple_type =
      create_type
      @@ Type.t_record
           { fields = Record.of_list tuple_types; layout = Type.default_layout }
    in
    const
      E.(
        let%bind tuple_pat = all tuple_pat in
        return @@ P.P_tuple tuple_pat)
      tuple_type
  | P_variant (constructor, arg_pat) ->
    let%bind tvars, arg_type, sum_type =
      match%bind Context.get_sum constructor with
      | [] -> raise (unbound_constructor constructor)
      | (tvar, tvars, arg_type, sum_type) :: other ->
        let%bind () = lift @@ warn_ambiguous_constructor_pat ~pat ~tvar ~arg_type other in
        return (tvars, arg_type, sum_type)
    in
    let%bind subst =
      tvars
      |> List.map ~f:(fun tvar ->
             let%bind texists = exists Type in
             return (tvar, texists))
      |> all
    in
    let apply_subst type_ =
      List.fold_right subst ~init:type_ ~f:(fun (tvar, texists) type_ ->
          Type.subst type_ ~tvar ~type_:texists)
    in
    let arg_type = apply_subst arg_type in
    let sum_type = apply_subst sum_type in
    let%bind arg_pat = check arg_pat arg_type in
    const
      E.(
        let%bind arg_pat = arg_pat in
        return @@ P.P_variant (constructor, arg_pat))
      sum_type
  | P_record record_pat ->
    let%bind field_types, record_pat =
      Record.LMap.fold
        (fun label expr result ->
          let%bind fields, record = result in
          let%bind expr_type, expr = infer expr in
          let fields = Record.LMap.add label expr_type fields in
          let record = Record.LMap.add label expr record in
          return (fields, record))
        record_pat
        (return (Record.LMap.empty, Record.LMap.empty))
    in
    let _, fields =
      (* No fold_mapi in utils :cry *)
      Record.LMap.fold_map field_types ~init:0 ~f:(fun label associated_type i ->
          let decl_pos =
            let (Label str) = label in
            match Int.of_string str with
            | i -> i
            | exception _ -> i
          in
          i + 1, { Rows.associated_type; michelson_annotation = None; decl_pos })
    in
    let%bind record_type =
      match%bind Context.get_record fields with
      | None ->
        let%bind layout = lexists () in
        create_type @@ Type.t_record { fields; layout }
      | Some (orig_var, row) -> create_type @@ Type.t_record_with_orig_var row ~orig_var
    in
    const
      E.(
        let%bind record_pat = all_lmap record_pat in
        return @@ P.P_record record_pat)
      record_type


and check_cases
    (cases : (I.expression, I.type_expression option) I.Match_expr.match_case list)
    (matchee_type : Type.t)
    (ret_type : Type.t)
    : ((O.type_expression O.Pattern.t * O.expression) list E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let check_case { I.Match_expr.pattern; body } =
    let%bind matchee_type = Context.tapply matchee_type in
    let%bind frag, pattern =
      With_frag.run @@ check_pattern ~mut:false pattern matchee_type
    in
    let%bind body = def_frag frag ~on_exit:Drop ~in_:(check_expression body ret_type) in
    return
      E.(
        let%map pattern = pattern
        and body = body in
        pattern, body)
  in
  cases |> List.map ~f:check_case |> all >>| E.all


and def_frag
    : type a.
      C.With_frag.fragment
      -> on_exit:a C.exit
      -> in_:(a, typer_error, Main_warnings.all) C.t
      -> (a, typer_error, Main_warnings.all) C.t
  =
 fun frag ~on_exit ~in_ ->
  let open C in
  def
    (List.map frag ~f:(fun (var, mut_flag, type_) -> var, mut_flag, type_))
    ~on_exit
    ~in_


and compile_match (matchee : O.expression E.t) cases matchee_type
    : (O.expression_content E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let%bind loc = loc () in
  let%bind syntax = Options.syntax () in
  return
    E.(
      let%bind matchee = matchee in
      let%bind cases = cases in
      let%bind matchee_type = decode matchee_type in
      let eqs = List.map cases ~f:(fun (pat, _body) -> pat, matchee_type) in
      let%bind () = check_anomalies ~loc ~syntax eqs matchee_type in
      return
      @@
      let cases =
        List.map cases ~f:(fun (pattern, body) -> O.Match_expr.{ pattern; body })
      in
      O.E_matching { matchee; cases })


and infer_module_expr (mod_expr : I.module_expr)
    : (Signature.t * O.module_expr E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let module M = Module_expr in
  let const content sig_ =
    let%bind loc = loc () in
    return
    @@ ( sig_
       , E.(
           let%bind content = content in
           return (Location.wrap ~loc content : O.module_expr)) )
  in
  set_loc mod_expr.location
  @@
  match mod_expr.wrap_content with
  | M_struct decls ->
    let%bind sig_, decls = infer_module decls in
    const
      E.(
        let%bind decls = decls in
        return @@ M.M_struct decls)
      sig_
  | M_module_path path ->
    (* Check we can access every element in [path] *)
    let%bind sig_ =
      Context.get_signature_exn path ~error:(unbound_module (List.Ne.to_list path))
    in
    const E.(return @@ M.M_module_path path) sig_
  | M_variable mvar ->
    (* Check we can access [mvar] *)
    let%bind sig_ = Context.get_module_exn mvar ~error:(unbound_module_variable mvar) in
    const E.(return @@ M.M_variable mvar) sig_


and infer_declaration (decl : I.declaration)
    : (Signature.item list * O.declaration E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let%bind syntax = Options.syntax () in
  let const content (sig_item : Signature.item list) =
    let%bind loc = loc () in
    return
      ( sig_item
      , E.(
          let%bind content = content in
          return (Location.wrap ~loc content : O.declaration)) )
  in
  set_loc decl.location
  @@
  match decl.wrap_content with
  | D_irrefutable_match { pattern; expr; attr } ->
    let%bind matchee_type, expr = infer_expression expr in
    let attr = infer_value_attr attr in
    let%bind matchee_type = Context.tapply matchee_type in
    let%bind frags, pattern =
      With_frag.run @@ check_pattern ~mut:false pattern matchee_type
    in
    const
      E.(
        let%bind expr = expr
        and pattern = pattern in
        let%bind () = check_let_annomalies ~syntax pattern expr.type_expression in
        return @@ O.D_irrefutable_match { pattern; expr; attr })
      (List.map ~f:(fun (v, _, ty) -> Context.Signature.S_value (v, ty)) frags)
  | D_type { type_binder; type_expr; type_attr = { public; hidden } } ->
    let%bind type_expr = evaluate_type_with_default_layout type_expr in
    let type_expr = { type_expr with orig_var = Some type_binder } in
    const
      E.(
        let%bind type_expr = decode type_expr in
        return @@ O.D_type { type_binder; type_expr; type_attr = { public; hidden } })
      [ S_type (type_binder, type_expr) ]
  | D_value { binder; attr; expr } ->
    let var = Binder.get_var binder in
    let ascr = Binder.get_ascr binder in
    let%bind expr =
      let%map loc = loc () in
      Option.value_map ascr ~default:expr ~f:(fun ascr -> I.e_ascription ~loc expr ascr)
    in
    let%bind expr_type, expr = infer_expression expr in
    let attr = infer_value_attr attr in
    const
      E.(
        let%bind expr_type = decode expr_type
        and expr = expr in
        return @@ O.D_value { binder = Binder.set_ascr binder expr_type; expr; attr })
      [ S_value (var, expr_type) ]
  | D_module { module_binder; module_; module_attr = { public; hidden } } ->
    let%bind sig_, module_ = infer_module_expr module_ in
    const
      E.(
        let%bind module_ = module_ in
        return @@ O.D_module { module_binder; module_; module_attr = { public; hidden } })
      [ S_module (module_binder, sig_) ]
  | D_contract _ ->
    (* TODO: Contracts *)
    assert false


and infer_module (module_ : I.module_) : (Signature.t * O.module_ E.t, _, _) C.t =
  let open C in
  let open Let_syntax in
  match module_ with
  | [] -> return ([], E.return [])
  | decl :: module_ ->
    let%bind sig_, decl = infer_declaration decl in
    let%bind tl_sig_, decls =
      def_sig_item sig_ ~on_exit:Lift_sig ~in_:(infer_module module_)
    in
    return
      ( sig_ @ tl_sig_
      , E.(
          let%bind decl = decl
          and decls = decls in
          return (decl :: decls)) )


(* Turns out we merge multiple programs together before this point, which raises error
   when we try doing Location.cover! This is a code smell from our build system *)
(* let loc_of_program (program : I.program) =
  match program with
  | [] -> assert false
  | decl :: decls ->
    List.fold_right
      decls
      ~f:(fun decl loc -> Location.cover loc decl.location)
      ~init:decl.location *)

let type_program ~raise ~options ?env program =
  let loc = Location.generated in
  C.run_elab
    (let%map.C _, program = infer_module program in
     program)
    ~raise
    ~options
    ~loc
    ?env
    ()


let type_declaration ~raise ~options ?env decl =
  C.run_elab
    (let%map.C _, decl = infer_declaration decl in
     decl)
    ~raise
    ~options
    ~loc:decl.location
    ?env
    ()


let type_expression ~raise ~options ?env ?tv_opt expr =
  C.run_elab
    (match tv_opt with
    | Some type_ ->
      let type_ = C.encode type_ in
      check_expression expr type_
    | None ->
      let%map.C _, expr = infer_expression expr in
      expr)
    ~raise
    ~options
    ~loc:expr.location
    ?env
    ()
