module List = Simple_utils.List
open Simple_utils.Trace
module Errors = Errors
open Errors
open Ligo_prim
module Signature = Context.Signature
module Attrs = Context.Attrs
module I = Ast_core
module O = Ast_typed
module C = Computation
module E = Elaboration

let untype_expression = Untyper.untype_expression
let untype_type_expression = Untyper.untype_type_expression
let untype_signature = Untyper.untype_signature

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
    return @@ Type.make_t ~loc content
  in
  let lift type_ =
    let%bind loc = loc () in
    return @@ Type.{ type_ with location = loc }
  in
  match type_.type_content with
  | T_contract_parameter x ->
    let%bind { items = _; sort } =
      Context.get_module_of_path_exn x ~error:(fun loc ->
          unbound_module (List.Ne.to_list x) loc)
    in
    let%bind parameter, _ =
      raise_opt ~error:not_a_contract @@ Signature.get_contract_sort sort
    in
    lift parameter
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
  | T_constant (constructor, arity) ->
    let rec ty_binders (n : int) =
      match n with
      | 0 -> return @@ []
      | n ->
        let%bind ty_binder = fresh_type_var () in
        let%bind rec_ty_binders = ty_binders (n - 1) in
        return @@ (ty_binder :: rec_ty_binders)
    in
    let%bind ty_binders = ty_binders arity in
    let%bind loc = loc () in
    let parameters = List.map ~f:(fun t -> Type.make_t ~loc (T_variable t)) ty_binders in
    let kind = Kind.Type in
    let%bind app =
      const @@ T_construct { language = "Michelson"; constructor; parameters }
    in
    let%bind type_, () =
      def_type_var
        (List.map ~f:(fun ty_binder -> ty_binder, kind) ty_binders)
        ~on_exit:Lift_type
        ~in_:(return @@ (app, ()))
    in
    List.fold_right ty_binders ~init:(return type_) ~f:(fun ty_binder type_ ->
        let%bind type_ = type_ in
        const @@ T_abstraction { ty_binder; kind; type_ })
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
          Context.get_module_of_path_exn
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
      Context.get_module_of_path_exn
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


and evaluate_row ~default_layout ({ fields; layout } : I.row) : (Type.row, 'err, 'wrn) C.t
  =
  let open C in
  let open Let_syntax in
  let%bind layout =
    match layout with
    | Some layout -> return @@ evaluate_layout layout
    | None -> default_layout (Map.key_set fields)
  in
  let%bind fields = fields |> Map.map ~f:(evaluate_type ~default_layout) |> all_lmap in
  return { Type.Row.fields; layout }


and evaluate_layout (layout : Layout.t) : Type.layout = L_concrete layout

module With_default_layout = struct
  let evaluate_type_ = evaluate_type

  let rec evaluate_type type_ =
    let open C in
    evaluate_type_
      ~default_layout:(fun fields -> return @@ Type.default_layout_from_field_set fields)
      type_


  and evaluate_signature_item_attribute (attr : Ast_core.sig_item_attribute) =
    let open C in
    let open Let_syntax in
    return
    @@ Attrs.Value.
         { view = attr.view
         ; entry = attr.entry
         ; dyn_entry = attr.dyn_entry
         ; public = true
         ; optional = attr.optional
         }


  and evaluate_signature (sig_ : Ast_core.signature) : (Signature.t * unit, _, _) C.t =
    let open C in
    let open Let_syntax in
    match sig_.items with
    | [] -> return @@ (Signature.{ items = []; sort = Signature.Ss_module }, ())
    | Ast_core.S_value (v, ty, attr) :: sig_ ->
      let%bind ty = evaluate_type ty in
      let%bind attr = evaluate_signature_item_attribute attr in
      let%bind { items; sort }, () = evaluate_signature { items = sig_ } in
      return @@ (Signature.{ sort; items = S_value (v, ty, attr) :: items }, ())
    | S_type (v, ty) :: sig_ ->
      let%bind ty = evaluate_type ty in
      let%bind { sort; items }, () =
        def_type [ v, ty ] ~on_exit:Drop ~in_:(evaluate_signature { items = sig_ })
      in
      return
      @@ (Signature.{ sort; items = S_type (v, ty, { public = true }) :: items }, ())
    | S_type_var v :: sig_ ->
      let%bind { sort; items }, () =
        def_type_var [ v, Type ] ~on_exit:Drop ~in_:(evaluate_signature { items = sig_ })
      in
      return
      @@ (Signature.{ sort; items = S_type_var (v, Attrs.Type.default) :: items }, ())
    | S_module (v, items) :: sig_ ->
      let%bind signature, () = evaluate_signature items in
      let%bind { sort; items }, () =
        def_module
          [ v, signature ]
          ~on_exit:Drop
          ~in_:(evaluate_signature { items = sig_ })
      in
      return
      @@ ( Signature.{ sort; items = S_module (v, signature, Attrs.Type.default) :: items }
         , () )
    | S_module_type (v, items) :: sig_ ->
      let%bind signature, () = evaluate_signature items in
      let%bind { sort; items }, () =
        def_module_type
          [ v, signature ]
          ~on_exit:Drop
          ~in_:(evaluate_signature { items = sig_ })
      in
      return
      @@ ( Signature.
             { sort
             ; items = S_module_type (v, signature, Attrs.Signature.default) :: items
             }
         , () )
    | S_include s_ :: sig_ ->
      let%bind signature = evaluate_signature_expr s_ in
      let%bind { sort; items }, () =
        def_sig_item
          signature.items
          ~on_exit:Drop
          ~in_:(evaluate_signature { items = sig_ })
      in
      return @@ (Signature.{ sort; items = signature.items @ items }, ())


  and evaluate_signature_expr (sig_expr : Ast_core.signature_expr)
      : (Signature.t, _, _) C.t
    =
    let open C in
    let open Let_syntax in
    match Location.unwrap sig_expr with
    | S_sig sig_ ->
      let%bind sig_, _ = evaluate_signature sig_ in
      return sig_
    | S_path module_path ->
      let%bind sig_ =
        Context.get_module_type_of_path_exn
          module_path
          ~error:(unbound_module_type module_path)
      in
      return sig_
end

let infer_value_attr : I.ValueAttr.t -> O.ValueAttr.t = fun x -> x

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
      assert_
        (Set.equal (Map.key_set record) (Map.key_set row.fields))
        ~error:(record_mismatch expr type_)
    in
    let%bind record =
      record
      |> Map.mapi ~f:(fun ~key:label ~data:expr ->
             (* Invariant: [Map.key_set record = Map.key_set row.fields]  *)
             let type_ = Map.find_exn row.fields label in
             check expr type_)
      |> all_lmap
    in
    const
      E.(
        let%bind record = all_lmap record in
        return @@ O.E_record record)
  | E_update { struct_; path; update }, T_record row ->
    let%bind struct_ = check struct_ type_ in
    let%bind field_row_elem =
      raise_opt ~error:(bad_record_access path) @@ Map.find row.fields path
    in
    let%bind update = check update field_row_elem in
    const
      E.(
        let%bind struct_ = struct_
        and update = update in
        return @@ O.E_update { struct_; path; update })
  | E_constructor { constructor; element }, T_sum row ->
    let%bind constr_row_elem =
      raise_opt ~error:(bad_constructor constructor type_)
      @@ Map.find row.fields constructor
    in
    let%bind element = check element constr_row_elem in
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
  | E_let_in { let_binder; rhs; let_result; attributes }, _ ->
    let%bind rhs_type, rhs = infer rhs in
    let%bind frag, let_binder =
      With_frag.run @@ check_pattern ~mut:false let_binder rhs_type
    in
    let%bind let_result = def_frag frag ~on_exit:Drop ~in_:(check let_result type_) in
    const
      E.(
        let%bind let_result = let_result
        and let_binder = let_binder
        and rhs = rhs in
        return @@ O.E_let_in { let_binder; rhs; let_result; attributes })
  | E_let_mut_in { let_binder; rhs; let_result; attributes }, _ ->
    let%bind rhs_type, rhs = infer rhs in
    let%bind frag, let_binder =
      With_frag.run @@ check_pattern ~mut:true let_binder rhs_type
    in
    let%bind let_result = def_frag frag ~on_exit:Drop ~in_:(check let_result type_) in
    const
      E.(
        let%bind let_result = let_result
        and let_binder = let_binder
        and rhs = rhs in
        return @@ O.E_let_mut_in { let_binder; rhs; let_result; attributes })
  | E_mod_in { module_binder; rhs; let_result }, _ ->
    let%bind rhs_sig, rhs = infer_module_expr rhs in
    let%bind let_result =
      def_module [ module_binder, rhs_sig ] ~on_exit:Drop ~in_:(check let_result type_)
    in
    const
      E.(
        let%bind let_result = let_result
        and rhs = rhs in
        return @@ O.E_mod_in { module_binder; rhs; let_result })
  | E_type_in { type_binder; rhs; let_result }, _ ->
    let%bind rhs = With_default_layout.evaluate_type rhs in
    let%bind let_result =
      def_type [ type_binder, rhs ] ~on_exit:Drop ~in_:(check let_result type_)
    in
    return let_result
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


and infer_expression (expr : I.expression)
    : (Type.t * O.expression E.t, typer_error, _) C.t
  =
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
    let%bind mut_flag, type_, _ =
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
  | E_contract x ->
    let%bind { items = _; sort } =
      Context.get_module_of_path_exn x ~error:(fun loc ->
          unbound_module (List.Ne.to_list x) loc)
    in
    let%bind parameter, storage =
      raise_opt ~error:not_a_contract @@ Signature.get_contract_sort sort
    in
    let contract_ty = Type.t_contract_of parameter storage in
    const E.(return (O.E_contract x)) contract_ty
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
    let%bind rhs = With_default_layout.evaluate_type rhs in
    let%bind res_type, let_result =
      def_type [ tvar, rhs ] ~on_exit:Lift_type ~in_:(infer let_result)
    in
    let%bind let_result = lift let_result res_type in
    return (res_type, let_result)
  | E_raw_code { language = "michelson"; code } ->
    let%bind code, result_type =
      raise_opt ~error:not_annotated @@ I.get_e_ascription code.expression_content
    in
    let vals = I.get_e_applications code.expression_content in
    let vals =
      match vals with
      | [] -> [ code ]
      | vals -> vals
    in
    let code = List.hd_exn vals in
    let args = List.tl_exn vals in
    let%bind args =
      List.fold_right
        ~f:(fun expr result ->
          let%bind list = result in
          let%bind expr_type, expr = infer expr in
          let list = (expr_type, expr) :: list in
          return list)
        ~init:(return [])
        args
    in
    let%bind _, code = infer code in
    let%bind loc = loc () in
    let%bind result_type = With_default_layout.evaluate_type result_type in
    let rec build_func_type args =
      match args with
      | [] -> result_type
      | (arg_type, _) :: args -> Type.t_arrow ~loc arg_type (build_func_type args) ()
    in
    let func_type = build_func_type args in
    const
      E.(
        let%bind code = code
        and code_type = decode func_type
        and args = all @@ List.map ~f:snd args in
        let code = { code with type_expression = code_type } in
        let code = O.e_a_applications ~loc code args in
        return @@ O.E_raw_code { language = "michelson"; code })
      result_type
  | E_raw_code { language; code } ->
    let%bind code, code_type =
      raise_opt ~error:not_annotated @@ I.get_e_ascription code.expression_content
    in
    let%bind code_type = With_default_layout.evaluate_type code_type in
    let%bind _, code = infer code in
    const
      E.(
        let%bind code = code
        and code_type = decode code_type in
        return
        @@ O.E_raw_code { language; code = { code with type_expression = code_type } })
      code_type
  | E_ascription { anno_expr; type_annotation } ->
    let%bind ascr = With_default_layout.evaluate_type type_annotation in
    let%bind expr = check anno_expr ascr in
    let%bind expr = lift expr ascr in
    return (ascr, expr)
  | E_recursive { fun_name; fun_type; lambda; force_lambdarec } ->
    let%bind fun_type = With_default_layout.evaluate_type fun_type in
    let%bind Arrow.{ type1 = arg_type; type2 = ret_type } =
      raise_opt
        ~error:(corner_case "Expected function type annotation for recursive function")
      @@ Type.get_t_arrow fun_type
    in
    let%bind lambda =
      def
        [ fun_name, Immutable, fun_type, Context.Attr.default ]
        ~on_exit:Drop
        ~in_:(check_lambda (Lambda.map Fn.id Option.some lambda) arg_type ret_type)
    in
    const
      E.(
        let%bind lambda = lambda
        and fun_type = decode fun_type in
        return @@ O.E_recursive { fun_name; fun_type; lambda; force_lambdarec })
      fun_type
  | E_record record ->
    let%bind fields, record =
      Label.Map.fold
        ~f:(fun ~key:label ~data:expr result ->
          let%bind fields, record = result in
          let%bind expr_type, expr = infer expr in
          let fields = Map.set fields ~key:label ~data:expr_type in
          let record = Map.set record ~key:label ~data:expr in
          return (fields, record))
        record
        ~init:(return Label.Map.(empty, empty))
    in
    let%bind record_type =
      match%bind Context.get_record fields with
      | None ->
        let%bind layout = lexists (Map.key_set fields) in
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
      raise_opt ~error:(bad_record_access field) @@ Map.find row.fields field
    in
    const
      E.(
        let%bind struct_ = struct_ in
        return @@ O.E_accessor { struct_; path = field })
      field_row_elem
  | E_update { struct_; path; update } ->
    let%bind record_type, struct_ = infer struct_ in
    let%bind row =
      let%bind record_type = Context.tapply record_type in
      raise_opt ~error:(expected_record record_type) @@ Type.get_t_record record_type
    in
    let%bind field_row_elem =
      raise_opt ~error:(bad_record_access path) @@ Map.find row.fields path
    in
    let%bind update = check update field_row_elem in
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
      Context.get_module_of_path_exn module_path' ~error:(unbound_module module_path)
    in
    let%bind elt_type, _ =
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
      def
        [ binder, Immutable, t_int, Context.Attr.default ]
        ~on_exit:Drop
        ~in_:(check f_body t_unit)
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
        [ key_binder, Immutable, key_type, Context.Attr.default
        ; val_binder, Immutable, val_type, Context.Attr.default
        ]
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
      def
        [ binder, Immutable, binder_type, Context.Attr.default ]
        ~on_exit:Drop
        ~in_:(check fe_body t_unit)
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
      let%bind arg_ascr = With_default_layout.evaluate_type arg_ascr in
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
           [ ( Param.get_var binder
             , Param.get_mut_flag binder
             , arg_type
             , Context.Attr.default )
           ]
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
            | Some arg_ascr -> With_default_layout.evaluate_type arg_ascr
            | None -> exists Type
          in
          let%bind ret_type, result =
            def
              [ ( Param.get_var binder
                , Param.get_mut_flag binder
                , arg_type
                , Context.Attr.default )
              ]
              ~on_exit:Lift_type
              ~in_:(infer_expression result)
          in
          let%bind type_ = create_type @@ Type.t_arrow arg_type ret_type in
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
         | T_construct { constructor = External Map_find_opt; parameters; _ } ->
           Constant_typers.External_types.map_find_opt_types parameters
         | T_construct { constructor = External Map_add; parameters; _ } ->
           Constant_typers.External_types.map_add_types parameters
         | T_construct { constructor = External Map_remove; parameters; _ } ->
           Constant_typers.External_types.map_remove_types parameters
         | T_construct { constructor = External Int; parameters; _ } ->
           Constant_typers.External_types.int_types parameters
         | T_construct { constructor = External Bytes; parameters; _ } ->
           Constant_typers.External_types.bytes_types parameters
         | T_construct { constructor = External Ediv; parameters; _ } ->
           Constant_typers.External_types.ediv_types parameters
         | T_construct { constructor = External And; parameters; _ } ->
           Constant_typers.External_types.and_types parameters
         | T_construct { constructor = External Or; parameters; _ } ->
           Constant_typers.External_types.or_types parameters
         | T_construct { constructor = External Xor; parameters; _ } ->
           Constant_typers.External_types.xor_types parameters
         | T_construct { constructor = External Lsl; parameters; _ } ->
           Constant_typers.External_types.lsl_types parameters
         | T_construct { constructor = External Lsr; parameters; _ } ->
           Constant_typers.External_types.lsr_types parameters
         | _ -> return ret_type)
        ~with_:(fun _ -> return ret_type)
    in
    return (ret_type, E.return, args)
  | T_exists tvar ->
    let%bind tvar1 = exists Type in
    let%bind tvar2 = exists Type in
    let%bind arr = create_type @@ Type.t_arrow tvar1 tvar2 in
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
        let%bind ascr = lift @@ With_default_layout.evaluate_type ascr in
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
    let%bind label_row_elem = raise_opt ~error:err @@ Map.find row.fields label in
    let%bind arg_pat = check arg_pat label_row_elem in
    const
      E.(
        let%bind arg_pat = arg_pat in
        return @@ P.P_variant (label, arg_pat))
  | P_tuple tuple_pat, T_record row when Map.length row.fields = List.length tuple_pat ->
    let%bind tuple_pat =
      tuple_pat
      |> List.mapi ~f:(fun i pat ->
             let%bind pat_row_elem =
               raise_opt ~error:err @@ Map.find row.fields (Label (Int.to_string i))
             in
             let%bind pat_type = Context.tapply pat_row_elem in
             check pat pat_type)
      |> all
    in
    const
      E.(
        let%bind tuple_pat = all tuple_pat in
        return @@ P.P_tuple tuple_pat)
  | P_record record_pat, T_record row when Map.length row.fields = Map.length record_pat
    ->
    let%bind record_pat =
      record_pat
      |> Map.mapi ~f:(fun ~key:label ~data:pat ->
             let%bind label_row_elem =
               raise_opt ~error:err @@ Map.find row.fields label
             in
             let%bind pat_type = Context.tapply label_row_elem in
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
      | Some ascr -> lift @@ With_default_layout.evaluate_type ascr
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
             return ((Label.Label (Int.to_string i), pat_type), pat))
      |> all
      >>| List.unzip
    in
    let%bind tuple_type =
      create_type
      @@ Type.t_record
           { fields = Record.of_list tuple_types
           ; layout =
               Type.default_layout
                 (tuple_types
                 |> List.map ~f:(fun (i, _type) -> { Layout.name = i; annot = None }))
           }
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
    let%bind fields, record_pat =
      Map.fold
        ~f:(fun ~key:label ~data:expr result ->
          let%bind fields, record = result in
          let%bind expr_type, expr = infer expr in
          let fields = Map.set fields ~key:label ~data:expr_type in
          let record = Map.set record ~key:label ~data:expr in
          return (fields, record))
        record_pat
        ~init:(return Label.Map.(empty, empty))
    in
    let%bind record_type =
      match%bind Context.get_record fields with
      | None ->
        let%bind layout = lexists (Map.key_set fields) in
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
    (List.map frag ~f:(fun (var, mut_flag, type_) ->
         var, mut_flag, type_, Context.Attr.default))
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


and cast_items
    (inferred_sig : Signature.t)
    (items :
      [> `S_type of Type_var.t * Type.t * Attrs.Type.t
      | `S_value of Value_var.t * Type.t * Attrs.Value.t
      ]
      list)
  =
  let open C in
  let open Let_syntax in
  match items with
  | [] -> return ([], [])
  | `S_type (v, ty, _) :: sig_ ->
    let%bind ty' =
      raise_opt (Signature.get_type inferred_sig v) ~error:(signature_not_found_type v)
    in
    if Type.equal ty ty'
    then (
      let%bind sig_, entries = cast_items inferred_sig sig_ in
      return (Signature.S_type (v, ty', Attrs.Module.default) :: sig_, entries))
    else raise (signature_not_match_type v ty ty')
  | `S_value (v, ty, attr) :: sig_ ->
    let%bind () =
      match Signature.get_value inferred_sig v with
      | Some (ty', attr') ->
        if Bool.equal attr.entry attr'.entry
           && Bool.equal attr.view attr'.view
           && Type.equal ty ty'
        then return ()
        else raise (signature_not_match_value v ty ty')
      | None when attr.optional -> return ()
      | None -> raise (signature_not_found_value v)
    in
    let%bind sig_, entries = cast_items inferred_sig sig_ in
    let entries = entries @ if attr.entry then [ v ] else [] in
    return (Signature.S_value (v, ty, attr) :: sig_, entries)


and instantiate_var mt ~tvar ~type_ =
  let self mt = instantiate_var mt ~tvar ~type_ in
  match mt with
  | [] -> mt
  | `S_value (val_var, val_type, val_attr) :: mt ->
    let val_type = Type.subst val_type ~tvar ~type_ in
    `S_value (val_var, val_type, val_attr) :: self mt
  | `S_type (type_var, type_type, attr) :: mt ->
    let type_type = Type.subst type_type ~tvar ~type_ in
    `S_type (type_var, type_type, attr) :: self mt


and cast_signature (inferred_sig : Signature.t) (annoted_sig : Signature.t)
    : (Signature.t * Value_var.t list, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let%bind tvars, items =
    raise_opt ~error:(corner_case "Signature is not casteable")
    @@ Signature.as_casteable annoted_sig
  in
  let instantiate_tvar tvar r =
    let%bind insts, items = r in
    let%bind type_ =
      raise_opt
        (Signature.get_type inferred_sig tvar)
        ~error:(signature_not_found_type tvar)
    in
    return
      ( Signature.S_type (tvar, type_, Attrs.Type.default) :: insts
      , instantiate_var items ~tvar ~type_ )
  in
  let%bind insts, items =
    List.fold_right tvars ~init:(return ([], items)) ~f:instantiate_tvar
  in
  let%bind items, entries = cast_items inferred_sig items in
  return ({ Signature.items = insts @ items; sort = inferred_sig.sort }, entries)


and infer_module_expr ?is_annoted_entry (mod_expr : I.module_expr)
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
           let%bind module_content = content in
           let%bind signature = decode_signature sig_ in
           return ({ module_content; module_location = loc; signature } : O.module_expr))
       )
  in
  set_loc mod_expr.location
  @@
  match mod_expr.wrap_content with
  | M_struct decls ->
    let%bind sig_, decls = infer_module ?is_annoted_entry decls in
    const
      E.(
        let%bind decls = decls in
        return @@ M.M_struct decls)
      sig_
  | M_module_path path ->
    (* Check we can access every element in [path] *)
    let%bind sig_ =
      Context.get_module_of_path_exn path ~error:(unbound_module (List.Ne.to_list path))
    in
    const E.(return @@ M.M_module_path path) sig_
  | M_variable mvar ->
    (* Check we can access [mvar] *)
    let%bind sig_ = Context.get_module_exn mvar ~error:(unbound_module_variable mvar) in
    const E.(return @@ M.M_variable mvar) sig_


and infer_declaration (decl : I.declaration)
    : (Signature.item list * O.declaration list E.t, _, _) C.t
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
          return [ (Location.wrap ~loc content : O.declaration) ]) )
  in
  set_loc decl.location
  @@
  match decl.wrap_content with
  | D_value { binder; attr; expr }
  | D_irrefutable_match { pattern = { wrap_content = P_var binder; _ }; expr; attr }
    when attr.dyn_entry ->
    (* Dynamic entrypoints:
       A dynamic entrypoint declaration has the following form :
       `[@dyn_entry] let v = e`
       and is type checked as:
       - e : ('param, 'storage) entrypoint
       - x : ('param, 'storage) dyn_entrypoint
         (see stdlib type definitions)
    *)
    let var = Binder.get_var binder in
    let ascr = Binder.get_ascr binder in
    let%bind expr =
      let%map loc = loc () in
      Option.value_map ascr ~default:expr ~f:(fun ascr -> I.e_ascription ~loc expr ascr)
    in
    let%bind expr_type, expr = infer_expression expr in
    let%bind lhs_type =
      match Type.dynamic_entrypoint expr_type with
      | Error (`Not_entry_point_form x) ->
        C.raise_l ~loc:(Value_var.get_location var) (not_an_entrypoint expr_type)
      | Ok t -> return t
    in
    let attr = infer_value_attr attr in
    const
      E.(
        let%bind lhs_type = decode lhs_type
        and expr = expr in
        return @@ O.D_value { binder = Binder.set_ascr binder lhs_type; expr; attr })
      [ S_value (var, lhs_type, Context.Attr.of_core_attr attr) ]
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
      (List.map
         ~f:(fun (v, _, ty) ->
           Context.Signature.S_value (v, ty, Context.Attr.of_core_attr attr))
         frags)
  | D_type { type_binder; type_expr; type_attr = { public; hidden } as attr } ->
    let%bind type_expr = With_default_layout.evaluate_type type_expr in
    let type_expr = { type_expr with orig_var = Some type_binder } in
    const
      E.(
        let%bind type_expr = decode type_expr in
        return @@ O.D_type { type_binder; type_expr; type_attr = { public; hidden } })
      [ S_type (type_binder, type_expr, Attrs.Type.of_core_attr attr) ]
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
      [ S_value (var, expr_type, Context.Attr.of_core_attr attr) ]
  | D_module
      { module_binder; module_; module_attr = { public; hidden } as attr; annotation } ->
    let%bind module_, sig_ =
      match annotation with
      | None ->
        (* For non-annoted signatures, we use the one inferred *)
        let%bind inferred_sig, module_ = infer_module_expr module_ in
        return (module_, remove_non_public inferred_sig)
      | Some { signature; filter } ->
        (* For annoted signtures, we evaluate the signature, cast the inferred signature to it, and check that all entries implemented where declared *)
        let%bind annoted_sig = With_default_layout.evaluate_signature_expr signature in
        let annoted_entries =
          List.filter_map
            ~f:(function
              | S_value (v, _, attr) when attr.entry -> Some v
              | _ -> None)
            annoted_sig.items
        in
        let is_annoted_entry =
          if filter
          then List.mem annoted_entries ~equal:Value_var.equal
          else fun _ -> true
        in
        let%bind inferred_sig, module_ = infer_module_expr ~is_annoted_entry module_ in
        let%bind annoted_sig, _ = cast_signature inferred_sig annoted_sig in
        let final_sig = if filter then annoted_sig else inferred_sig in
        return (module_, remove_non_public final_sig)
    in
    const
      E.(
        let%bind module_ = module_ in
        let%bind signature = decode_signature sig_ in
        return
        @@ O.D_module
             { module_binder
             ; module_ = { module_ with signature }
             ; module_attr = { public; hidden }
             ; annotation = ()
             })
      [ S_module (module_binder, sig_, Attrs.Module.of_core_attr attr) ]
  | D_signature { signature_binder; signature; signature_attr } ->
    let%bind signature = With_default_layout.evaluate_signature_expr signature in
    const
      E.(
        let%bind signature = decode_signature signature in
        return @@ O.D_signature { signature_binder; signature; signature_attr })
      [ S_module_type
          (signature_binder, signature, Attrs.Signature.of_core_attr signature_attr)
      ]
  | D_module_include module_ ->
    let%bind sig_, module_ = infer_module_expr module_ in
    const
      E.(
        let%bind module_ = module_ in
        return @@ O.D_module_include module_)
      sig_.items


and infer_module ?is_annoted_entry (module_ : I.module_)
    : (Signature.t * O.module_ E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let rec loop (module_ : I.module_) : (Signature.t * O.module_ E.t, _, _) C.t =
    match module_ with
    | [] ->
      (* By default, we assume that the module is a module *)
      return ({ Signature.items = []; sort = Ss_module }, E.return [])
    | decl :: module_ ->
      let%bind sig_items, decl = infer_declaration decl in
      let%bind sig_', decls =
        def_sig_item sig_items ~on_exit:Lift_sig ~in_:(loop module_)
      in
      return
        ( { sig_' with items = sig_items @ sig_'.items }
        , E.(
            let%bind decl = decl
            and decls = decls in
            return (decl @ decls)) )
  in
  let%bind inferred_module_sig, module_expr = loop module_ in
  let%bind inferred_sort = infer_signature_sort ?is_annoted_entry inferred_module_sig in
  return ({ inferred_module_sig with sort = inferred_sort }, module_expr)


and infer_signature_sort ?is_annoted_entry (old_sig : Signature.t)
    : (Signature.sort, _, _) C.t
  =
  (* A module is said to have a 'contract'ual signature if:
     - it contains at least 1 entrypoint
     - it contains zero or more views
     - it contains zero or more dynamic entrypoints
  *)
  let open C in
  let open Let_syntax in
  let is_annoted_entry = Option.value ~default:(Fn.const true) is_annoted_entry in
  let entrypoints =
    List.filter_map old_sig.items ~f:(function
        | S_value (var, type_, attr) when attr.entry && is_annoted_entry var ->
          Some (var, type_)
        | _ -> None)
  in
  match List.Ne.of_list_opt entrypoints with
  | None -> return old_sig.sort
  | Some entrypoints ->
    (* FIXME: This could be improved by using unification to unify the storage
       types together, permitting more programs to type check. *)
    let%bind parameter, storage =
      match Type.parameter_from_entrypoints entrypoints with
      | Error (`Duplicate_entrypoint v) ->
        C.raise_l ~loc:(Value_var.get_location v) (duplicate_entrypoint v)
      | Error (`Not_entry_point_form (ep, ep_type)) ->
        C.raise_l ~loc:(Value_var.get_location ep) (not_an_entrypoint ep_type)
      | Error (`Storage_does_not_match (ep_1, storage_1, ep_2, storage_2)) ->
        C.raise_l
          ~loc:(Value_var.get_location ep_1)
          (storage_do_not_match ep_1 storage_1 ep_2 storage_2)
      | Error (`Wrong_dynamic_storage_definition t) ->
        C.raise_l ~loc:t.location (wrong_dynamic_storage_definition t)
      | Ok (p, s) -> return (p, s)
    in
    return (Signature.Ss_contract { storage; parameter })


and remove_non_public (sig_ : Signature.t) =
  let items =
    List.filter sig_.items ~f:(function
        | Signature.S_value (_, _, attr) -> attr.public || attr.entry || attr.view
        | Signature.S_type (_, _, attr) -> attr.public
        | Signature.S_type_var (_, attr) -> attr.public
        | Signature.S_module (_, _, attr) -> attr.public
        | Signature.S_module_type (_m, _, attr) -> attr.public)
  in
  { sig_ with items }


(* Turns out we merge multiple programs together before this point, which raises error
   when we try doing Location.cover! This is a code smell from our build system
   Note: still trying with cover_until_file_change
   *)
let loc_of_program =
  List.fold ~init:Location.generated ~f:(fun acc el ->
      Location.(cover_until_file_change acc el.location))


let type_program ~raise ~options ?env program =
  C.run_elab
    (let%map.C signature, program = infer_module program in
     E.(
       let%bind program = program in
       let signature = remove_non_public signature in
       let%bind signature = decode_signature signature in
       return { Ast_typed.pr_module = program; pr_sig = signature }))
    ~raise
    ~options
    ~loc:(loc_of_program program)
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


let eval_signature_sort ~raise ~options ~loc ?env old_sig =
  C.run_elab
    (let%map.C sig_sort = infer_signature_sort (C.encode_signature old_sig) in
     E.(decode_sig_sort sig_sort))
    ~raise
    ~options
    ~loc
    ?env
    ()
