open Core
open Ligo_prim
module Errors = Errors
module Type_var_name_tbl = Type.Type_var_name_tbl
module Signature = Context.Signature
module Attrs = Context.Attrs
module I = Ast_core
module O = Ast_typed
module C = Computation
module E = Elaboration
module Error_recovery = C.Error_recovery
module Ne_list = Simple_utils.Ne_list
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
module Ligo_string = Simple_utils.Ligo_string
module Ligo_map = Simple_utils.Ligo_map
module Fuzzy_bool = Simple_utils.Fuzzy_bool
module Refs_tbl = Context.Refs_tbl
module Persistent_env = Persistent_env
module Cmi = Cmi
module Cmo = Cmo

let untype_expression = Untyper.untype_expression
let untype_type_expression = Untyper.untype_type_expression
let untype_signature = Untyper.untype_signature

let assert_type_expression_eq ~raise (loc : Location.t) (type1, type2) : unit =
  Trace.trace_option ~raise (Errors.assert_equal type1 type2 loc)
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


let check_let_anomalies ~options:_ ~syntax binder type_expr =
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
  let const ?orig_var ?applied_types content =
    let%bind loc = loc () in
    return @@ Type.make_t ~loc ?orig_var ?applied_types content
  in
  let lift type_ =
    let%bind loc = loc () in
    return @@ Type.{ type_ with location = loc }
  in
  let%bind refs_tbl = refs_tbl () in
  match type_.type_content with
  | T_contract_parameter x ->
    let%bind { items = _; sort } =
      Error_recovery.Get.module_of_path
        x
        ~error:(Errors.unbound_module (Nonempty_list.to_list x))
    in
    let%bind parameter, _ =
      Error_recovery.raise_or_use_default_opt
        ~error:Errors.not_a_contract
        ~default:
          (let%bind p = Error_recovery.wildcard_type in
           let%bind s = Error_recovery.wildcard_type in
           return (p, s))
        (Signature.get_contract_sort sort)
    in
    lift parameter
  | T_arrow { type1; type2; param_names } ->
    let%bind type1 = evaluate_type ~default_layout type1 in
    let%bind type2 = evaluate_type ~default_layout type2 in
    const @@ T_arrow { type1; type2; param_names }
  | T_sum row ->
    let%bind row = evaluate_row ~default_layout row in
    const @@ T_sum row
  | T_union union ->
    let module Comp_union = C.Make_all (Union) in
    let%bind union =
      union |> Union.map (evaluate_type ~default_layout) |> Comp_union.all
    in
    const @@ T_union union
  | T_record row ->
    let%bind row = evaluate_row ~default_layout row in
    const @@ T_record row
  | T_variable tvar ->
    (* Get the closest type or type variable with type var [tvar] *)
    (match%bind
       Error_recovery.Get.type_or_type_var tvar ~error:(Errors.unbound_type_variable tvar)
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
        Error_recovery.Get.type_
          type_operator
          ~error:(Errors.unbound_type_variable type_operator)
      | fst_mod :: more_mods ->
        let%bind sig_ =
          Error_recovery.Get.module_of_path
            (fst_mod :: more_mods)
            ~error:(Errors.unbound_module module_path)
        in
        Error_recovery.raise_or_use_default_opt
          (Signature.get_type ~refs_tbl sig_ type_operator)
          ~default:Error_recovery.wildcard_type
          ~error:(Errors.unbound_type_variable type_operator)
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
             | _ ->
               Error_recovery.raise_or_use_default
                 ~default:(return ())
                 ~error:(Errors.ill_formed_type arg))
      |> all_unit
    in
    (* 4. Beta-reduce *)
    let vars, ty_body = Type.destruct_type_abstraction operator in
    let%bind vargs =
      match List.zip_with_remainder vars arguments with
      | vargs, Some _excess ->
        let actual = List.length arguments in
        let expected = List.length vars in
        Error_recovery.raise_or_use_default
          ~default:(return vargs)
          ~error:(Errors.type_app_wrong_arity (Some type_operator) expected actual)
      | vargs, None -> return vargs
    in
    let result =
      List.fold_right vargs ~init:ty_body ~f:(fun (tvar, type_) result ->
          Type.subst result ~tvar ~type_)
    in
    const ~orig_var:(module_path, type_operator) ~applied_types:arguments result.content
  | T_module_accessor { module_path = fst_mod :: more_mods as module_path; element } ->
    let%bind sig_ =
      Error_recovery.Get.module_of_path
        (fst_mod :: more_mods)
        ~error:(Errors.unbound_module module_path)
    in
    Error_recovery.raise_or_use_default_opt
      ~error:(Errors.unbound_type_variable element)
      ~default:Error_recovery.wildcard_type
      (Signature.get_type ~refs_tbl sig_ element)
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
  | T_module_accessor { module_path = []; element } -> assert false


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


  and evaluate_signature_item_attribute (attr : Sig_item_attr.t) =
    let open C in
    let open Let_syntax in
    return
    @@ Attrs.Value.
         { view = attr.view
         ; entry = attr.entry
         ; dyn_entry = attr.dyn_entry
         ; public = true
         ; optional = attr.optional
         ; leading_comments = attr.leading_comments
         }


  and evaluate_signature (sig_ : Ast_core.signature) : (Signature.t * unit, _, _) C.t =
    let open C in
    let open Let_syntax in
    match sig_.items with
    | [] -> return @@ (Signature.{ items = []; sort = Signature.Ss_module }, ())
    | { wrap_content = sig_item; location = loc } :: sig_ ->
      (match sig_item with
      | Ast_core.S_value (v, ty, attr) ->
        let%bind ty = evaluate_type ty in
        let%bind attr = evaluate_signature_item_attribute attr in
        let%bind { items; sort }, () = evaluate_signature { items = sig_ } in
        return
        @@ ( Signature.
               { sort; items = Location.wrap ~loc (S_value (v, ty, attr)) :: items }
           , () )
      | S_type (v, ty, attr) ->
        let%bind ty = evaluate_type ty in
        let%bind { sort; items }, () =
          def_type [ v, ty ] ~on_exit:Drop ~in_:(evaluate_signature { items = sig_ })
        in
        return
        @@ ( Signature.
               { sort
               ; items =
                   Location.wrap
                     ~loc
                     (S_type
                        ( v
                        , ty
                        , { public = true; leading_comments = attr.leading_comments } ))
                   :: items
               }
           , () )
      | S_type_var (v, attr) ->
        let%bind { sort; items }, () =
          def_type_var
            [ v, Type ]
            ~on_exit:Drop
            ~in_:(evaluate_signature { items = sig_ })
        in
        return
        @@ ( Signature.
               { sort
               ; items =
                   Location.wrap
                     ~loc
                     (S_type_var
                        ( v
                        , { Attrs.Type.default with
                            leading_comments = attr.leading_comments
                          } ))
                   :: items
               }
           , () )
      | S_module (v, items) ->
        let%bind signature, () = evaluate_signature items in
        let%bind { sort; items }, () =
          def_module
            [ v, signature ]
            ~on_exit:Drop
            ~in_:(evaluate_signature { items = sig_ })
        in
        return
        @@ ( Signature.
               { sort
               ; items =
                   Location.wrap ~loc (S_module (v, signature, Attrs.Type.default))
                   :: items
               }
           , () )
      | S_module_type (v, items) ->
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
               ; items =
                   Location.wrap
                     ~loc
                     (S_module_type (v, signature, Attrs.Signature.default))
                   :: items
               }
           , () )
      | S_include s_ ->
        let%bind signature = evaluate_signature_expr s_ in
        let%bind { sort; items }, () =
          def_sig_item
            signature.items
            ~on_exit:Drop
            ~in_:(evaluate_signature { items = sig_ })
        in
        return @@ (Signature.{ sort; items = signature.items @ items }, ()))


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
        Error_recovery.Get.module_type_of_path
          module_path
          ~error:(Errors.unbound_module_type module_path)
      in
      return sig_
end

let infer_value_attr : I.ValueAttr.t -> O.ValueAttr.t = fun x -> x

let infer_literal ?(singleton = false) lit : (Type.t * O.expression E.t, _, _) C.t =
  let open C in
  let open Let_syntax in
  let const constr =
    let%bind loc = loc () in
    let%bind type_ = create_type constr in
    return
      ( type_
      , E.(
          let open Let_syntax in
          let%bind type_ = decode type_ in
          return @@ O.make_e ~loc (E_literal lit) type_) )
  in
  if singleton
  then const (Type.t_singleton lit)
  else (
    let typ = Literal_value.typeof lit in
    let constr = Type.t_construct typ [] in
    const constr)


let t_record_with_orig_var (fields : Type.t Record.t) : (Type.t, _, _) C.t =
  let open C in
  let open Let_syntax in
  match%bind Context.get_record fields with
  | None ->
    let%bind layout = lexists (Map.key_set fields) in
    create_type @@ Type.t_record { fields; layout }
  | Some (orig_var, row) ->
    let%bind refs_tbl = refs_tbl () in
    List.iter (Record.labels fields) ~f:(fun label' ->
        Option.iter
          (Ligo_map.find_kv (module Label) row.fields label')
          ~f:(fun (label, _) -> Refs_tbl.add_ref_label refs_tbl ~key:label ~data:label'));
    (match orig_var with
    | None -> create_type @@ Type.t_record row
    | Some orig_var ->
      let%bind path = path () in
      return
        { (Type.t_record row ~loc:(Type_var.get_location orig_var) ()) with
          abbrev = Some { orig_var = path, orig_var; applied_types = [] }
        })


let get_type_of_ctor
    (row : Type__Type_def.row)
    (label : Label.t)
    ?(use_raise_opt : bool = false)
    ~(error : ('err Errors.with_loc, 'err, 'a) C.t)
    ()
    : (Type.t, 'err, 'a) C.t
  =
  let open C in
  let open Let_syntax in
  let raise = function
    | None ->
      let%bind error = error in
      if use_raise_opt
      then raise error
      else
        Error_recovery.raise_or_use_default
          ~default:(map Error_recovery.wildcard_type ~f:(fun t -> label, t))
          ~error
    | Some result -> return result
  in
  let%bind constr, label_row_elem =
    raise @@ Ligo_map.find_kv (module Label) row.fields label
  in
  let%bind refs_tbl = refs_tbl () in
  Refs_tbl.add_ref_label refs_tbl ~key:constr ~data:label;
  return label_row_elem


let check_record const ~expr ~type_ record (row : Type.row) try_check =
  let open C in
  let open Let_syntax in
  let%bind () =
    assert_
      (Set.equal (Map.key_set record) (Map.key_set row.fields))
      ~error:(Errors.record_mismatch expr type_)
  in
  let%bind record =
    record
    |> Map.mapi ~f:(fun ~key:label ~data:expr ->
           (* Invariant: [Map.key_set record = Map.key_set row.fields]  *)
           let%bind type_ =
             get_type_of_ctor
               ~error:
                 (let%map decoded_row = lift_elab (E.decode_row row) in
                  Errors.unbound_label_edge_case `Record label decoded_row)
               row
               label
               ()
           in
           try_check expr type_)
    |> all_lmap
  in
  const
    E.(
      let open Let_syntax in
      let%bind record = all_lmap record in
      return @@ O.E_record record)


module Union_conditional_body_thunk = struct
  type ('expr, 'typ) t =
    { var : Value_var.t
    ; input_type : 'typ
    ; output_type : 'typ
    ; typ : 'typ
    ; thunk : 'expr
    }
  [@@deriving map]
end

let inline_let_switch_condition
    ~(let_binder : I.type_expression option Linear_pattern.t)
    ~rhs
    ~let_result
    ~default
    ~on_inline
  =
  let let_binder_var =
    match let_binder.wrap_content with
    | P_var binder -> Some binder.var
    | _ -> None
  in
  match let_binder_var with
  | None -> default ()
  | Some let_binder_var ->
    (* Magic hack to propagate static information  *)
    if not
         (Value_var.is_generated let_binder_var
         && (Value_var.is_name_prefix let_binder_var "switch_case_condition_"
            || Value_var.is_name_prefix let_binder_var "switch_subject"))
    then default ()
    else (
      let reduced_expr =
        I.Helpers.subst_expr ~var:let_binder_var ~by:rhs ~in_:let_result
      in
      on_inline reduced_expr)


let rec check_expression (expr : I.expression) (type_ : Type.t)
    : (O.expression E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let check expr type_ = check_expression expr type_ in
  let infer expr = try_infer_expression expr in
  let%bind () = hash_context () in
  set_loc expr.location
  @@
  let const content =
    let%bind loc = loc () in
    return
      E.(
        let open Let_syntax in
        let%bind content = content in
        let%bind type_ = decode type_ in
        return @@ O.make_e ~loc content type_)
  in
  let const_error_recovery () =
    let%bind type_ = Error_recovery.wildcard_type in
    const (E.return @@ O.E_error expr)
  in
  let const_with_type content type_ =
    let%bind loc = loc () in
    return
      E.(
        let open Let_syntax in
        let%bind content = content in
        let%bind type_ = decode type_ in
        return @@ O.make_e ~loc content type_)
  in
  match expr.expression_content, type_.content with
  | E_literal lit, T_singleton lit_in_typ ->
    let%bind lit_type, expr = infer_literal ~singleton:true lit in
    let%bind () =
      assert_
        (Type.equal lit_type type_)
        ~error:(Errors.literal_type_mismatch lit_type type_)
    in
    return expr
  | E_literal lit, T_construct { language = _; constructor; parameters } ->
    check_literal const_error_recovery ~type_ lit ~constructor
  | E_constant { cons_name = const; arguments = args }, _ ->
    check_constant const args type_
  | ( E_type_abstraction { type_binder = tvar; result }
    , T_for_all { ty_binder = tvar'; kind; type_ } ) ->
    let type_ = Type.subst_var type_ ~tvar:tvar' ~tvar':tvar in
    let%bind result =
      def_type_var [ tvar, kind ] ~on_exit:Drop ~in_:(check result type_)
    in
    let%bind type_ = create_type @@ Type.t_for_all { ty_binder = tvar; kind; type_ } in
    const_with_type
      E.(
        let open Let_syntax in
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
        let open Let_syntax in
        let%bind result = result in
        return @@ O.E_type_abstraction { type_binder = tvar; result })
  | E_lambda lambda, T_arrow { type1 = arg_type; type2 = ret_type; param_names = _ } ->
    let%bind lambda = check_lambda lambda arg_type ret_type in
    const
      E.(
        let open Let_syntax in
        let%bind lambda = lambda in
        return @@ O.E_lambda lambda)
  | E_record record, T_record row ->
    check_record const ~expr ~type_ record row check_expression
  | E_tuple tuple, T_record row ->
    (* TODO: split T_record to T_tuple *)
    let record = Record.record_of_proper_tuple tuple in
    check_record const ~expr ~type_ record row check_expression
  | E_array (entry :: entries), T_record row ->
    let record = Record.record_of_proper_tuple (entry :: entries) in
    check_record const ~expr ~type_ record row
    @@ fun entry type_ ->
    (match entry with
    | Array_repr.Expr_entry entry -> check_expression entry type_
    | Rest_entry entry ->
      Error_recovery.raise_or_use_default
        ~error:(Errors.unsupported_rest_property entry)
        ~default:(const_error_recovery ()))
  | E_array [], T_construct { language = _; constructor = Unit; parameters = [] } ->
    const @@ E.(return @@ O.E_literal Literal_unit)
  | ( (E_array entries | E_array_as_list entries)
    , T_construct { language = _; constructor = List; parameters = [ _elt_type ] } ) ->
    check_array_as_list ~type_ entries
  | E_update { struct_; path; update }, T_record row ->
    let%bind struct_ = check struct_ type_ in
    let%bind field_row_elem =
      get_type_of_ctor ~error:(return (Errors.bad_record_access type_ path)) row path ()
    in
    let%bind update = check update field_row_elem in
    const
      E.(
        let open Let_syntax in
        let%bind struct_ = struct_
        and update = update in
        return @@ O.E_update { struct_; path; update })
  | E_constructor { constructor; element }, T_sum row ->
    let%bind constr_row_elem =
      get_type_of_ctor
        ~error:(return (Errors.bad_constructor constructor type_))
        row
        constructor
        ()
    in
    let%bind element = check element constr_row_elem in
    const
      E.(
        let open Let_syntax in
        let%bind element = element in
        return @@ O.E_constructor { constructor; element })
  | E_matching ({ matchee; cases } as match_), _ ->
    let handle_as_normal_match () =
      let%bind matchee_type, matchee_typed = infer matchee in
      let%bind matchee_type = Context.tapply matchee_type in
      let%bind cases = check_cases cases matchee_type type_ in
      let%bind match_expr = compile_match matchee_typed cases matchee_type in
      const match_expr
    in
    (match
       Ast_core.Helpers.Conditional_with_condition_accessor_equals_literal.of_expr expr
     with
    | None -> handle_as_normal_match ()
    | Some conditional ->
      let%bind matchee_typed = check matchee (Type.t_bool ~loc:Location.generated ()) in
      let condition = conditional.condition in
      let%bind struct_type, struct_typed = infer condition.struct_ in
      (match
         ( Type.get_t_union struct_type
         , I.get_e_variable (I.Helpers.remove_ascriptions condition.struct_) )
       with
      | None, _ | _, None -> handle_as_normal_match ()
      | Some union, Some struct_var ->
        let fresh_union_var () =
          Value_var.fresh_like struct_var ~generated:true ~loc:Location.generated
        in
        let%bind partitioned_injections =
          partition_union_for_conditional ~condition ~union
        in
        let partitioned_summands =
          Fuzzy_bool.Indexed.map
            partitioned_injections
            ~f:(List.map ~f:Union.Injection.source)
        in
        if not (List.is_empty partitioned_summands.maybe)
        then handle_as_normal_match ()
        else (
          let then_subunion, else_subunion =
            let Fuzzy_bool.Indexed.{ yes; no; _ } = partitioned_summands in
            (yes, no) |> Tuple2.map ~f:Union.make
          in
          let then_subunion_as_type, else_subunion_as_type =
            Tuple2.map (then_subunion, else_subunion) ~f:(fun union ->
                Type.t_union union ~loc:Location.generated ())
          in
          let then_subunion_is_empty, else_subunion_is_empty =
            Tuple2.map (then_subunion, else_subunion) ~f:(fun subunion ->
                List.is_empty (Union.summands subunion))
          in
          let%bind then_body_thunk =
            check_union_conditional_branch_and_make_body_thunk
              ~name:"then_body_thunk"
              ~struct_var
              ~subunion:then_subunion_as_type
              ~subunion_is_empty:then_subunion_is_empty
              ~body:conditional.then_body
              ~expected_body_type:type_
              ~fresh_union_var
          in
          let%bind else_body_thunk =
            check_union_conditional_branch_and_make_body_thunk
              ~name:"else_body_thunk"
              ~struct_var
              ~subunion:else_subunion_as_type
              ~subunion_is_empty:else_subunion_is_empty
              ~body:conditional.else_body
              ~expected_body_type:type_
              ~fresh_union_var
          in
          return
            E.(
              let open Let_syntax in
              let module Elab_union_conditional_body_thunk =
                E.Make_all2 (Union_conditional_body_thunk)
              in
              let%bind type_ = decode type_ in
              let%bind struct_typed = struct_typed in
              let%bind then_body_thunk =
                then_body_thunk
                |> Union_conditional_body_thunk.map Fn.id decode
                |> Elab_union_conditional_body_thunk.all
              in
              let%bind else_body_thunk =
                else_body_thunk
                |> Union_conditional_body_thunk.map Fn.id decode
                |> Elab_union_conditional_body_thunk.all
              in
              let loc = expr.location in
              let%bind after_expansion_body =
                let%bind content =
                  compile_union_conditional_after_expansion_body
                    ~struct_typed
                    ~then_body_thunk
                    ~else_body_thunk
                    ~fresh_union_var
                    ~partitioned_injections
                    ~then_subunion
                    ~else_subunion
                    ~then_subunion_as_type
                    ~else_subunion_as_type
                    ~result_type:type_
                in
                return @@ O.make_e content type_ ~loc
              in
              let%bind after_expansion =
                compile_union_conditional_after_expansion
                  ~then_body_thunk
                  ~else_body_thunk
                  ~after_expansion_body
              in
              let%bind before_expansion =
                let%bind content =
                  compile_union_conditional_before_expansion
                    ~matchee_typed
                    ~conditional_as_untyped_match_on_bool:match_
                    ~then_body_thunk
                    ~else_body_thunk
                    ~struct_var
                in
                return @@ O.make_e content type_ ~loc
              in
              return
              @@ O.e_union_use
                   (Union.Use.make ~before_expansion ~after_expansion)
                   type_
                   ~loc))))
  | E_let_in { let_binder; rhs; let_result; attributes }, _ ->
    let handle_as_normal_let_in () =
      let%bind rhs_type, rhs = infer rhs in
      let%bind frag, let_binder =
        With_frag.run @@ check_pattern ~mut:false let_binder rhs_type
      in
      let%bind let_result = def_frag frag ~on_exit:Drop ~in_:(check let_result type_) in
      const
        E.(
          let open Let_syntax in
          let%bind let_result = let_result
          and let_binder = let_binder
          and rhs = rhs in
          return @@ O.E_let_in { let_binder; rhs; let_result; attributes })
    in
    inline_let_switch_condition
      ~let_binder
      ~rhs
      ~let_result
      ~default:handle_as_normal_let_in
      ~on_inline:(fun reduced_expr -> check reduced_expr type_)
  | E_let_mut_in { let_binder; rhs; let_result; attributes }, _ ->
    let handle_as_normal_let_mut_in () =
      let%bind rhs_type, rhs = infer rhs in
      let%bind frag, let_binder =
        With_frag.run @@ check_pattern ~mut:true let_binder rhs_type
      in
      let%bind let_result = def_frag frag ~on_exit:Drop ~in_:(check let_result type_) in
      const
        E.(
          let open Let_syntax in
          let%bind let_result = let_result
          and let_binder = let_binder
          and rhs = rhs in
          return @@ O.E_let_mut_in { let_binder; rhs; let_result; attributes })
    in
    inline_let_switch_condition
      ~let_binder
      ~rhs
      ~let_result
      ~default:handle_as_normal_let_mut_in
      ~on_inline:(fun reduced_expr -> check reduced_expr type_)
  | E_mod_in { module_binder; rhs; let_result }, _ ->
    let%bind rhs_sig, rhs = infer_module_expr rhs in
    let%bind let_result =
      def_module [ module_binder, rhs_sig ] ~on_exit:Drop ~in_:(check let_result type_)
    in
    const
      E.(
        let open Let_syntax in
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
        let open Let_syntax in
        let%bind expr = expr in
        f expr)


and check_array_as_list ~type_ entries =
  let open C in
  let open Let_syntax in
  let list_concat ~loc ~left ~right =
    (* List.fold_right (fun x -> x.0 :: x.1) left right *)
    let open I in
    let p = Value_var.fresh ~loc () in
    let result =
      let l = e_record_accessor ~loc (e_variable ~loc p) (Label.of_int 0) in
      let r = e_record_accessor ~loc (e_variable ~loc p) (Label.of_int 1) in
      e_constant ~loc C_CONS [ l; r ]
    in
    let f = e_lambda ~loc (Ligo_prim.Param.make p None) None result in
    e_constant ~loc C_LIST_FOLD_RIGHT [ f; left; right ]
  in
  let%bind loc = loc () in
  let list =
    let open I in
    let empty = e_constant ~loc C_LIST_EMPTY [] in
    List.fold_right entries ~init:empty ~f:(fun entry right ->
        match entry with
        | Array_repr.Expr_entry left -> e_constant ~loc C_CONS [ left; right ]
        | Array_repr.Rest_entry left -> list_concat ~loc ~left ~right)
  in
  check_expression list type_


and check_literal const_error_recovery ~type_ lit ~constructor =
  let open C in
  let open Let_syntax in
  let check expr = check_expression expr type_ in
  let%bind loc = loc () in
  let const_raise_opt ~error value default =
    match value with
    | Some value -> default value
    | None ->
      Error_recovery.raise_or_use_default ~error ~default:(const_error_recovery ())
  in
  match lit, constructor with
  | Literal_string lit, Key_hash ->
    let s = Ligo_string.extract lit in
    const_raise_opt
      ~error:(Errors.bad_key_hash s)
      (Tezos_crypto.Signature.Public_key_hash.of_b58check_opt s)
    @@ fun _ -> check @@ I.(e_key_hash ~loc s)
  | Literal_string lit, Signature ->
    let s = Ligo_string.extract lit in
    const_raise_opt
      ~error:(Errors.bad_signature s)
      (Tezos_crypto.Signature.of_b58check_opt s)
    @@ fun _ -> check @@ I.(e_signature ~loc s)
  | Literal_string lit, Key ->
    let s = Ligo_string.extract lit in
    const_raise_opt
      ~error:(Errors.bad_key s)
      (Tezos_crypto.Signature.Public_key.of_b58check_opt s)
    @@ fun _ -> check @@ I.(e_key ~loc s)
  | Literal_string lit, Timestamp ->
    let open Tezos_base.TzPervasives.Time.Protocol in
    let str = Ligo_string.extract lit in
    const_raise_opt ~error:(Errors.bad_timestamp str) (of_notation str)
    @@ fun time ->
    let lit = Z.of_int64 @@ to_seconds time in
    check @@ I.(e_timestamp ~loc lit)
  | Literal_int lit, Timestamp -> check @@ I.(e_timestamp ~loc lit)
  | Literal_string lit, Chain_id ->
    let lit = Ligo_string.extract lit in
    check @@ I.(e_chain_id ~loc lit)
  | Literal_string lit, Address ->
    let lit = Ligo_string.extract lit in
    (* TODO: bad address? *)
    check @@ I.(e_address ~loc lit)
  | Literal_bytes lit, Bls12_381_g1 -> check @@ I.(e_bls12_381_g1 ~loc lit)
  | Literal_bytes lit, Bls12_381_g2 -> check @@ I.(e_bls12_381_g2 ~loc lit)
  | Literal_bytes lit, Bls12_381_fr -> check @@ I.(e_bls12_381_fr ~loc lit)
  | Literal_bytes lit, Chest -> check @@ I.(e_chest ~loc lit)
  | Literal_bytes lit, Chest_key -> check @@ I.(e_chest_key ~loc lit)
  | Literal_string lit, Bytes ->
    let str_to_byte_opt x =
      try Some (Hex.to_bytes (`Hex x)) with
      | _ -> None
    in
    let str = Ligo_string.extract lit in
    const_raise_opt ~error:Errors.bad_conversion_bytes (str_to_byte_opt str)
    @@ fun b -> check @@ I.(e_bytes ~loc b)
  | Literal_int lit, Nat -> check @@ I.(e_nat ~loc lit)
  | Literal_int lit, Tez ->
    let lit = Z.mul (Z.of_int 1_000_000) lit in
    check @@ I.(e_mutez ~loc lit)
  | _ ->
    let open C in
    let open Let_syntax in
    let%bind lit_type, expr = infer_literal lit in
    let%bind () =
      assert_
        (Type.equal lit_type type_)
        ~error:(Errors.literal_type_mismatch lit_type type_)
    in
    return expr


and partition_union_for_conditional
    ~(condition :
       (I.expression, I.type_expression) I.Helpers.Condition_accessor_equals_literal.t)
    ~(union : Type.t Union.t)
    : ( Type.t Union.Injection.t list Fuzzy_bool.Indexed.t, Errors.typer_error
    , Main_warnings.all ) C.t
  =
  let open C in
  let open Let_syntax in
  union
  |> Union.Injection.injections_of_union
  |> List.map ~f:(fun injection ->
         let summand = Union.Injection.source injection in
         match summand.content with
         | T_record row ->
           let%bind field_type =
             get_type_of_ctor
               ~error:(return (Errors.bad_record_access summand condition.field))
               row
               condition.field
               ()
           in
           let condition_value : Fuzzy_bool.t =
             match Type.get_t_singleton field_type with
             | None -> Maybe
             | Some lit -> if Literal_value.equal lit condition.lit then Yes else No
           in
           return (condition_value, injection)
         | _ -> raise (Errors.expected_record summand))
  |> all
  >>| Fuzzy_bool.Indexed.of_alist_multi


and check_union_conditional_branch_and_make_body_thunk
    ~name
    ~struct_var
    ~body
    ~subunion
    ~subunion_is_empty
    ~expected_body_type
    ~fresh_union_var
  =
  let open C in
  let open Let_syntax in
  let subunion_var = fresh_union_var () in
  let body = I.Helpers.subst_var ~old_var:struct_var ~new_var:subunion_var body in
  let%bind body_typed =
    if subunion_is_empty
       && (not (Type.is_t_unit expected_body_type))
       && I.equal_expression_content
            body.expression_content
            (I.E_literal Literal_value.Literal_unit)
    then
      (* Handle the potentially superfluous default case added by the reduce_switch nanopass *)
      return
        E.(
          let open Let_syntax in
          let%bind expected_body_type = decode expected_body_type in
          return
          @@ Ast_typed.e_failwith_str
               ~loc:body.location
               "This failwith is a placeholder for a useless body, and should have been \
                erased before Michelson code was generated."
               expected_body_type)
    else
      def
        [ subunion_var, Immutable, subunion, Context.Attr.default ]
        ~on_exit:Drop
        ~in_:(check_expression body expected_body_type)
  in
  let body_thunk_type =
    Type.t_arrow subunion expected_body_type ~loc:Location.generated ()
  in
  let body_thunk_typed =
    E.(
      let open Let_syntax in
      let%bind subunion = decode subunion in
      let%bind body_type = decode expected_body_type in
      let%bind body_typed = body_typed in
      let lam =
        let binder = Param.make subunion_var subunion in
        Lambda.{ binder; output_type = body_type; result = body_typed }
      in
      let%bind body_thunk_type = decode body_thunk_type in
      return @@ O.e_lambda lam body_thunk_type ~loc:Location.generated)
  in
  let var = Value_var.fresh ~name ~loc:Location.generated () in
  return
    Union_conditional_body_thunk.
      { var
      ; input_type = subunion
      ; output_type = expected_body_type
      ; typ = body_thunk_type
      ; thunk = body_thunk_typed
      }


and infer_expression (expr : I.expression)
    : (Type.t * O.expression E.t, Errors.typer_error, _) C.t
  =
  let open C in
  let open Let_syntax in
  let check expr type_ = check_expression expr type_ in
  let infer expr = try_infer_expression expr in
  let%bind () = hash_context () in
  set_loc expr.location
  @@
  let lift expr type_ =
    let%map loc = loc () in
    E.(
      let open Let_syntax in
      let%map expr = expr
      and type_ = decode type_ in
      O.make_e ~loc expr.O.expression_content type_)
  in
  let const content type_ =
    let%bind loc = loc () in
    return
      ( type_
      , E.(
          let open Let_syntax in
          let%bind content = content in
          let%bind type_ = decode type_ in
          return @@ O.make_e ~loc content type_) )
  in
  let%bind syntax = Options.syntax () in
  let const_error_recovery ?type_ errors =
    let%bind () = all_unit @@ List.map ~f:log_error errors in
    let%bind type_ =
      match type_ with
      | None -> Error_recovery.wildcard_type
      | Some type_ -> return type_
    in
    const (E.return @@ O.E_error expr) type_
  in
  let%bind refs_tbl = refs_tbl () in
  match expr.expression_content with
  | E_literal lit -> infer_literal lit
  | E_constant { cons_name = const; arguments = args } ->
    let%bind type_ = exists Type in
    let%bind expr = check_constant const args type_ in
    return (type_, expr)
  | E_variable var ->
    let%bind mut_flag, type_, _ =
      Error_recovery.Get.value var ~error:(function
          | `Not_found -> Errors.unbound_variable var
          | `Mut_var_captured -> Errors.mut_var_captured var)
    in
    const
      E.(
        match mut_flag with
        | Immutable -> return (O.E_variable var)
        | Mutable -> return (O.E_deref var))
      type_
  | E_contract x ->
    let%bind { items = _; sort } =
      Error_recovery.Get.module_of_path
        x
        ~error:(Errors.unbound_module (Nonempty_list.to_list x))
    in
    let%bind parameter, storage =
      Error_recovery.raise_or_use_default_opt
        ~error:Errors.not_a_contract
        ~default:
          (let%bind t1 = Error_recovery.wildcard_type in
           let%bind t2 = Error_recovery.wildcard_type in
           return (t1, t2))
        (Signature.get_contract_sort sort)
    in
    let contract_ty = Type.t_contract_of parameter storage in
    const E.(return (O.E_contract x)) contract_ty
  | E_lambda lambda -> infer_lambda lambda
  | E_application { lamb; args } ->
    let%bind lamb_type, lamb = infer lamb in
    let%bind ret_type, f, args =
      let%bind lamb_type = Context.tapply lamb_type in
      infer_application expr lamb_type args
    in
    const
      E.(
        let open Let_syntax in
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
           let open Let_syntax in
           let%bind expr = expr in
           return (O.E_type_abstraction { type_binder = tvar; result = expr }))
         ret_type
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let handle_as_normal_let_in () =
      let%bind rhs_type, rhs = infer rhs in
      let%bind rhs_type = Context.tapply rhs_type in
      let%bind frags, let_binder =
        With_frag.run @@ check_pattern ~mut:false let_binder rhs_type
      in
      let%bind res_type, let_result =
        def_frag frags ~on_exit:Lift_type ~in_:(infer let_result)
      in
      let attributes = infer_value_attr attributes in
      let%bind options = options () in
      const
        E.(
          let open Let_syntax in
          let%bind rhs_type = decode rhs_type in
          let%bind let_binder = let_binder in
          let%bind rhs = rhs
          and let_result = let_result in
          let%bind () = check_let_anomalies ~options ~syntax let_binder rhs_type in
          return (O.E_let_in { let_binder; rhs; let_result; attributes }))
        res_type
    in
    inline_let_switch_condition
      ~let_binder
      ~rhs
      ~let_result
      ~default:handle_as_normal_let_in
      ~on_inline:(fun reduced_expr -> infer reduced_expr)
  | E_type_in { type_binder = tvar; rhs; let_result } ->
    let%bind rhs = With_default_layout.evaluate_type rhs in
    let%bind res_type, let_result =
      def_type [ tvar, rhs ] ~on_exit:Lift_type ~in_:(infer let_result)
    in
    let%bind let_result = lift let_result res_type in
    return (res_type, let_result)
  | E_raw_code { language = "michelson"; code } ->
    (match I.get_e_ascription code.expression_content with
    | None -> const_error_recovery [ Errors.not_annotated ]
    | Some (code, result_type) ->
      let vals = I.get_e_applications code.expression_content in
      let code, args =
        match vals with
        | [] -> code, []
        | code :: args -> code, args
      in
      let%bind args =
        List.fold_right args ~init:(return []) ~f:(fun expr result ->
            let%bind list = result in
            let%bind expr_type, expr = infer expr in
            let list = (expr_type, expr) :: list in
            return list)
      in
      let%bind _, code = infer code in
      let%bind loc = loc () in
      let%bind result_type = With_default_layout.evaluate_type result_type in
      let rec build_func_type = function
        | [] -> result_type
        | (arg_type, _) :: args -> Type.t_arrow ~loc arg_type (build_func_type args) ()
      in
      let func_type = build_func_type args in
      const
        E.(
          let open Let_syntax in
          let%bind code = code
          and code_type = decode func_type
          and args = all @@ List.map ~f:snd args in
          let code = { code with type_expression = code_type } in
          let code = O.e_a_applications ~loc code args in
          return @@ O.E_raw_code { language = "michelson"; code })
        result_type)
  | E_raw_code { language; code } ->
    (match I.get_e_ascription code.expression_content with
    | None -> const_error_recovery [ Errors.not_annotated ]
    | Some (code, code_type) ->
      let%bind code_type = With_default_layout.evaluate_type code_type in
      let%bind _, code = infer code in
      const
        E.(
          let open Let_syntax in
          let%bind code = code
          and code_type = decode code_type in
          return
          @@ O.E_raw_code { language; code = { code with type_expression = code_type } })
        code_type)
  | E_ascription { anno_expr; type_annotation } ->
    let%bind ascr = With_default_layout.evaluate_type type_annotation in
    let%bind expr = check anno_expr ascr in
    let%bind expr = lift expr ascr in
    return (ascr, expr)
  | E_recursive { fun_name; fun_type; lambda; force_lambdarec } ->
    let%bind fun_type = With_default_layout.evaluate_type fun_type in
    let%bind Arrow.{ type1 = arg_type; type2 = ret_type; param_names = _ } =
      Error_recovery.raise_or_use_default_opt
        ~error:
          (Errors.corner_case "Expected function type annotation for recursive function")
        ~default:
          (let%bind type1 = Error_recovery.wildcard_type in
           let%bind type2 = Error_recovery.wildcard_type in
           return Arrow.{ type1; type2; param_names = [] })
        (Type.get_t_arrow fun_type)
    in
    let%bind lambda =
      def
        [ fun_name, Immutable, fun_type, Context.Attr.default ]
        ~on_exit:Drop
        ~in_:(check_lambda (Lambda.map Fn.id Option.some lambda) arg_type ret_type)
    in
    const
      E.(
        let open Let_syntax in
        let%bind lambda = lambda
        and fun_type = decode fun_type in
        return @@ O.E_recursive { fun_name; fun_type; lambda; force_lambdarec })
      fun_type
  | E_record record -> infer_record const record try_infer_expression
  | E_tuple tuple ->
    infer_record const (Record.record_of_proper_tuple tuple) try_infer_expression
  | E_array entries ->
    let%bind array_as_list = Options.array_as_list () in
    if array_as_list
    then infer_array_as_list entries
    else (
      let infer_array_item entry =
        match entry with
        | Array_repr.Expr_entry entry -> try_infer_expression entry
        | Rest_entry entry ->
          Error_recovery.raise_or_use_default
            ~error:(Errors.unsupported_rest_property entry)
            ~default:(const_error_recovery [])
      in
      match entries with
      | [] -> infer_literal Literal_unit
      | [ hd ] ->
        let%bind hd_type, hd = infer_array_item hd in
        let%bind array_type = create_type @@ Type.t_list hd_type in
        let%bind loc = loc () in
        return
          ( array_type
          , E.(
              let open Let_syntax in
              let%map hd = hd in
              O.e_list ~loc [ hd ] hd.type_expression) )
      | hd :: tl ->
        infer_record const (Record.record_of_proper_tuple (hd :: tl)) infer_array_item)
  | E_array_as_list entries -> infer_array_as_list entries
  | E_accessor { struct_; path = field } ->
    let%bind struct_type, struct_typed = infer struct_ in
    let%bind struct_type = Context.tapply struct_type in
    (match struct_type.content with
    | T_union union ->
      let branches =
        Union.Injection.injections_of_union union
        |> List.map ~f:(fun injection ->
               let var = Value_var.fresh () ~loc:Location.generated in
               let pattern = Union.Match.Pattern.make ~var ~injection in
               let body =
                 let var_as_expr = I.e_variable var ~loc:Location.generated in
                 I.e_accessor
                   { struct_ = var_as_expr; path = field }
                   ~loc:Location.generated
                   ()
               in
               Union.Match.Branch.make ~pattern ~body)
      in
      let%bind match_type, branches = infer_union_match_branches branches in
      let union_accessor =
        compile_union_accessor
          ~struct_typed
          ~field
          ~branches
          ~loc:expr.location
          ~match_type
      in
      const union_accessor match_type
    | _ ->
      let%bind record_type, struct_ = infer struct_ in
      let%bind row =
        let%bind record_type = Context.tapply record_type in
        Error_recovery.raise_or_use_default_opt
          ~error:(Errors.expected_record record_type)
          ~default:Error_recovery.row
          (Type.get_t_record record_type)
      in
      let%bind field_row_elem =
        get_type_of_ctor
          ~error:(return (Errors.bad_record_access record_type field))
          row
          field
          ()
      in
      const
        E.(
          let open Let_syntax in
          let%bind struct_ = struct_ in
          return @@ O.E_accessor { struct_; path = field })
        field_row_elem)
  | E_update { struct_; path; update } ->
    let%bind record_type, struct_ = infer struct_ in
    let%bind row =
      let%bind record_type = Context.tapply record_type in
      Error_recovery.raise_or_use_default_opt
        ~error:(Errors.expected_record record_type)
        ~default:Error_recovery.row
        (Type.get_t_record record_type)
    in
    let%bind field_row_elem =
      get_type_of_ctor
        ~error:(return (Errors.bad_record_access record_type path))
        row
        path
        ()
    in
    let%bind update = check update field_row_elem in
    const
      E.(
        let open Let_syntax in
        let%bind struct_ = struct_
        and update = update in
        return @@ O.E_update { struct_; path; update })
      record_type
  | E_constructor { constructor = Label (label, _) as constructor; element = _ }
    when String.(label = "M_right" || label = "M_left") ->
    let error = Errors.michelson_or_no_annotation constructor in
    Error_recovery.raise_or_use_default ~error ~default:(const_error_recovery [])
  | E_constructor { constructor; element = arg } ->
    let%bind tvars, arg_type, sum_type =
      match%bind Context.get_sum constructor with
      | [] ->
        Error_recovery.raise_or_use_default
          ~error:(Errors.unbound_constructor constructor)
          ~default:
            (let tvars = [] in
             let%bind arg_type = Error_recovery.wildcard_type in
             let%bind sum_type = Error_recovery.wildcard_type in
             return (tvars, arg_type, sum_type))
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
    let sum_type =
      Type.set_applied_types ~applied_types:(List.map subst ~f:snd) sum_type
    in
    let%bind arg = check arg arg_type in
    const
      E.(
        let open Let_syntax in
        let%bind arg = arg in
        return @@ O.E_constructor { constructor; element = arg })
      sum_type
  | E_matching _ ->
    let%bind ret_type = exists Type in
    let%bind match_expr = check expr ret_type in
    let%bind ret_type = Context.tapply ret_type in
    return (ret_type, match_expr)
  | E_mod_in { module_binder = mvar; rhs; let_result } ->
    let%bind sig_, rhs = infer_module_expr rhs in
    let%bind ret_type, let_result =
      def_module [ mvar, sig_ ] ~on_exit:Lift_type ~in_:(infer let_result)
    in
    const
      E.(
        let open Let_syntax in
        let%bind let_result = let_result
        and rhs = rhs in
        return @@ O.E_mod_in { module_binder = mvar; rhs; let_result })
      ret_type
  | E_module_accessor { module_path = fst_mod :: more_mods as module_path; element } ->
    let%bind sig_ =
      Error_recovery.Get.module_of_path
        (fst_mod :: more_mods)
        ~error:(Errors.unbound_module module_path)
    in
    let%bind elt_type, _ =
      Error_recovery.raise_or_use_default_opt
        ~error:(Errors.unbound_variable element)
        ~default:
          (let%bind elt_type = Error_recovery.wildcard_type in
           return (elt_type, Attrs.Value.default))
        (Signature.get_value ~refs_tbl sig_ element)
    in
    const E.(return @@ O.E_module_accessor { module_path; element }) elt_type
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let handle_as_normal_let_mut_in () =
      let%bind rhs_type, rhs = infer rhs in
      let%bind rhs_type = Context.tapply rhs_type in
      let%bind frags, let_binder =
        With_frag.run @@ check_pattern ~mut:true let_binder rhs_type
      in
      let%bind res_type, let_result =
        def_frag frags ~on_exit:Lift_type ~in_:(infer let_result)
      in
      let attributes = infer_value_attr attributes in
      let%bind options = options () in
      const
        E.(
          let open Let_syntax in
          let%bind rhs_type = decode rhs_type in
          let%bind let_binder = let_binder in
          let%bind rhs = rhs
          and let_result = let_result in
          let%bind () = check_let_anomalies ~options ~syntax let_binder rhs_type in
          return (O.E_let_mut_in { let_binder; rhs; let_result; attributes }))
        res_type
    in
    inline_let_switch_condition
      ~let_binder
      ~rhs
      ~let_result
      ~default:handle_as_normal_let_mut_in
      ~on_inline:(fun reduced_expr -> infer reduced_expr)
  | E_assign { binder; expression } ->
    let%bind type_ =
      let var = Binder.get_var binder in
      set_loc (Value_var.get_location var)
      @@ Error_recovery.Get.mut var ~error:(function
             | `Not_found -> Errors.unbound_mut_variable var
             | `Mut_var_captured -> Errors.mut_var_captured var)
    in
    let%bind type_ = Context.tapply type_ in
    let%bind expression = check expression type_ in
    let%bind ret_type = create_type Type.t_unit in
    const
      E.(
        let open Let_syntax in
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
        let open Let_syntax in
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
      Error_recovery.raise_or_use_default_opt
        ~error:(Errors.mismatching_for_each_collection_type collection_type type_)
        ~default:
          (let%bind key_type = Error_recovery.wildcard_type in
           let%bind val_type = Error_recovery.wildcard_type in
           return (key_type, val_type))
        (Type.get_t_map type_)
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
        let open Let_syntax in
        let%bind collection = collection
        and fe_body = fe_body in
        return @@ O.E_for_each { fe_binder; collection; collection_type = Map; fe_body })
      t_unit
  | E_for_each { fe_binder = _, Some _; collection_type = List | Set; _ } ->
    let error = Errors.mismatching_for_each_binder_arity 1 2 in
    Error_recovery.raise_or_use_default ~error ~default:(const_error_recovery [ error ])
  | E_for_each
      { fe_binder = (binder, None) as fe_binder; collection; collection_type; fe_body } ->
    let%bind t_unit = create_type Type.t_unit in
    let%bind type_, collection = infer collection in
    let%bind (binder_type : Type.t) =
      let opt_value default =
        Error_recovery.raise_or_use_default_opt
          ~default
          ~error:(Errors.mismatching_for_each_collection_type collection_type type_)
      in
      (* This is bad -- TODO: get rid of collection type (no-longer required) + use patterns *)
      let get_t_map type_ =
        let%bind type1, type2 =
          opt_value
            (let%bind type1 = Error_recovery.wildcard_type in
             let%bind type2 = Error_recovery.wildcard_type in
             return (type1, type2))
          @@ Type.get_t_map type_
        in
        create_type (Type.t_tuple [ type1; type2 ])
      in
      match (collection_type : For_each_loop.collect_type) with
      | Set -> opt_value Error_recovery.wildcard_type @@ Type.get_t_set type_
      | List -> opt_value Error_recovery.wildcard_type @@ Type.get_t_list type_
      | Map -> get_t_map type_
      | Any ->
        try_
          (opt_value Error_recovery.wildcard_type
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
        let open Let_syntax in
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
        let open Let_syntax in
        let%bind cond = cond
        and body = body in
        return @@ O.E_while { cond; body })
      t_unit
  | E_module_accessor { module_path = []; element } -> assert false


and infer_record : type e. _ -> e Record.t -> (e -> _) -> _ =
 fun const record try_infer ->
  let open C in
  let open Let_syntax in
  let%bind fields, record =
    Map.fold
      ~f:(fun ~key:label ~data:expr result ->
        let%bind fields, record = result in
        let%bind expr_type, expr = try_infer expr in
        let fields = Map.set fields ~key:label ~data:expr_type in
        let record = Map.set record ~key:label ~data:expr in
        return (fields, record))
      record
      ~init:(return Label.Map.(empty, empty))
  in
  let%bind record_type = t_record_with_orig_var fields in
  const
    E.(
      let open Let_syntax in
      let%bind record = all_lmap record in
      return @@ O.E_record record)
    record_type


and infer_union_match_branches
    :  (I.expression, Type.t) Union.Match.Branch.t list
    -> ( Type.t * (O.expression, O.type_expression) Union.Match.Branch.t list E.t
       , Errors.typer_error
       , Main_warnings.all )
       C.t
  =
 fun branches ->
  let open C in
  let open Let_syntax in
  let%bind patterns_and_bodies =
    branches
    |> List.map ~f:(fun branch ->
           let pattern = Union.Match.Branch.pattern branch in
           let%bind body_type, body =
             let var_in_ctx =
               let Binder.{ var; ascr = var_typ } =
                 pattern |> Union.Match.Pattern.to_binder
               in
               var, Param.Immutable, var_typ, Attrs.Value.default
             in
             let body = Union.Match.Branch.body branch in
             def [ var_in_ctx ] ~on_exit:Drop ~in_:(infer_expression body)
           in
           return (pattern, body, body_type))
    |> C.all
  in
  let body_types = List.map ~f:Tuple3.get3 patterns_and_bodies in
  let%bind lub_of_body_types, body_types_to_lub = Computation.lub body_types in
  let branches =
    let branches =
      List.map2
        patterns_and_bodies
        body_types_to_lub
        ~f:(fun (pattern, body, body_type) (body_type_, body_type_to_lub) ->
          assert (Type.equal body_type body_type_);
          E.(
            let open Let_syntax in
            let module Elab_pattern = E.Make_all (Union.Match.Pattern) in
            let%bind pattern =
              pattern |> Union.Match.Pattern.map decode |> Elab_pattern.all
            in
            let%bind body = body in
            let%bind body_in_lub = body_type_to_lub body in
            return (Union.Match.Branch.make ~pattern ~body:body_in_lub)))
    in
    match branches with
    | Unequal_lengths -> assert false
    | Ok branches -> E.all branches
  in
  return (lub_of_body_types, branches)


and infer_array_as_list entries =
  let open C in
  let open Let_syntax in
  let%bind elt_type = exists Type in
  let%bind type_ =
    let%bind loc = loc () in
    return @@ Type.t_construct ~loc List [ elt_type ] ()
  in
  let%bind expr = check_array_as_list ~type_ entries in
  return @@ (type_, expr)


and try_infer_expression (expr : I.expression) : (Type.t * O.expression E.t, _, _) C.t =
  let open C in
  let open Let_syntax in
  if%bind Error_recovery.is_enabled
  then
    (* Most error-recovery cases should be handled by using the functions provided by
       [Error_recovery]. However, in case we still reach [raise.error], we still have this
       catch-all case to allow the recovery to continue. *)
    try_with_diagnostics
      (infer_expression expr)
      ~with_:
        (let%bind loc = loc () in
         (* Use an arbitrary type for the erroneous expression *)
         let%map ret_type =
           match Ast_core.Combinators.get_e_ascription_opt expr with
           | None -> Error_recovery.wildcard_type
           | Some ascr -> With_default_layout.evaluate_type ascr.type_annotation
         in
         ( ret_type
         , E.(
             let open Let_syntax in
             let%bind ret_type = decode ret_type in
             return @@ O.make_e ~loc (E_error expr) ret_type) ))
      ~diagnostics:(fun errors warnings ->
        (* [try_] will swallow errors and warnings in case of success, since we
           extensively use [log_error] for error-recovery. Hence, we still want to log all
           of those that reached here, no matter if we had a success or failure. *)
        let%bind () =
          all_unit
          @@ List.map errors ~f:(fun error ->
                 lift_raise (fun raise -> raise.log_error error))
        in
        let%bind () =
          all_unit
          @@ List.map warnings ~f:(fun warning ->
                 lift_raise (fun raise -> raise.warning warning))
        in
        return ())
  else infer_expression expr


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
       let open Let_syntax in
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
          let open Let_syntax in
          let%bind content = content in
          let%bind type_ = decode type_ in
          return @@ O.make_e ~loc content type_) )
  in
  let is_initial_arg =
    match I.(get_e_lambda result.expression_content) with
    | Some lam ->
      (match Param.get_initial_arg lam.binder with
      | Initial -> true
      | Not_initial -> false)
    | None -> false
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
          let var_name = Param.get_var binder in
          let%bind ret_type, result =
            def
              [ var_name, Param.get_mut_flag binder, arg_type, Context.Attr.default ]
              ~on_exit:Lift_type
              ~in_:(try_infer_expression result)
          in
          let var_name =
            if Value_var.is_generated var_name
            then "_"
            else Value_var.to_name_exn var_name
          in
          let arg_names, ret_names =
            let collected_names = Type.get_param_names ret_type in
            if is_initial_arg
            then [ var_name ], collected_names
            else var_name :: collected_names, []
          in
          let%bind type_ =
            create_type
            @@ Type.t_arrow
                 ~param_names:arg_names
                 arg_type
                 (Type.set_param_names ret_names ret_type)
          in
          const
            E.(
              let open Let_syntax in
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
      let open Let_syntax in
      let%map expr = expr in
      List.fold_right tvars ~init:expr ~f:(fun (tvar, kind) expr ->
          O.e_type_abstraction
            ~loc
            { type_binder = tvar; result = expr }
            (O.t_for_all ~loc tvar kind expr.type_expression)))
  in
  return (type_, expr)


and infer_application (expr : I.expression) (lamb_type : Type.t) (args : I.expression)
    : (Type.t * (O.expression -> O.expression E.t) * O.expression E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let fail () =
    Error_recovery.raise_or_use_default
      ~error:(Errors.should_be_a_function_type lamb_type args)
      ~default:
        (let%bind loc = loc () in
         return
           ( lamb_type
           , E.return
           , E.(
               let open Let_syntax in
               let%bind lamb_type = decode lamb_type in
               return (O.make_e ~loc (O.E_error expr) lamb_type)) ))
  in
  match lamb_type.content with
  | T_for_all { ty_binder = tvar; kind; type_ } ->
    let%bind texists = exists kind in
    let lamb_type = Type.subst type_ ~tvar ~type_:texists in
    let%bind ret_type, f, args = infer_application expr lamb_type args in
    return
      ( ret_type
      , E.(
          fun hole ->
            let open Let_syntax in
            let%bind texists = decode texists in
            let%bind lamb_type = decode lamb_type in
            f
              (O.make_e
                 ~loc:hole.O.location
                 (E_type_inst { forall = hole; type_ = texists })
                 lamb_type))
      , args )
  | T_arrow { type1 = arg_type; type2 = ret_type; param_names } ->
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
    let applied_param_names = List.drop param_names 1 in
    let ret_type =
      if List.is_empty applied_param_names
      then ret_type
      else Type.set_param_names applied_param_names ret_type
    in
    return (ret_type, E.return, args)
  | T_exists tvar ->
    let%bind tvar1 = exists Type in
    let%bind tvar2 = exists Type in
    let%bind arr = create_type @@ Type.t_arrow ~param_names:[] tvar1 tvar2 in
    let%bind () = unify_texists tvar arr in
    let%bind args = check_expression args tvar1 in
    return (tvar2, E.return, args)
  | _ -> fail ()


and check_constant const args type_ : (O.expression E.t, _, _) C.t =
  let open C in
  let open Let_syntax in
  let%bind args, ret_coerce =
    Constant_typers.check_constant
      ~infer:try_infer_expression
      ~check:check_expression
      const
      args
      type_
  in
  let%bind loc = loc () in
  return
    E.(
      let open Let_syntax in
      let%bind args = args
      and type_ = decode type_ in
      ret_coerce
      @@ O.make_e
           ~loc
           (E_constant { cons_name = const; arguments = args })
           { type_ with location = loc })


and check_pattern ~mut (pat : I.type_expression option I.Pattern.t) (type_ : Type.t)
    : (O.type_expression O.Pattern.t E.t, _, _) C.With_frag.t
  =
  let open C.With_frag in
  let open Let_syntax in
  let module P = O.Pattern in
  let unify_with_typer_error type1 type2 =
    unify type1 type2
    |> Computation.With_frag.map_error ~f:(fun err -> (err :> Errors.typer_error))
  in
  let check = check_pattern ~mut in
  let infer = infer_pattern ~mut in
  let const content =
    let%bind loc = loc () in
    return
      E.(
        let open Let_syntax in
        let%bind content = content in
        return @@ (Location.wrap ~loc content : O.type_expression O.Pattern.t))
  in
  let err = Errors.pattern_do_not_conform_type pat type_ in
  (* TODO: add error recovery for patterns *)
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
        unify_with_typer_error ascr type_
      | None -> return ()
    in
    let%bind () =
      extend [ Binder.get_var binder, (if mut then Mutable else Immutable), type_ ]
    in
    const
      E.(
        let open Let_syntax in
        let%bind type_ = decode type_ in
        return @@ P.P_var (Binder.set_ascr binder type_))
  | ( P_list (Cons (hd_pat, tl_pat))
    , T_construct { constructor = Literal_types.List; parameters = [ elt_type ]; _ } ) ->
    let%bind hd_pat = check hd_pat elt_type in
    let%bind tl_pat = check tl_pat type_ in
    const
      E.(
        let open Let_syntax in
        let%bind hd_pat = hd_pat
        and tl_pat = tl_pat in
        return @@ P.P_list (Cons (hd_pat, tl_pat)))
  | ( P_list (List list_pat)
    , T_construct { constructor = Literal_types.List; parameters = [ elt_type ]; _ } ) ->
    let%bind list_pat = list_pat |> List.map ~f:(fun pat -> check pat elt_type) |> all in
    const
      E.(
        let open Let_syntax in
        let%bind list_pat = all list_pat in
        return @@ P.P_list (List list_pat))
  | P_variant (label, arg_pat), T_sum row ->
    let%bind label_row_elem =
      C.With_frag.lift
      @@ get_type_of_ctor row label ~error:(C.return err) ~use_raise_opt:true ()
    in
    let%bind arg_pat = check arg_pat label_row_elem in
    const
      E.(
        let open Let_syntax in
        let%bind arg_pat = arg_pat in
        return @@ P.P_variant (label, arg_pat))
  | P_tuple tuple_pat, T_record row when Map.length row.fields = List.length tuple_pat ->
    let%bind tuple_pat =
      tuple_pat
      |> List.mapi ~f:(fun i pat ->
             let%bind pat_row_elem =
               C.With_frag.lift
               @@ get_type_of_ctor
                    ~error:(C.return err)
                    ~use_raise_opt:true
                    row
                    (Label.create (Int.to_string i))
                    ()
             in
             let%bind pat_type = Context.tapply pat_row_elem in
             check pat pat_type)
      |> all
    in
    const
      E.(
        let open Let_syntax in
        let%bind tuple_pat = all tuple_pat in
        return @@ P.P_tuple tuple_pat)
  | P_record record_pat, T_record row when Map.length row.fields = Map.length record_pat
    ->
    let%bind record_pat =
      record_pat
      |> Map.mapi ~f:(fun ~key:label ~data:pat ->
             let%bind label_row_elem =
               C.With_frag.lift
               @@ get_type_of_ctor ~error:(C.return err) ~use_raise_opt:true row label ()
             in
             let%bind pat_type = Context.tapply label_row_elem in
             check pat pat_type)
      |> all_lmap
    in
    const
      E.(
        let open Let_syntax in
        let%bind record_pat = all_lmap record_pat in
        return @@ P.P_record record_pat)
  | _ ->
    let%bind type_', pat = infer pat in
    let%bind () =
      let%bind type_' = Context.tapply type_' in
      let%bind type_ = Context.tapply type_ in
      unify_with_typer_error type_' type_
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
          let open Let_syntax in
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
        let open Let_syntax in
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
        let open Let_syntax in
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
        let open Let_syntax in
        let%bind list_pat = all list_pat in
        return @@ P.P_list (List list_pat))
      t_list
  | P_tuple tuple_pat ->
    let%bind tuple_types, tuple_pat =
      tuple_pat
      |> List.mapi ~f:(fun i pat ->
             let%bind pat_type, pat = infer pat in
             return ((Label.create (Int.to_string i), pat_type), pat))
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
        let open Let_syntax in
        let%bind tuple_pat = all tuple_pat in
        return @@ P.P_tuple tuple_pat)
      tuple_type
  | P_variant (constructor, arg_pat) ->
    let%bind tvars, arg_type, sum_type =
      match%bind Context.get_sum constructor with
      | [] -> raise (Errors.unbound_constructor constructor)
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
    let sum_type =
      Type.set_applied_types ~applied_types:(List.map subst ~f:snd) sum_type
    in
    let%bind arg_pat = check arg_pat arg_type in
    const
      E.(
        let open Let_syntax in
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
    let%bind record_type = lift @@ t_record_with_orig_var fields in
    const
      E.(
        let open Let_syntax in
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
        let open Let_syntax in
        let%map pattern = pattern
        and body = body in
        pattern, body)
  in
  cases |> List.map ~f:check_case |> all >>| E.all


and def_frag
    : type a.
      C.With_frag.fragment
      -> on_exit:a C.exit
      -> in_:(a, Errors.typer_error, Main_warnings.all) C.t
      -> (a, Errors.typer_error, Main_warnings.all) C.t
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
  let%bind options = options () in
  let%bind loc = loc () in
  let%bind syntax = Options.syntax () in
  let%bind refs_tbl = refs_tbl () in
  return
    E.(
      let open Let_syntax in
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


and compile_union_accessor ~struct_typed ~field ~branches ~loc ~match_type =
  E.(
    let open Let_syntax in
    let%bind struct_ = struct_typed in
    let%bind match_type = decode match_type in
    let before_expansion = O.e_accessor { struct_; path = field } match_type ~loc in
    let%bind branches = branches in
    let after_expansion =
      O.e_union_match (Union.Match.make ~matchee:struct_ ~branches) match_type ~loc
    in
    return @@ O.E_union_use (Union.Use.make ~before_expansion ~after_expansion))


and compile_union_injection ~union ~union_as_type ~source_index_in_union expr =
  let module Elab_union_injection = E.Make_all (Union.Injection) in
  let injection =
    Union.Injection.make ~source_index_in_target:source_index_in_union ~target:union
    |> Option.value_or_thunk ~default:(fun () -> assert false)
  in
  E.(
    let open Let_syntax in
    let%bind union_as_type = decode union_as_type in
    let%bind injection =
      injection |> Union.Injection.map decode |> Elab_union_injection.all
    in
    return
    @@ O.e_union_injected
         (Union.Injected.make ~expr_in_source:expr ~injection)
         union_as_type
         ~loc:Location.generated)


and compile_union_conditional_before_expansion
    ~matchee_typed
    ~(conditional_as_untyped_match_on_bool :
       (I.expression, I.type_expression option) I.Match_expr.t)
    ~then_body_thunk
    ~else_body_thunk
    ~struct_var
  =
  E.(
    let open Let_syntax in
    let%bind matchee_typed = matchee_typed in
    let recover_body_from_body_thunk (body_thunk : O.expression) =
      match body_thunk.expression_content with
      | E_lambda { binder; output_type; result } ->
        O.Helpers.subst_var ~old_var:binder.binder.var ~new_var:struct_var result
      | _ -> assert false
    in
    let cases =
      conditional_as_untyped_match_on_bool.cases
      |> List.map ~f:(fun case ->
             match I.Pattern.decompose_boolean_pattern case.pattern with
             | None -> assert false
             | Some dbp ->
               let pattern = O.Pattern.recompose_boolean_pattern dbp in
               let body =
                 recover_body_from_body_thunk
                   (if dbp.value then then_body_thunk.thunk else else_body_thunk.thunk)
               in
               O.Match_expr.{ pattern; body })
    in
    return @@ O.E_matching O.Match_expr.{ matchee = matchee_typed; cases })


and compile_union_conditional_branch
    ~injections_into_union
    ~subunion
    ~subunion_as_type
    ~body_thunk
    ~fresh_union_var
  =
  let open Union_conditional_body_thunk in
  let module Elab_union_injection = E.Make_all (Union.Injection) in
  E.(
    let open Let_syntax in
    let%bind injections_into_union =
      injections_into_union
      |> List.map ~f:(fun inj ->
             inj |> Union.Injection.map decode |> Elab_union_injection.all)
      |> E.all
    in
    let make_branch index_in_subunion injection_into_union =
      let var = fresh_union_var () in
      let pattern = Union.Match.Pattern.make ~var ~injection:injection_into_union in
      let summand = Union.Injection.source injection_into_union in
      let var_as_expr = O.e_variable var summand ~loc:Location.generated in
      let%bind var_in_subunion =
        compile_union_injection
          ~union:subunion
          ~union_as_type:subunion_as_type
          ~source_index_in_union:index_in_subunion
          var_as_expr
      in
      let body_thunk_var_as_expr =
        O.e_variable body_thunk.var body_thunk.typ ~loc:Location.generated
      in
      let call_body_thunk =
        O.e_application
          Application.{ lamb = body_thunk_var_as_expr; args = var_in_subunion }
          body_thunk.output_type
          ~loc:Location.generated
      in
      return @@ Union.Match.Branch.make ~pattern ~body:call_body_thunk
    in
    injections_into_union |> List.mapi ~f:make_branch |> E.all)


and compile_union_conditional_after_expansion_body
    ~struct_typed
    ~then_body_thunk
    ~else_body_thunk
    ~fresh_union_var
    ~(partitioned_injections : Type.t Union.Injection.t List.t Fuzzy_bool.Indexed.t)
    ~then_subunion
    ~else_subunion
    ~then_subunion_as_type
    ~else_subunion_as_type
    ~result_type
  =
  E.(
    let open Let_syntax in
    let%bind yes_branches =
      compile_union_conditional_branch
        ~injections_into_union:partitioned_injections.yes
        ~subunion:then_subunion
        ~subunion_as_type:then_subunion_as_type
        ~body_thunk:then_body_thunk
        ~fresh_union_var
    in
    let%bind no_branches =
      compile_union_conditional_branch
        ~injections_into_union:partitioned_injections.no
        ~subunion:else_subunion
        ~subunion_as_type:else_subunion_as_type
        ~body_thunk:else_body_thunk
        ~fresh_union_var
    in
    let branches = List.append yes_branches no_branches in
    return @@ O.E_union_match (Union.Match.make ~matchee:struct_typed ~branches))


and compile_union_conditional_after_expansion
    ~then_body_thunk
    ~else_body_thunk
    ~after_expansion_body
  =
  E.(
    let open Let_syntax in
    let add_let_in_above var var_type var_value body =
      O.e_let_in
        O.Let_in.
          { let_binder =
              { wrap_content = O.Pattern.P_var (Binder.make var var_type)
              ; location = Location.generated
              }
          ; rhs = var_value
          ; let_result = body
          ; attributes = { O.ValueAttr.default_attributes with no_mutation = true }
          }
        body.type_expression
        ~loc:Location.generated
    in
    return
      (after_expansion_body
      |> add_let_in_above else_body_thunk.var else_body_thunk.typ else_body_thunk.thunk
      |> add_let_in_above then_body_thunk.var then_body_thunk.typ then_body_thunk.thunk))


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
  let%bind refs_tbl = refs_tbl () in
  match items with
  | [] -> return ([], [])
  | `S_type (v, ty, _) :: sig_ ->
    let%bind ty' =
      Error_recovery.raise_or_use_default_opt
        ~error:(Errors.signature_not_found_type v)
        ~default:Error_recovery.wildcard_type
        (Signature.get_type ~refs_tbl inferred_sig v)
    in
    let%bind sig_, entries = cast_items inferred_sig sig_ in
    let%bind loc = loc () in
    let cast_items' =
      return
        ( Location.wrap ~loc (Signature.S_type (v, ty', Attrs.Module.default)) :: sig_
        , entries )
    in
    if Type.equal ty ty'
    then cast_items'
    else
      Error_recovery.raise_or_use_default
        ~error:(Errors.signature_not_match_type v ty ty')
        ~default:cast_items'
  | `S_value (v, ty, attr) :: sig_ ->
    let%bind v =
      match
        Signature.get_value_with_vvar ~should_add_reference:false ~refs_tbl inferred_sig v
      with
      | Some (v', ty', attr') ->
        let%bind eq = eq ty ty' in
        if Bool.equal attr.entry attr'.entry && Bool.equal attr.view attr'.view && eq
        then return v'
        else
          Error_recovery.raise_or_use_default
            ~error:(Errors.signature_not_match_value v ty ty')
            ~default:(return v')
      | None when attr.optional -> return v
      | None ->
        Error_recovery.raise_or_use_default
          ~error:(Errors.signature_not_found_value v)
          ~default:(return v)
    in
    let%bind sig_, entries = cast_items inferred_sig sig_ in
    let entries = entries @ if attr.entry then [ v ] else [] in
    let%bind loc = loc () in
    return (Location.wrap ~loc (Signature.S_value (v, ty, attr)) :: sig_, entries)


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
  let%bind refs_tbl = refs_tbl () in
  let%bind tvars, items =
    (* TODO: review this *)
    Error_recovery.raise_or_use_default_opt
      ~error:(Errors.corner_case "Signature is not casteable")
      ~default:(return ([], []))
      (Signature.as_casteable annoted_sig)
  in
  let instantiate_tvar tvar r =
    let%bind insts, items = r in
    let%bind tvar, type_ =
      match
        Signature.get_type_with_tvar
          ~should_add_reference:false
          ~refs_tbl
          inferred_sig
          tvar
      with
      | None ->
        let%map type_ =
          Error_recovery.raise_or_use_default_type
            ~error:(Errors.signature_not_found_type tvar)
        in
        tvar, type_
      | Some t -> return t
    in
    let%bind loc = loc () in
    return
      ( Location.wrap ~loc (Signature.S_type (tvar, type_, Attrs.Type.default)) :: insts
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
           let open Let_syntax in
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
        let open Let_syntax in
        let open Let_syntax in
        let%bind decls = decls in
        return @@ M.M_struct decls)
      sig_
  | M_module_path path ->
    (* Check we can access every element in [path] *)
    let%bind sig_ =
      Error_recovery.Get.module_of_path
        path
        ~error:(Errors.unbound_module (Nonempty_list.to_list path))
    in
    const E.(return @@ M.M_module_path path) sig_
  | M_variable mvar ->
    (* Check we can access [mvar] *)
    let%bind sig_ =
      Error_recovery.Get.module_ mvar ~error:(Errors.unbound_module_variable mvar)
    in
    const E.(return @@ M.M_variable mvar) sig_


and infer_declaration (decl : I.declaration)
    : (Signature.item Location.wrap list * O.declaration list E.t, _, _) C.t
  =
  let open C in
  let open Let_syntax in
  let%bind syntax = Options.syntax () in
  let const content (sig_item : Signature.item Location.wrap list) =
    let%bind loc = loc () in
    return
      ( sig_item
      , E.(
          let open Let_syntax in
          let%bind content = content in
          return [ (Location.wrap ~loc content : O.declaration) ]) )
  in
  set_loc decl.location
  @@ set_poly_name_tbl (Type_var_name_tbl.create ())
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
    let%bind loc = loc () in
    let expr =
      Option.value_map ascr ~default:expr ~f:(fun ascr ->
          I.e_ascription ~loc:expr.location expr ascr)
    in
    let%bind expr_type, expr = try_infer_expression expr in
    let%bind lhs_type =
      Error_recovery.raise_or_use_default_result
        (Type.dynamic_entrypoint expr_type)
        ~default:Error_recovery.wildcard_type
        ~error:(fun _loc (`Not_entry_point_form x) ->
          Errors.not_an_entrypoint expr_type (Value_var.get_location var))
        ~ok:return
    in
    let attr = infer_value_attr attr in
    const
      E.(
        let open Let_syntax in
        let%bind lhs_type = decode lhs_type
        and expr = expr in
        return @@ O.D_value { binder = Binder.set_ascr binder lhs_type; expr; attr })
      [ Location.wrap ~loc
        @@ Signature.S_value (var, lhs_type, Context.Attr.of_core_attr attr)
      ]
  | D_irrefutable_match { pattern; expr; attr } ->
    let%bind matchee_type, expr = try_infer_expression expr in
    let attr = infer_value_attr attr in
    let%bind matchee_type = Context.tapply matchee_type in
    let%bind frags, pattern =
      With_frag.run @@ check_pattern ~mut:false pattern matchee_type
    in
    let%bind loc = loc () in
    let%bind options = options () in
    const
      E.(
        let open Let_syntax in
        let%bind expr = expr
        and pattern = pattern in
        let%bind () = check_let_anomalies ~options ~syntax pattern expr.type_expression in
        return @@ O.D_irrefutable_match { pattern; expr; attr })
      (List.map
         ~f:(fun (v, _, ty) ->
           Location.wrap ~loc @@ Signature.S_value (v, ty, Context.Attr.of_core_attr attr))
         frags)
  | D_type { type_binder; type_expr; type_attr } ->
    let%bind type_expr = With_default_layout.evaluate_type type_expr in
    let%bind path = path () in
    let type_expr =
      Type.set_applied_types ~applied_types:[]
      @@ Type.set_orig_var ~orig_var:(path, type_binder) type_expr
    in
    let%bind loc = loc () in
    const
      E.(
        let open Let_syntax in
        let%bind type_expr = decode type_expr in
        return @@ O.D_type { type_binder; type_expr; type_attr })
      [ Location.wrap ~loc
        @@ Signature.S_type (type_binder, type_expr, Attrs.Type.of_core_attr type_attr)
      ]
  | D_value { binder; attr; expr } ->
    let var = Binder.get_var binder in
    let ascr = Binder.get_ascr binder in
    let%bind loc = loc () in
    let expr =
      Option.value_map ascr ~default:expr ~f:(fun ascr ->
          I.e_ascription ~loc:expr.location expr ascr)
    in
    let%bind expr_type, expr = try_infer_expression expr in
    let attr = infer_value_attr attr in
    const
      E.(
        let open Let_syntax in
        let%bind expr_type = decode expr_type
        and expr = expr in
        return @@ O.D_value { binder = Binder.set_ascr binder expr_type; expr; attr })
      [ Location.wrap ~loc
        @@ Signature.S_value (var, expr_type, Context.Attr.of_core_attr attr)
      ]
  | D_module { module_binder; module_; module_attr; annotation } ->
    let%bind path = path () in
    let inner_path = path @ [ module_binder ] in
    let%bind module_, sig_ =
      match annotation with
      | None ->
        (* For non-annoted signatures, we use the one inferred *)
        let%bind inferred_sig, module_ =
          set_path inner_path @@ infer_module_expr module_
        in
        return (module_, remove_non_public inferred_sig)
      | Some { signature; filter } ->
        (* For annoted signtures, we evaluate the signature, cast the inferred signature to it, and check that all entries implemented where declared *)
        let%bind annoted_sig = With_default_layout.evaluate_signature_expr signature in
        let annoted_entries =
          List.filter_map
            ~f:(function
              | { wrap_content = S_value (v, _, attr); location = _ } when attr.entry ->
                Some v
              | _ -> None)
            annoted_sig.items
        in
        let is_annoted_entry =
          if filter
          then List.mem annoted_entries ~equal:Value_var.equal
          else fun _ -> true
        in
        set_path inner_path
        @@ let%bind inferred_sig, module_ = infer_module_expr ~is_annoted_entry module_ in
           let%bind annoted_sig, _ = cast_signature inferred_sig annoted_sig in
           let final_sig = if filter then annoted_sig else inferred_sig in
           return (module_, remove_non_public final_sig)
    in
    let%bind loc = loc () in
    set_path inner_path
    @@ const
         E.(
           let open Let_syntax in
           let%bind module_ = module_ in
           let%bind signature = decode_signature sig_ in
           return
           @@ O.D_module
                { module_binder
                ; module_ = { module_ with signature }
                ; module_attr
                ; annotation = ()
                })
         [ Location.wrap ~loc
           @@ Signature.S_module
                (module_binder, sig_, Attrs.Module.of_core_attr module_attr)
         ]
  | D_signature { signature_binder; signature; signature_attr } ->
    let%bind path = path () in
    let inner_path = path @ [ signature_binder ] in
    let%bind loc = loc () in
    set_path inner_path
    @@ let%bind signature = With_default_layout.evaluate_signature_expr signature in
       const
         E.(
           let open Let_syntax in
           let%bind signature = decode_signature signature in
           return @@ O.D_signature { signature_binder; signature; signature_attr })
         [ Location.wrap ~loc
           @@ Signature.S_module_type
                (signature_binder, signature, Attrs.Signature.of_core_attr signature_attr)
         ]
  | D_module_include module_ ->
    let%bind sig_, module_ = infer_module_expr module_ in
    const
      E.(
        let open Let_syntax in
        let%bind module_ = module_ in
        return @@ O.D_module_include module_)
      sig_.items
  | D_import (Import_rename { alias; imported_module; import_attr } as decl) ->
    let%bind path = path () in
    let inner_name = path @ [ alias ] in
    let%bind loc = loc () in
    (* Lookup signature of [module_path] *)
    let%bind sig_ =
      Error_recovery.Get.module_
        imported_module
        ~error:(Errors.unbound_module_variable imported_module)
    in
    set_path inner_name
    @@ const
         E.(return @@ O.D_import decl)
         [ Location.wrap ~loc
           @@ Signature.S_module
                ( alias
                , sig_
                , (* Note that if [public] is [false] (which it is by default), then
                     the module binding is added to the context but not the signature of the
                     enclosing module. *)
                  Attrs.Module.of_core_attr import_attr )
         ]
  | D_import (Import_all_as { alias; module_str; import_attr } as decl) ->
    let%bind path = path () in
    let inner_name = path @ [ alias ] in
    let%bind loc = loc () in
    let imported_module = Module_var.of_input_var ~loc module_str in
    (* Lookup signature of [module_path] *)
    let%bind sig_ =
      Error_recovery.Get.module_
        imported_module
        ~error:(Errors.unbound_module_variable imported_module)
    in
    set_path inner_name
    @@ const
         E.(return @@ O.D_import decl)
         [ Location.wrap ~loc
           @@ Signature.S_module
                ( alias
                , sig_
                , (* Note that if [public] is [false] (which it is by default), then
                     the module binding is added to the context but not the signature of the
                     enclosing module. *)
                  Attrs.Module.of_core_attr import_attr )
         ]
  | D_import (Import_selected { imported; module_str; import_attr } as decl) ->
    let%bind loc = loc () in
    let imported_module = Module_var.of_input_var ~loc module_str in
    let%bind { items; _ } =
      Error_recovery.Get.module_
        imported_module
        ~error:(Errors.unbound_module_variable imported_module)
    in
    let (h :: tl) = imported in
    let imported = h :: tl in
    let find_var_type items var =
      let item =
        List.find_map items ~f:(fun item ->
            match Location.unwrap item with
            | Signature.S_value (v, type_, _) ->
              if Value_var.equal v var then Some type_ else None
            | _ -> None)
      in
      match item with
      | None -> Computation.raise (Errors.unbound_variable var)
      | Some t -> return t
    in
    let%bind types =
      List.fold_right imported ~init:(return []) ~f:(fun var acc ->
          let%bind type_ = find_var_type items var in
          let%map acc = acc in
          let attr =
            { O.ValueAttr.default_attributes with public = import_attr.public }
          in
          Location.wrap
            ~loc
            (Signature.S_value (var, type_, Context.Attr.of_core_attr attr))
          :: acc)
    in
    const E.(return @@ O.D_import decl) types


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
      let sig_items = filter_non_public_sig_items sig_items in
      return
        ( { sig_' with items = sig_items @ sig_'.items }
        , E.(
            let open Let_syntax in
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
        | { wrap_content = S_value (var, type_, attr); location = _ }
          when attr.entry && is_annoted_entry var -> Some (var, type_)
        | _ -> None)
  in
  (* In case there is no entrypoints, or it has some error, infer the signature sort as
     the old sort. One might be tempted to use [Error_recovery.wildcard_type] for the parameter
     and storage, but it may trigger a [failwith] later on. *)
  let default = return old_sig.sort in
  match Ne_list.of_list_opt entrypoints with
  | None -> default
  | Some entrypoints ->
    (* FIXME: This could be improved by using unification to unify the storage
       types together, permitting more programs to type check. *)
    Error_recovery.raise_or_use_default_result
      (Type.parameter_from_entrypoints entrypoints)
      ~default
      ~ok:(fun (parameter, storage) ->
        return (Signature.Ss_contract { storage; parameter }))
      ~error:
        (fun _loc -> function
          | `Duplicate_entrypoint v ->
            Errors.duplicate_entrypoint v (Value_var.get_location v)
          | `Not_entry_point_form (ep, ep_type) ->
            Errors.not_an_entrypoint ep_type (Value_var.get_location ep)
          | `Storage_does_not_match (ep_1, storage_1, ep_2, storage_2) ->
            (Errors.storage_do_not_match ep_1 storage_1 ep_2 storage_2)
              (Value_var.get_location ep_1)
          | `Wrong_dynamic_storage_definition t ->
            Errors.wrong_dynamic_storage_definition t t.Type.location)


and filter_non_public_sig_items (sig_items : Signature.item Location.wrap list) =
  List.filter sig_items ~f:(fun { wrap_content; location = _ } ->
      match wrap_content with
      | Signature.S_value (_, _, attr) -> attr.public || attr.entry || attr.view
      | Signature.S_type (_, _, attr) -> attr.public
      | Signature.S_type_var (_, attr) -> attr.public
      | Signature.S_module (_, _, attr) -> attr.public
      | Signature.S_module_type (_m, _, attr) -> attr.public)


and remove_non_public (sig_ : Signature.t) =
  { sig_ with items = filter_non_public_sig_items sig_.items }


(* Turns out we merge multiple programs together before this point, which raises error
   when we try doing Location.cover! This is a code smell from our build system
   Note: still trying with cover_until_file_change
   *)
let loc_of_program =
  List.fold ~init:Location.generated ~f:(fun acc el ->
      Location.(cover_until_file_change acc el.location))


let type_program_with_refs ~raise ~options ~refs_tbl ?env program =
  C.run_elab_with_refs
    (let%map.C signature, program = infer_module program in
     E.(
       let open Let_syntax in
       let%bind program = program in
       let signature = remove_non_public signature in
       let%bind signature = decode_signature signature in
       return { Ast_typed.pr_module = program; pr_sig = signature }))
    ~raise
    ~options
    ~loc:(loc_of_program program)
    ~path:[]
    ~refs_tbl
    ?env
    ()


let type_program ~raise ~options ?env program =
  type_program_with_refs ~raise ~options ~refs_tbl:(Refs_tbl.create ()) ?env program


let type_declaration ~raise ~options ~path ?env decl =
  C.run_elab
    (let%map.C _, decl = infer_declaration decl in
     decl)
    ~raise
    ~options
    ~loc:decl.location
    ~path
    ?env
    ()


let type_expression ~raise ~options ~path ?env ?tv_opt expr =
  C.run_elab
    (match tv_opt with
    | Some type_ ->
      let type_ = C.encode ~raise type_ in
      check_expression expr type_
    | None ->
      let%map.C _, expr = try_infer_expression expr in
      expr)
    ~raise
    ~options
    ~loc:expr.location
    ~path
    ?env
    ()


let type_type_expression ~raise ~options ~path ?env (ty : I.type_expression) =
  C.run_elab
    (let%bind.C ty = With_default_layout.evaluate_type ty in
     let ty = E.decode ty in
     C.return ty)
    ~raise
    ~options
    ~loc:ty.location
    ~path
    ?env
    ()


let eval_signature_sort ~raise ~options ~loc ~path ?env old_sig =
  C.run_elab
    (let%map.C sig_sort = infer_signature_sort (C.encode_signature ~raise old_sig) in
     E.(decode_sig_sort sig_sort))
    ~raise
    ~options
    ~loc
    ~path
    ?env
    ()
