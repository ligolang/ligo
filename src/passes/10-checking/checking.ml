module Trace = Simple_utils.Trace
open Trace
module Errors = Errors
open Errors
module Signature = Context.Signature
module Elaboration = Context.Elaboration
module I = Ast_core
module O = Ast_typed
open O.Combinators
open Subtyping
module Pair = Simple_utils.Pair
open Ligo_prim

let debug = false
let assertions = false
(* let local_pos : Context.pos list ref = ref [] *)

(* let curr_pos () : Context.pos = List.hd_exn !local_pos *)
let pp_local_context ppf ctx = Context.pp ppf ctx
let untype_expression = Untyper.untype_expression
let untype_program = Untyper.untype_program

let assert_type_expression_eq
    ~raise
    (loc : Location.t)
    ((tv', tv) : O.type_expression * O.type_expression)
    : unit
  =
  trace_option ~raise (assert_equal loc tv' tv)
  @@ O.assert_type_expression_eq (tv', tv)


(*
  This function operates on the return type of Context.get_sum.
  If type match the constructor label and its argument type, warns user about ambiguous constructor
*)
let warn_ambiguous_constructor ~raise loc (var_chosen, c_arg_t) ignored =
  let ignored_match =
    List.find
      ~f:(fun (_, _, a, _) ->
        Option.is_some (O.Misc.assert_type_expression_eq (c_arg_t, a)))
      ignored
  in
  match ignored_match with
  | Some (var_ignored, _, _, _) ->
    raise.warning
      (`Checking_ambiguous_constructor (loc, var_chosen, var_ignored))
  | None -> ()


let t_subst t ~tvar ~type_ = O.Helpers.subst_no_capture_type tvar type_ t

let t_exists ~loc (evar : Exists_var.t) =
  t_variable ~loc (evar :> O.type_variable) ()


let t_subst_var ~loc t ~tvar ~tvar' =
  t_subst t ~tvar ~type_:(t_variable ~loc tvar' ())


let t_subst_evar ~loc t ~tvar ~evar =
  t_subst t ~tvar ~type_:(t_exists ~loc evar)


let get_signature
    ~raise
    ~loc
    ctx
    ((local_module, path) : O.module_variable List.Ne.t)
  =
  let try_ ~f t mvar =
    let sig_ =
      trace_option ~raise (unbound_module_variable mvar loc) @@ f t mvar
    in
    if debug
    then
      Format.printf
        "@[Get Signature@.MVar: %a@.Signature: %a@]\n"
        Module_var.pp
        mvar
        Signature.pp
        sig_;
    sig_
  in
  List.fold
    path
    ~init:(try_ ~f:Context.get_module ctx local_module)
    ~f:(fun sig_ mvar -> try_ ~f:Signature.get_module sig_ mvar)


let rec evaluate_type ~raise ~(ctx : Context.t) (type_ : I.type_expression)
    : O.type_expression
  =
  let loc = type_.location in
  let self ?(ctx = ctx) = evaluate_type ~raise ~ctx in
  let return content = make_t ~loc:type_.location content (Some type_) in
  match type_.type_content with
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow { type1; type2 }
  | T_sum m ->
    let rows =
      let layout = Option.value ~default:default_layout m.layout in
      let aux
          ({ associated_type; michelson_annotation; decl_pos } : I.row_element)
        =
        let associated_type = self associated_type in
        ({ associated_type; michelson_annotation; decl_pos } : O.row_element)
      in
      let fields = Record.LMap.map aux m.fields in
      O.{ fields; layout }
    in
    return @@ T_sum rows
  | T_record m ->
    let rows =
      let layout = Option.value ~default:default_layout m.layout in
      let aux
          ({ associated_type; michelson_annotation; decl_pos } : I.row_element)
        =
        let associated_type = self associated_type in
        ({ associated_type; michelson_annotation; decl_pos } : O.row_element)
      in
      let fields = Record.LMap.map aux m.fields in
      O.{ fields; layout }
    in
    return @@ T_record rows
  | T_variable name ->
    (match Context.get_type ctx name with
    | Some x -> x
    | None ->
      (match Context.get_type_var ctx name with
      | Some _ -> return @@ T_variable name
      | None -> raise.error (unbound_type_variable name type_.location)))
  | T_app { type_operator; arguments } ->
    (* TODO: Remove strong normalization (GA) *)
    let operator =
      trace_option ~raise (unbound_type_variable type_operator type_.location)
      @@ Context.get_type ctx type_operator
    in
    let is_fully_applied location (t : O.type_expression) =
      match t.type_content with
      | T_abstraction x ->
        let rec aux : O.type_expression * int -> O.type_expression * int =
         fun (t, i) ->
          match t.type_content with
          | T_abstraction x -> aux (x.type_, i + 1)
          | _ -> t, i
        in
        let expected = snd @@ aux (x.type_, 1) in
        raise.error (type_app_wrong_arity None expected 0 location)
      | _ -> ()
    in
    let aux : I.type_expression -> O.type_expression =
     fun t ->
      let t' = self t in
      is_fully_applied t.location t';
      t'
    in
    let arguments = List.map ~f:aux arguments in
    let vars, ty_body = O.Helpers.destruct_type_abstraction operator in
    let vargs =
      match List.zip vars arguments with
      | Unequal_lengths ->
        let actual = List.length arguments in
        let expected = List.length vars in
        raise.error
          (type_app_wrong_arity
             (Some type_operator)
             expected
             actual
             type_.location)
      | Ok x -> x
    in
    let res =
      (* Note:
        Currently, there is no way for ty_body to look like `fun 'a 'b -> forall 'a 'b . <some type>` `fun 'a 'b -> 'a * (fun 'b -> <type>)`
        so it is fine to use `psubst_type`. If this changes, we should use `subst_type` and capture the FV in the right element of vargs *)
      let table = O.Helpers.TMap.of_list vargs in
      O.Helpers.psubst_type table ty_body
    in
    return res.type_content
  | T_module_accessor { module_path; element } ->
    let sig_ = get_signature ~raise ~loc ctx (List.Ne.of_list module_path) in
    trace_option ~raise (unbound_type_variable element type_.location)
    @@ Signature.get_type sig_ element
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x ->
    let ctx = Context.add_type_var ctx x.ty_binder x.kind in
    let type_ = self ~ctx x.type_ in
    return @@ T_abstraction { x with type_ }
  | T_for_all x ->
    let ctx = Context.add_type_var ctx x.ty_binder x.kind in
    let type_ = self ~ctx x.type_ in
    return @@ T_for_all { x with type_ }


let type_value_attr : I.ValueAttr.t -> O.ValueAttr.t =
 fun { inline; no_mutation; view; public; hidden; thunk } ->
  { inline; no_mutation; view; public; hidden; thunk }


let infer_literal ~raise ~loc lit
    : O.type_expression * (O.expression, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  let return type_ =
    type_, return @@ O.make_e ~location:loc (E_literal lit) type_
  in
  match lit with
  | Literal_unit -> return @@ t_unit ~loc ()
  | Literal_string _ -> return @@ t_string ~loc ()
  | Literal_key _ -> return @@ t_key ~loc ()
  | Literal_key_hash _ -> return @@ t_key_hash ~loc ()
  | Literal_chain_id _ -> return @@ t_chain_id ~loc ()
  | Literal_signature _ -> return @@ t_signature ~loc ()
  | Literal_bytes _ -> return @@ t_bytes ~loc ()
  | Literal_int _ -> return @@ t_int ~loc ()
  | Literal_nat _ -> return @@ t_nat ~loc ()
  | Literal_timestamp _ -> return @@ t_timestamp ~loc ()
  | Literal_mutez _ -> return @@ t_mutez ~loc ()
  | Literal_address _ -> return @@ t_address ~loc ()
  | Literal_operation _ -> return @@ t_operation ~loc ()
  | Literal_bls12_381_g1 _ -> return @@ t_bls12_381_g1 ~loc ()
  | Literal_bls12_381_g2 _ -> return @@ t_bls12_381_g2 ~loc ()
  | Literal_bls12_381_fr _ -> return @@ t_bls12_381_fr ~loc ()
  | Literal_chest _ | Literal_chest_key _ ->
    raise.error
      (corner_case
         "chest / chest_key not allowed in the syntax (only tests need this \
          type)")


let equal_lmap_doms lmap1 lmap2 =
  let open Record in
  let dom lmap = LSet.of_list (LMap.keys lmap) in
  LSet.equal (dom lmap1) (dom lmap2)


type nonrec raise = (Errors.typer_error, Main_warnings.all) raise

let rec check_expression
    ~(raise : raise)
    ~options
    ~ctx
    (expr : I.expression)
    (type_ : O.type_expression)
    : Context.t * (O.expression, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  Context.Hashes.set_context ctx;
  (* Debug stuff *)
  if debug
  then
    Format.printf
      "@[<hv>Check@.Context: %a@.Expr: %a@.Type: %a@]\n"
      pp_local_context
      ctx
      I.PP.expression
      expr
      O.PP.type_expression
      type_;
  if assertions
  then (
    assert (Well_formed.context ctx);
    assert (
      match Well_formed.type_expr ~ctx type_ with
      | Some Type -> true
      | _ -> false));
  let loc = expr.location in
  let return content = return @@ O.make_e ~location:loc content type_ in
  let check ?(raise = raise) ?(options = options) ?(ctx = ctx) expr type_ =
    check_expression ~raise ~options ~ctx expr type_
  and infer ?(raise = raise) ?(options = options) ?(ctx = ctx) expr =
    infer_expression ~raise ~options ~ctx expr
  in
  let ctx, expr' =
    match expr.expression_content, type_.type_content with
    | E_literal lit, T_constant _ ->
      let lit_type, expr = infer_literal ~raise ~loc lit in
      Assert.assert_true
        ~raise
        (assert_equal loc lit_type type_)
        (O.type_expression_eq (lit_type, type_));
      ctx, expr
    (* TODO: Not keen about this (add alpha equiv in type) *)
    | ( E_type_abstraction { type_binder = tvar; result }
      , T_for_all { ty_binder = tvar'; kind; type_ } )
      when Type_var.equal tvar tvar' ->
      let ctx, pos = Context.mark ctx ~mut:false in
      let ctx, result =
        check ~ctx:Context.(ctx |:: C_type_var (tvar', kind)) result type_
      in
      ( Context.drop_until ctx ~pos
      , let%bind result = result in
        return @@ E_type_abstraction { type_binder = tvar; result } )
    | _, T_for_all { ty_binder = tvar; kind; type_ } ->
      let tvar' = Type_var.fresh_like ~loc tvar in
      let ctx, pos = Context.mark ctx ~mut:false in
      let ctx, result =
        check
          ~ctx:Context.(ctx |:: C_type_var (tvar', kind))
          expr
          (t_subst_var ~loc type_ ~tvar ~tvar')
      in
      ( Context.drop_until ctx ~pos
      , let%bind result = result in
        return @@ E_type_abstraction { type_binder = tvar; result } )
    | E_lambda lambda, T_arrow { type1 = arg_type; type2 = ret_type } ->
      let ctx, lambda =
        check_lambda ~raise ~loc ~options ~ctx lambda arg_type ret_type
      in
      ( ctx
      , let%bind lambda = lambda in
        return @@ E_lambda lambda )
    | E_record record, T_record row ->
      (* Check domain of record and row are consistent *)
      if not (equal_lmap_doms record row.fields)
      then raise.error (record_mismatch loc expr type_);
      (* Type check record using row *)
      let ctx, record =
        Record.LMap.fold_map record ~init:ctx ~f:(fun label expr ctx ->
            let expr_type = Record.LMap.find label row.fields in
            check ~ctx expr expr_type.associated_type)
      in
      ( ctx
      , let%bind record = Elaboration.all_lmap record in
        return @@ E_record record )
    | E_update { struct_; path; update }, T_record row ->
      let ctx, struct_ = check ~ctx struct_ type_ in
      let field_row_elem =
        trace_option ~raise (bad_record_access path loc)
        @@ Record.LMap.find_opt path row.fields
      in
      let ctx, update = check ~ctx update field_row_elem.associated_type in
      ( ctx
      , let%bind struct_ = struct_
        and update = update in
        return @@ E_update { struct_; path; update } )
    | E_constructor { constructor; element }, T_sum row ->
      (* Find row element *)
      let constructor_row_elem =
        trace_option ~raise (bad_constructor loc constructor type_)
        @@ Record.LMap.find_opt constructor row.fields
      in
      (* Type check element *)
      let ctx, element =
        check ~ctx element constructor_row_elem.associated_type
      in
      ( ctx
      , let%bind element = element in
        return @@ E_constructor { constructor; element } )
    | E_matching { matchee; cases }, _ ->
      (* Infer type of matchee *)
      let ctx, matchee_type, matchee = infer ~ctx matchee in
      (* Type check the match cases *)
      let ctx, cases =
        check_cases ~raise ~options ~ctx cases matchee_type type_
      in
      (* Elaborate (by compiling pattern) *)
      (* TODO: Assert matchee_type is fully resolved *)
      ( ctx
      , let%bind match_ =
          compile_match ~options ~loc ~ctx matchee cases matchee_type
        in
        return match_ )
    | _ ->
      let ctx, type_', expr = infer expr in
      let ctx, f =
        subtype
          ~raise
          ~loc
          ~ctx
          ~received:(Context.apply ctx type_')
          ~expected:(Context.apply ctx type_)
      in
      ctx, Elaboration.map ~f expr
  in
  if debug
  then
    Format.printf
      "@[<hv>Check (After)@.Context: %a@.Expr: %a@]\n"
      pp_local_context
      ctx
      I.PP.expression
      expr;
  ctx, expr'


and infer_expression ~(raise : raise) ~options ~ctx (expr : I.expression)
    : Context.t * O.type_expression * (O.expression, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  Context.Hashes.set_context ctx;
  (* Debug stuff *)
  if debug
  then
    Format.printf
      "@[<hv>Infer@.Context: %a@.Expr: %a@]\n"
      pp_local_context
      ctx
      I.PP.expression
      expr;
  Format.print_flush ();
  if assertions then assert (Well_formed.context ctx);
  let loc = expr.location in
  let return content type_ = return @@ O.make_e ~location:loc content type_ in
  let lift (expr : (O.expression, _, _) Elaboration.t) =
    let%bind expr = expr in
    return expr.expression_content expr.type_expression
  in
  let check ?(raise = raise) ?(options = options) ?(ctx = ctx) expr type_ =
    check_expression ~raise ~options ~ctx expr type_
  and infer ?(raise = raise) ?(options = options) ?(ctx = ctx) expr =
    infer_expression ~raise ~options ~ctx expr
  in
  let ctx, type_, expr' =
    match expr.expression_content with
    | E_literal lit ->
      let type_, expr = infer_literal ~raise ~loc lit in
      ctx, type_, expr
    | E_constant { cons_name = const; arguments = args } ->
      infer_constant ~raise ~options ~ctx ~loc const args
    | E_variable var ->
      (match Context.get_value ctx var with
      | Error `Not_found -> raise.error @@ unbound_variable var expr.location
      | Error `Mut_var_captured ->
        raise.error @@ mut_var_captured expr.location var
      | Ok (mut_flag, type_) ->
        ( ctx
        , type_
        , (match mut_flag with
          | Immutable -> return (E_variable var) type_
          | Mutable -> return (E_deref var) type_) ))
    | E_lambda lambda -> infer_lambda ~raise ~options ~loc ~ctx lambda
    | E_application { lamb; args } ->
      let ctx, lamb_type, lamb = infer lamb in
      let ctx, ret_type, f, args =
        infer_application
          ~raise
          ~loc
          ~options
          ~ctx
          (Context.apply ctx lamb_type)
          args
      in
      ( ctx
      , ret_type
      , let%bind lamb = lamb
        and args = args in
        return (E_application { lamb = f lamb; args }) ret_type )
    | E_type_abstraction { type_binder = tvar; result } ->
      Context.Generalization.enter ~ctx ~mut:false ~in_:(fun ctx ->
          let ctx, ret_type, result =
            infer ~ctx:Context.(ctx |:: C_type_var (tvar, Type)) result
          in
          let ret_type = O.t_for_all tvar Type ret_type in
          ( ctx
          , ret_type
          , let%bind result = result in
            return (E_type_abstraction { type_binder = tvar; result }) ret_type
          ))
    | E_let_in { let_binder; rhs; let_result; attr } ->
      let rhs_ascr = Binder.get_ascr let_binder in
      let rhs =
        Option.value_map
          rhs_ascr
          ~f:(fun rhs_ascr -> I.e_ascription ~loc rhs rhs_ascr)
          ~default:rhs
      in
      let ctx, rhs_type, rhs = infer rhs in
      let rhs_type = Context.apply ctx rhs_type in
      let ctx, res_type, let_result =
        Context.enter ~ctx ~mut:false ~in_:(fun ctx ->
            infer
              ~ctx:
                Context.(
                  ctx
                  |:: C_value (Binder.get_var let_binder, Immutable, rhs_type))
              let_result)
      in
      let attr = type_value_attr attr in
      ( ctx
      , res_type
      , let%bind rhs = rhs
        and let_result = let_result in
        return
          (E_let_in
             { let_binder = Binder.map (Fn.const rhs_type) let_binder
             ; rhs
             ; let_result
             ; attr
             })
          res_type )
    | E_type_in { type_binder = tvar; rhs; let_result } ->
      let rhs = evaluate_type ~raise ~ctx rhs in
      let ctx, res_type, let_result =
        infer ~ctx:Context.(ctx |:: C_type (tvar, rhs)) let_result
      in
      ctx, res_type, lift let_result
    | E_raw_code { language; code } ->
      let code, code_type =
        trace_option ~raise (not_annotated loc)
        @@ I.get_e_ascription code.expression_content
      in
      let code_type = evaluate_type ~raise ~ctx code_type in
      let ctx, _code_type, code = infer code in
      ( ctx
      , code_type
      , let%bind code = code in
        return
          (E_raw_code
             { language; code = { code with type_expression = code_type } })
          code_type )
    | E_ascription { anno_expr; type_annotation } ->
      let ascr = evaluate_type ~raise ~ctx type_annotation in
      let ctx, expr = check anno_expr ascr in
      ctx, ascr, lift expr
    | E_recursive { fun_name; fun_type; lambda } ->
      let fun_type = evaluate_type ~raise ~ctx fun_type in
      let ctx, lambda =
        check
          ~ctx:Context.(ctx |:: C_value (fun_name, Immutable, fun_type))
          (I.make_e ~loc (E_lambda (Lambda.map Fn.id Option.some lambda)))
          fun_type
      in
      ( ctx
      , fun_type
      , let%bind lambda = lambda in
        let%bind raise = Elaboration.raise in
        let lambda =
          trace_option
            ~raise
            (corner_case "Annotated lambda should return lambda")
          @@ O.get_e_lambda_opt lambda
        in
        return (E_recursive { fun_name; fun_type; lambda }) fun_type )
    | E_record record ->
      let (ctx, row_content), record =
        Record.LMap.fold_map
          record
          ~init:(ctx, Record.LMap.empty)
          ~f:(fun label expr (ctx, row_content) ->
            let ctx, expr_type, expr = infer ~ctx expr in
            let row_content = Record.LMap.add label expr_type row_content in
            (ctx, row_content), expr)
      in
      let _, row =
        (* No fold_mapi in utils :cry *)
        Record.LMap.fold_map
          row_content
          ~init:0
          ~f:(fun label associated_type i ->
            let decl_pos =
              let (Label str) = label in
              match Int.of_string str with
              | i -> i
              | exception _ -> i
            in
            ( i + 1
            , { Rows.associated_type; michelson_annotation = None; decl_pos } ))
      in
      let record_type =
        match Context.get_record row ctx with
        | None -> t_record ~loc ~layout:default_layout row
        | Some (orig_var, row) ->
          make_t_orig_var ~loc (T_record row) None orig_var
      in
      ( ctx
      , record_type
      , let%bind record = Elaboration.all_lmap record in
        return (E_record record) record_type )
    | E_accessor { struct_; path = field } ->
      let ctx, record_type, struct_ = infer ~ctx struct_ in
      let row =
        trace_option ~raise (expected_record loc record_type)
        @@ get_t_record (Context.apply ctx record_type)
      in
      let field_row_elem =
        trace_option ~raise (bad_record_access field loc)
        @@ Record.LMap.find_opt field row.fields
      in
      let field_type = field_row_elem.associated_type in
      ( ctx
      , field_type
      , let%bind struct_ = struct_ in
        return (E_accessor { struct_; path = field }) field_type )
    | E_update { struct_; path; update } ->
      let ctx, record_type, struct_ = infer ~ctx struct_ in
      let row =
        trace_option ~raise (expected_record loc record_type)
        @@ get_t_record (Context.apply ctx record_type)
      in
      let field_row_elem =
        trace_option ~raise (bad_record_access path loc)
        @@ Record.LMap.find_opt path row.fields
      in
      let ctx, update = check ~ctx update field_row_elem.associated_type in
      ( ctx
      , record_type
      , let%bind struct_ = struct_
        and update = update in
        return (E_update { struct_; path; update }) record_type )
    | E_constructor { constructor = Label label as constructor; _ }
      when String.(label = "M_right" || label = "M_left") ->
      raise.error (michelson_or_no_annotation constructor loc)
    | E_constructor { constructor; element = arg } ->
      if debug then Format.printf "Finding constructor sum...\n";
      Format.print_flush ();
      (* [tvars] are the parameters of the type *)
      let tvars, arg_type, sum_type =
        match Context.get_sum constructor ctx with
        | (tvar, tvars, arg_type, sum_type) :: ignored ->
          warn_ambiguous_constructor ~raise loc (tvar, arg_type) ignored;
          tvars, arg_type, sum_type
        | [] -> raise.error (unbound_constructor constructor loc)
      in
      if debug
      then (
        let (Label constr) = constructor in
        Format.printf
          "Found constructor type for %s. Type: %a\n"
          constr
          O.PP.type_expression
          sum_type);
      Format.print_flush ();
      let module TMap = O.Helpers.TMap in
      (* Instantiate [tvars] (assumption: kind is [Type]) *)
      let tvars : Exists_var.t TMap.t =
        tvars
        |> List.map ~f:(fun tvar -> tvar, Exists_var.fresh ~loc ())
        |> TMap.of_list
      in
      let ctx =
        Context.(
          ctx
          |@ of_list
               (TMap.values tvars
               |> List.map ~f:(fun evar -> C_exists_var (evar, Type))))
      in
      let arg_type =
        TMap.fold
          (fun tvar evar arg_type -> t_subst_evar ~loc arg_type ~tvar ~evar)
          tvars
          arg_type
      in
      let sum_type =
        TMap.fold
          (fun tvar evar sum_type -> t_subst_evar ~loc sum_type ~tvar ~evar)
          tvars
          sum_type
      in
      (* Check argument *)
      let ctx, arg = check ~ctx arg arg_type in
      ( ctx
      , sum_type
      , let%bind arg = arg in
        return (E_constructor { constructor; element = arg }) sum_type )
    | E_matching { matchee; cases } ->
      (* Infer type of matchee *)
      let ctx, matchee_type, matchee = infer ~ctx matchee in
      (* Add existential for return type *)
      let evar = Exists_var.fresh ~loc () in
      let ctx = Context.(ctx |:: C_exists_var (evar, Type)) in
      let ret_type = t_exists ~loc evar in
      (* Type check the match cases *)
      let ctx, cases =
        check_cases ~raise ~options ~ctx cases matchee_type ret_type
      in
      (* Elaborate (by compiling pattern) *)
      ( ctx
      , ret_type
      , let%bind match_ =
          compile_match ~options ~loc ~ctx matchee cases matchee_type
        in
        return match_ ret_type )
    | E_mod_in { module_binder = mvar; rhs; let_result } ->
      let ctx, sig_, rhs = infer_module_expr ~raise ~options ~ctx rhs in
      let ctx, ret_type, let_result =
        Context.enter ~ctx ~mut:false ~in_:(fun ctx ->
            infer ~ctx:Context.(ctx |:: C_module (mvar, sig_)) let_result)
      in
      ( ctx
      , ret_type
      , let%bind let_result = let_result
        and rhs = rhs in
        return (E_mod_in { module_binder = mvar; rhs; let_result }) ret_type )
    | E_module_accessor { module_path; element } ->
      let module_path' = List.Ne.of_list module_path in
      let sig_ = get_signature ~raise ~loc ctx module_path' in
      let pp_path ppf path =
        List.Ne.iter
          (fun mvar -> Format.fprintf ppf "%a." Module_var.pp mvar)
          path
      in
      if debug
      then
        Format.printf
          "@[Path: %a@.Element: %a@.Signature: %a@]\n"
          pp_path
          module_path'
          Value_var.pp
          element
          Signature.pp
          sig_;
      Format.print_flush ();
      let elt_type =
        trace_option ~raise (unbound_variable element loc)
        @@ Signature.get_value sig_ element
      in
      ( ctx
      , elt_type
      , return (E_module_accessor { module_path; element }) elt_type )
    | E_let_mut_in { let_binder; rhs; let_result; attr } ->
      let rhs_ascr = Binder.get_ascr let_binder in
      let rhs =
        Option.value_map
          rhs_ascr
          ~f:(fun rhs_ascr -> I.e_ascription ~loc rhs rhs_ascr)
          ~default:rhs
      in
      let ctx, rhs_type, rhs = infer rhs in
      let rhs_type = Context.apply ctx rhs_type in
      (* TODO: Improve inference here -- we could subtype it to get a monotype? *)
      if is_t_for_all rhs_type
      then raise.error (mut_is_polymorphic loc rhs_type);
      let ctx, res_type, let_result =
        Context.enter ~ctx ~mut:false ~in_:(fun ctx ->
            infer
              ~ctx:
                Context.(
                  ctx |:: C_value (Binder.get_var let_binder, Mutable, rhs_type))
              let_result)
      in
      let attr = type_value_attr attr in
      ( ctx
      , res_type
      , let%bind rhs = rhs
        and let_result = let_result in
        return
          (E_let_mut_in
             { let_binder = Binder.map (Fn.const rhs_type) let_binder
             ; rhs
             ; let_result
             ; attr
             })
          res_type )
    | E_assign { binder; expression } ->
      let type_ =
        trace_option
          ~raise
          (unbound_mut_variable
             (Binder.get_var binder)
             (Value_var.get_location @@ Binder.get_var binder))
        @@ Context.get_mut ctx (Binder.get_var binder)
      in
      let binder = Binder.map (Fn.const type_) binder in
      let ctx, expression = check ~ctx expression type_ in
      let ret_type = O.t_unit ~loc () in
      ( ctx
      , ret_type
      , let%bind expression = expression in
        return (E_assign { binder; expression }) ret_type )
    | E_for { binder; start; final; incr; f_body } ->
      let t_int = t_int ~loc () in
      let t_unit = t_unit ~loc () in
      let ctx, start = check ~ctx start t_int in
      let ctx, final = check ~ctx final t_int in
      let ctx, incr = check ~ctx incr t_int in
      let ctx, f_body =
        check
          ~ctx:Context.(ctx |:: C_value (binder, Immutable, t_int))
          f_body
          t_unit
      in
      ( ctx
      , t_unit
      , let%bind start = start
        and final = final
        and incr = incr
        and f_body = f_body in
        return (E_for { binder; start; final; incr; f_body }) t_unit )
    | E_for_each
        { fe_binder = (key_binder, Some val_binder) as fe_binder
        ; collection
        ; collection_type = (Map | Any) as collection_type
        ; fe_body
        } ->
      let t_unit = t_unit ~loc () in
      let ctx, type_, collection = infer ~ctx collection in
      let key_type, val_type =
        trace_option
          ~raise
          (mismatching_for_each_collection_type loc collection_type type_)
        @@ get_t_map (Context.apply ctx type_)
      in
      let ctx, fe_body =
        check
          ~ctx:
            Context.(
              ctx
              |:: C_value (key_binder, Immutable, key_type)
              |:: C_value (val_binder, Immutable, val_type))
          fe_body
          t_unit
      in
      ( ctx
      , t_unit
      , let%bind collection = collection
        and fe_body = fe_body in
        return
          (E_for_each { fe_binder; collection; collection_type = Map; fe_body })
          t_unit )
    | E_for_each
        { fe_binder = _, Some _
        ; collection_type = (List | Set) as collection_type
        ; _
        } ->
      raise.error @@ mismatching_for_each_binder_arity loc collection_type 2
    | E_for_each
        { fe_binder = (binder, None) as fe_binder
        ; collection
        ; collection_type
        ; fe_body
        } ->
      let t_unit = t_unit ~loc () in
      let ctx, type_, collection = infer ~ctx collection in
      let binder_type =
        trace_option
          ~raise
          (mismatching_for_each_collection_type loc collection_type type_)
        @@
        (* This is bad -- TODO: get rid of collection type (no-longer required) + use patterns *)
        let get_t_map type_ =
          get_t_map type_
          |> Option.map ~f:(fun (t1, t2) -> t_tuple ~loc [ t1; t2 ])
        in
        match (collection_type : For_each_loop.collect_type) with
        | Set -> get_t_set type_
        | List -> get_t_list type_
        | Map -> get_t_map type_
        | Any ->
          Option.first_some
            (get_t_set type_)
            (Option.first_some (get_t_list type_) (get_t_map type_))
      in
      let ctx, fe_body =
        check
          ~ctx:Context.(ctx |:: C_value (binder, Immutable, binder_type))
          fe_body
          t_unit
      in
      ( ctx
      , t_unit
      , let%bind collection = collection
        and fe_body = fe_body in
        return
          (E_for_each { fe_binder; collection; collection_type = Map; fe_body })
          t_unit )
    | E_while { cond; body } ->
      let t_unit = t_unit ~loc () in
      let ctx, cond = check ~ctx cond (t_bool ~loc ()) in
      let ctx, body = check ~ctx body t_unit in
      ( ctx
      , t_unit
      , let%bind cond = cond
        and body = body in
        return (E_while { cond; body }) t_unit )
  in
  if debug
  then
    Format.printf
      "@[<hv>Infer (After)@.Context: %a@.Expr: %a@.Type: %a@]\n"
      pp_local_context
      ctx
      I.PP.expression
      expr
      O.PP.type_expression
      type_;
  if assertions
  then (
    assert (Well_formed.context ctx);
    assert (
      match Well_formed.type_expr ~ctx type_ with
      | Some Type -> true
      | _ -> false));
  ctx, type_, expr'


and infer_constant ~(raise : raise) ~options ~ctx ~loc const args
    : Context.t * O.type_expression * (O.expression, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  let return args type_ =
    let%bind args = args in
    return
    (* Location updated due to implementation of Constant_typers. *)
    @@ O.make_e
         ~location:loc
         (E_constant { cons_name = const; arguments = args })
         { type_ with location = loc }
  in
  let ctx, args, ret_type =
    Constant_typers.infer_constant
      ~raise
      ~options
      ~infer:(infer_expression ~options)
      ~check:(check_expression ~options)
      ~ctx
      ~loc
      const
      args
  in
  ctx, ret_type, return args ret_type


and check_lambda
    ~raise
    ~loc
    ~options
    ~ctx
    ({ binder; output_type = ret_ascr; result } : _ Lambda.t)
    arg_type
    ret_type
    : Context.t * (_ Lambda.t, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  let result =
    Option.value_map ret_ascr ~default:result ~f:(fun ret_ascr ->
        I.e_ascription ~loc result ret_ascr)
  in
  let ctx, arg_type, f =
    match Param.get_ascr binder with
    | Some arg_ascr ->
      let arg_ascr = evaluate_type ~raise ~ctx arg_ascr in
      (* TODO: Kinding check for ascription *)
      let ctx, _f =
        subtype ~raise ~loc ~ctx ~received:arg_type ~expected:arg_ascr
      in
      (* Generate let binding for ascription subtyping, will be inlined later on *)
      ctx, arg_ascr, fun hole -> hole
    | None -> ctx, arg_type, fun x -> x
  in
  let arg_type = Context.apply ctx arg_type in
  let ret_type = Context.apply ctx ret_type in
  let ctx, pos = Context.mark ctx ~mut:true in
  let ctx, result =
    check_expression
      ~raise
      ~options
      ~ctx:
        Context.(
          ctx
          |:: C_value (Param.get_var binder, Param.get_mut_flag binder, arg_type))
      result
      ret_type
  in
  ( Context.drop_until ctx ~pos
  , let%bind result = result in
    return
      Lambda.
        { binder = Param.map (Fn.const arg_type) binder
        ; result = f result
        ; output_type = ret_type
        } )


and infer_lambda
    ~raise
    ~options
    ~loc
    ~ctx
    ({ binder; output_type = ret_ascr; result } : _ Lambda.t)
    : Context.t * O.type_expression * (O.expression, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  let return content type_ = return @@ O.make_e ~location:loc content type_ in
  (* Desugar return ascription to (result : ret_ascr) *)
  let result =
    Option.value_map ret_ascr ~default:result ~f:(fun ret_ascr ->
        I.e_ascription ~loc result ret_ascr)
  in
  Context.Generalization.enter ~ctx ~mut:true ~in_:(fun ctx ->
      let ctx, arg_type =
        match Param.get_ascr binder with
        | Some arg_ascr -> ctx, evaluate_type ~raise ~ctx arg_ascr
        | None ->
          let evar = Exists_var.fresh ~loc () in
          Context.(ctx |:: C_exists_var (evar, Type)), t_exists ~loc evar
      in
      let ctx, ret_type, result =
        infer_expression
          ~raise
          ~options
          ~ctx:
            Context.(
              ctx
              |:: C_value
                    (Param.get_var binder, Param.get_mut_flag binder, arg_type))
          result
      in
      let type_ = O.t_arrow ~loc arg_type ret_type () in
      let lambda : (O.expression, _, _) Elaboration.t =
        let%bind result = result in
        return
          (E_lambda
             { binder = Param.map (Fn.const arg_type) binder
             ; result
             ; output_type = ret_type
             })
          type_
      in
      ctx, type_, lambda)


and infer_application ~raise ~loc ~options ~ctx lamb_type args
    : Context.t
      * O.type_expression
      * (O.expression -> O.expression)
      * (O.expression, _, _) Elaboration.t
  =
  let self = infer_application ~raise ~loc ~options in
  let check = check_expression ~raise ~options in
  let fail () = raise.error (should_be_a_function_type lamb_type args) in
  match lamb_type.type_content with
  | T_for_all { ty_binder = tvar; kind; type_ } ->
    let evar = Exists_var.fresh ~loc () in
    let lamb_type = t_subst type_ ~tvar ~type_:(t_exists ~loc evar) in
    let ctx, ret_type, f, args =
      self ~ctx:Context.(ctx |:: C_exists_var (evar, kind)) lamb_type args
    in
    ( ctx
    , ret_type
    , (fun hole ->
        f
          (O.e_type_inst
             { forall = hole; type_ = t_exists ~loc evar }
             lamb_type))
    , args )
  | T_arrow { type1 = arg_type; type2 = ret_type } ->
    let ctx, args = check ~ctx args arg_type in
    let ctx, ret_type =
      (* Attempt to reduce external types using external typers. *)
      try_with
        (fun ~raise ~catch:_ ->
          let ret_type = Context.apply ctx ret_type in
          match ret_type.type_content with
          | T_constant { injection = External "int"; parameters; _ } ->
            Constant_typers.External_types.int_types ~raise ~loc ~ctx parameters
          | T_constant
              { injection = External ("ediv" | "u_ediv"); parameters; _ } ->
            Constant_typers.External_types.ediv_types
              ~raise
              ~loc
              ~ctx
              parameters
          | T_constant { injection = External ("and" | "u_and"); parameters; _ }
            ->
            Constant_typers.External_types.and_types ~raise ~loc ~ctx parameters
          | _ -> ctx, ret_type)
        (fun ~catch:_ _ -> ctx, ret_type)
    in
    let ret_type = { ret_type with location = loc } in
    ctx, ret_type, (fun hole -> hole), args
  | T_variable tvar ->
    (match Exists_var.of_type_var tvar with
    | None -> fail ()
    | Some evar ->
      let kind =
        Context.get_exists_var ctx evar
        |> trace_option ~raise (unbound_exists_variable loc evar)
      in
      if not
           (match kind with
           | Type -> true
           | _ -> false)
      then
        raise.error
          (corner_case
             "Existential variable used in application has invalid kind");
      let evar1 = Exists_var.fresh ~loc () in
      let evar2 = Exists_var.fresh ~loc () in
      let arg_type = t_exists ~loc evar1 in
      let ret_type = t_exists ~loc evar2 in
      let hole =
        Context.of_list
          [ C_exists_var (evar1, Type)
          ; C_exists_var (evar2, Type)
          ; C_exists_eq (evar, Type, O.t_arrow ~loc arg_type ret_type ())
          ]
      in
      let ctx, args =
        check
          ~ctx:Context.(insert_at ctx ~at:(C_exists_var (evar, Type)) ~hole)
          args
          arg_type
      in
      ctx, ret_type, (fun hole -> hole), args)
  | _ -> fail ()


and infer_pattern
    ~(raise : raise)
    ~ctx
    (pat : I.type_expression option I.Pattern.t)
    : Context.t
      * O.type_expression
      * (O.type_expression O.Pattern.t, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  let loc = pat.location in
  let return content =
    return @@ (Location.wrap ~loc content : O.type_expression O.Pattern.t)
  in
  let infer ~ctx pat = infer_pattern ~raise ~ctx pat in
  let check ~ctx pat type_ = check_pattern ~raise ~ctx pat type_ in
  match pat.wrap_content with
  | P_unit -> ctx, t_unit ~loc (), return @@ P_unit
  | P_var binder ->
    let var = Binder.get_var binder in
    let ctx, type_ =
      match Binder.get_ascr binder with
      | Some ascr ->
        let ascr = evaluate_type ~raise ~ctx ascr in
        ctx, ascr
      | None ->
        let evar = Exists_var.fresh ~loc () in
        Context.(ctx |:: C_exists_var (evar, Type)), t_exists ~loc evar
    in
    let ctx = Context.(ctx |:: C_value (var, Immutable, type_)) in
    ctx, type_, return @@ P_var (Binder.set_ascr binder type_)
  | P_list (Cons (hd_pat, tl_pat)) ->
    let ctx, elt_type, hd_pat = infer ~ctx hd_pat in
    let ctx, tl_pat = check ~ctx tl_pat (t_list ~loc elt_type) in
    ( ctx
    , t_list ~loc elt_type
    , let%bind hd_pat = hd_pat
      and tl_pat = tl_pat in
      return @@ P_list (Cons (hd_pat, tl_pat)) )
  | P_list (List list_pat) ->
    let evar = Exists_var.fresh ~loc () in
    let ctx = Context.(ctx |:: C_exists_var (evar, Type)) in
    let elt_type = t_exists ~loc evar in
    let ctx, list_pat =
      List.fold_right list_pat ~init:(ctx, []) ~f:(fun elt (ctx, list_pat) ->
          let ctx, elt = check ~ctx elt elt_type in
          ctx, elt :: list_pat)
    in
    ( ctx
    , t_list ~loc elt_type
    , let%bind list_pat = Elaboration.all list_pat in
      return @@ P_list (List list_pat) )
  | P_variant (constructor, arg_pat) ->
    let tvars, arg_type, sum_type =
      match Context.get_sum constructor ctx with
      | (tvar, tvars, arg_type, sum_type) :: ignored ->
        warn_ambiguous_constructor ~raise loc (tvar, arg_type) ignored;
        tvars, arg_type, sum_type
      | [] -> raise.error (unbound_constructor constructor loc)
    in
    let module TMap = O.Helpers.TMap in
    (* Instantiate [tvars] (assumption: kind is [Type]) *)
    let tvars : Exists_var.t TMap.t =
      tvars
      |> List.map ~f:(fun tvar -> tvar, Exists_var.fresh ~loc ())
      |> TMap.of_list
    in
    let ctx =
      Context.(
        ctx
        |@ of_list
             (TMap.values tvars
             |> List.map ~f:(fun evar -> C_exists_var (evar, Type))))
    in
    let arg_type =
      TMap.fold
        (fun tvar evar arg_type -> t_subst_evar ~loc arg_type ~tvar ~evar)
        tvars
        arg_type
    in
    let sum_type =
      TMap.fold
        (fun tvar evar sum_type -> t_subst_evar ~loc sum_type ~tvar ~evar)
        tvars
        sum_type
    in
    (* Check argument *)
    let ctx, arg_pat = check ~ctx arg_pat arg_type in
    ( ctx
    , sum_type
    , let%bind arg_pat = arg_pat in
      return @@ P_variant (constructor, arg_pat) )
  | P_tuple tuple_pat ->
    let (ctx, tuple_types), tuple_pat =
      List.fold_mapi
        tuple_pat
        ~init:(ctx, [])
        ~f:(fun i (ctx, tuple_types) pat ->
          let ctx, pat_type, pat = infer ~ctx pat in
          let row_elem =
            { Rows.associated_type = pat_type
            ; decl_pos = i
            ; michelson_annotation = None
            }
          in
          (ctx, (Label.Label (Int.to_string i), row_elem) :: tuple_types), pat)
    in
    let tuple_type =
      Record.of_list tuple_types |> t_record ~loc ~layout:default_layout
    in
    ( ctx
    , tuple_type
    , let%bind tuple_pat = Elaboration.all tuple_pat in
      return @@ P_tuple tuple_pat )
  | P_record lps ->
    let (ctx, row_content), record_pat =
      Record.LMap.fold_map
        lps
        ~init:(ctx, Record.LMap.empty)
        ~f:(fun label pat (ctx, row_content) ->
          let ctx, pat_type, pat = infer ~ctx pat in
          let row_content = Record.LMap.add label pat_type row_content in
          (ctx, row_content), (label, pat))
    in
    let _, row =
      (* No fold_mapi in utils :cry *)
      Record.LMap.fold_map
        row_content
        ~init:0
        ~f:(fun label associated_type i ->
          let decl_pos =
            let (Label str) = label in
            match Int.of_string str with
            | i -> i
            | exception _ -> i
          in
          i + 1, { Rows.associated_type; michelson_annotation = None; decl_pos })
    in
    let record_type =
      match Context.get_record row ctx with
      | None -> t_record ~loc ~layout:default_layout row
      | Some (orig_var, row) ->
        make_t_orig_var ~loc (T_record row) None orig_var
    in
    let labels, pats = List.unzip (Record.LMap.values record_pat) in
    ( ctx
    , record_type
    , let%bind pats = Elaboration.all pats in
      return
      @@ P_record (Record.of_list (List.zip_exn labels pats))
    )


and check_pattern
    ~(raise : raise)
    ~ctx
    (pat : I.type_expression option I.Pattern.t)
    (type_ : O.type_expression)
    : Context.t * (O.type_expression O.Pattern.t, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  let loc = pat.location in
  let err () = pattern_do_not_conform_type pat type_ in
  let fail () = raise.error (err ()) in
  let return content =
    return @@ (Location.wrap ~loc content : O.type_expression O.Pattern.t)
  in
  let self ?(raise = raise) = check_pattern ~raise in
  let infer ~ctx pat = infer_pattern ~raise ~ctx pat in
  let ctx, pat =
    match pat.wrap_content, type_.type_content with
    | P_unit, O.T_constant { injection = Literal_types.Unit; _ } ->
      ctx, return @@ P_unit
    | P_unit, _ -> fail ()
    | P_var binder, _ ->
      (* TODO: Check if the binder annot is consistent with expected type *)
      ( Context.(ctx |:: C_value (Binder.get_var binder, Immutable, type_))
      , return @@ P_var (Binder.map (Fn.const @@ type_) binder) )
    | ( P_list (Cons (hd_pat, tl_pat))
      , O.T_constant
          { injection = Literal_types.List; parameters = [ elt_type ]; _ } ) ->
      let ctx, hd_pat = self ~ctx hd_pat elt_type in
      let ctx, tl_pat = self ~ctx tl_pat type_ in
      ( ctx
      , let%bind hd_pat = hd_pat
        and tl_pat = tl_pat in
        return @@ P_list (Cons (hd_pat, tl_pat)) )
    | ( P_list (List list_pat)
      , O.T_constant
          { injection = Literal_types.List; parameters = [ elt_type ]; _ } ) ->
      let ctx, list_pat =
        List.fold_right list_pat ~init:(ctx, []) ~f:(fun elt (ctx, list_pat) ->
            let ctx, elt = self ~ctx elt elt_type in
            ctx, elt :: list_pat)
      in
      ( ctx
      , let%bind list_pat = Elaboration.all list_pat in
        return @@ P_list (List list_pat) )
    | P_variant (label, arg_pat), O.T_sum row ->
      let label_row_elem =
        trace_option ~raise (err ()) @@ Record.LMap.find_opt label row.fields
      in
      let ctx, arg_pat = self ~ctx arg_pat label_row_elem.associated_type in
      ( ctx
      , let%bind arg_pat = arg_pat in
        return @@ P_variant (label, arg_pat) )
    | P_tuple tuple_pat, O.T_record row ->
      if Record.LMap.cardinal row.fields <> List.length tuple_pat
      then raise.error (fail ());
      let ctx, tuple_pat =
        List.fold_mapi tuple_pat ~init:ctx ~f:(fun i ctx pat ->
            let pat_row_elem =
              trace_option ~raise (err ())
              @@ Record.LMap.find_opt (Label (Int.to_string i)) row.fields
            in
            self ~ctx pat pat_row_elem.associated_type)
      in
      ( ctx
      , let%bind tuple_pat = Elaboration.all tuple_pat in
        return @@ P_tuple tuple_pat )
    | P_record lps, O.T_record row ->
      if Record.LMap.cardinal row.fields <> Record.LMap.cardinal lps
      then raise.error (fail ());
      let ctx, record_pat =
        Record.LMap.fold_map lps ~init:ctx ~f:(fun label pat ctx ->
            let label_row_elem =
              trace_option ~raise (err ())
              @@ Record.LMap.find_opt label row.fields
            in
            let ctx, pat = self ~ctx pat label_row_elem.associated_type in
            ctx, (label, pat))
      in
      let labels, pats = List.unzip (Record.LMap.values record_pat) in
      ( ctx
      , let%bind pats = Elaboration.all pats in
        return
        @@ P_record (Record.of_list (List.zip_exn labels pats)) )
    | _ ->
      let ctx, type_', pat = infer ~ctx pat in
      let ctx, _f =
        subtype
          ~raise
          ~loc
          ~ctx
          ~received:(Context.apply ctx type_')
          ~expected:(Context.apply ctx type_)
      in
      ctx, pat
  in
  (* if debug then Format.printf "Ctx After Pattern (Check): %a\n" Context.pp_ ctx; *)
  ctx, pat


and check_cases
    ~(raise : raise)
    ~options
    ~ctx
    (cases :
      (I.expression, I.type_expression option) I.Match_expr.match_case list)
    matchee_type
    ret_type
    : Context.t
      * ( (O.type_expression O.Pattern.t * O.expression) list
        , _
        , _ )
        Elaboration.t
  =
  let open Elaboration.Let_syntax in
  let ctx, cases =
    List.fold_map ~init:ctx cases ~f:(fun ctx { pattern; body } ->
        let ctx, pos = Context.mark ctx ~mut:false in
        let matchee_type = Context.apply ctx matchee_type in
        if debug
        then
          Format.printf "Matchee type: %a\n" O.PP.type_expression matchee_type;
        let ctx, pattern = check_pattern ~raise ~ctx pattern matchee_type in
        let ctx, body = check_expression ~raise ~options ~ctx body ret_type in
        ( Context.drop_until ctx ~pos
        , let%map pattern = pattern
          and body = body in
          pattern, body ))
  in
  ctx, Elaboration.all cases


and compile_match
    ~options
    ~loc
    ~ctx
    (matchee : (O.expression, _, _) Elaboration.t)
    cases
    matchee_type
    : (O.expression_content, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  let%bind matchee = matchee in
  let%bind cases = cases in
  (* Check anomalies *)
  let matchee_type = Context.apply ctx matchee_type in
  (* TODO: Assert matchee_type is fully resolved *)
  let eqs = List.map cases ~f:(fun (pat, body) -> pat, matchee_type, body) in
  let%bind raise = Elaboration.raise in
  let syntax = options.Compiler_options.syntax_for_errors in
  let () =
    Pattern_anomalies.check_anomalies ~raise ~syntax ~loc eqs matchee_type
  in
  (* Elaborate (by compiling pattern) *)
  return
  @@
  let cases =
    List.map cases ~f:(fun (pattern, body) -> O.Match_expr.{ pattern; body })
  in
  O.E_matching { matchee; cases }


and infer_module_expr ~raise ~options ~ctx (mod_expr : I.module_expr)
    : Context.t * Signature.t * (O.module_expr, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  let loc = mod_expr.location in
  let return content = return @@ (Location.wrap ~loc content : O.module_expr) in
  match mod_expr.wrap_content with
  | M_struct decls ->
    let ctx, sig_, decls = infer_module ~raise ~options ~ctx decls in
    ( ctx
    , sig_
    , let%bind decls = decls in
      return (M_struct decls) )
  | M_module_path path ->
    (* Check we can access every element in [path] *)
    let sig_ = get_signature ~raise ~loc ctx path in
    ctx, sig_, return (M_module_path path)
  | M_variable mvar ->
    (* Check we can access [mvar] *)
    let sig_ =
      trace_option ~raise (unbound_module_variable mvar loc)
      @@ Context.get_module ctx mvar
    in
    ctx, sig_, return (M_variable mvar)


and infer_declaration ~(raise : raise) ~options ~ctx (decl : I.declaration)
    : Context.t * Signature.item * (O.declaration, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  (* Debug *)
  if debug
  then
    Format.printf "@[<hv>Infer Declaration@.Decl: %a@]\n" I.PP.declaration decl;
  let loc = decl.location in
  let return (content : O.declaration_content) =
    return @@ Location.wrap ~loc content
  in
  let ctx, (sig_item : Signature.item), decl' =
    match decl.wrap_content with
    | D_type { type_binder; type_expr; type_attr = { public; hidden } } ->
      let type_expr = evaluate_type ~raise ~ctx type_expr in
      let type_expr = { type_expr with orig_var = Some type_binder } in
      ( ctx
      , S_type (type_binder, type_expr)
      , return
        @@ D_type { type_binder; type_expr; type_attr = { public; hidden } } )
    | D_value { binder; attr; expr } ->
      let var = Binder.get_var binder in
      let ascr = Binder.get_ascr binder in
      let expr =
        Option.value_map ascr ~default:expr ~f:(fun ascr ->
            I.e_ascription ~loc expr ascr)
      in
      let ascr = Option.map ascr ~f:(evaluate_type ~raise ~ctx) in
      let ctx, expr_type, expr =
        trace ~raise (constant_declaration_tracer loc var expr ascr)
        @@ infer_expression ~options ~ctx expr
      in
      let attr = type_value_attr attr in
      (* if debug then Format.printf "Ctx After Decl: %a\n" Context.pp_ ctx; *)
      ( ctx
      , S_value (var, expr_type)
      , let%bind expr = expr in
        return
        @@ D_value
             { binder = Binder.map (Fn.const @@ Some expr_type) binder
             ; expr
             ; attr
             } )
    | D_module { module_binder; module_; module_attr = { public; hidden } } ->
      let ctx, sig_, module_ = infer_module_expr ~raise ~options ~ctx module_ in
      ( ctx
      , S_module (module_binder, sig_)
      , let%bind module_ = module_ in
        return
        @@ D_module { module_binder; module_; module_attr = { public; hidden } }
      )
  in
  if debug
  then
    Format.printf
      "@[<hv>Infer Declaration@.Decl: %a@.Signature Item: %a@]\n"
      I.PP.declaration
      decl
      Signature.pp_item
      sig_item;
  ctx, sig_item, decl'


and infer_decl ~(raise : raise) ~options ~ctx (decl : I.decl)
    : Context.t * Signature.item * (O.decl, _, _) Elaboration.t
  =
  let ctx, sig_item, decl = infer_declaration ~raise ~options ~ctx decl in
  ctx, sig_item, decl


and infer_module ~raise ~options ~ctx (module_ : I.module_)
    : Context.t * Signature.t * (O.module_, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  match module_ with
  | [] -> ctx, [], return []
  | decl :: module_ ->
    Context.decl_enter ~ctx ~in_:(fun ctx ->
        let ctx, sig_item, decl = infer_decl ~raise ~options ~ctx decl in
        let ctx = Context.add_signature_item ctx sig_item in
        let ctx, sig_, decls = infer_module ~raise ~options ~ctx module_ in
        ( ctx
        , sig_item :: sig_
        , let%bind decl = decl
          and decls = decls in
          return (decl :: decls) ))


let rec infer_program ~raise ~options ~ctx (program : I.program)
    : Context.t * Signature.t * (O.program, _, _) Elaboration.t
  =
  let open Elaboration.Let_syntax in
  match program with
  | [] -> ctx, [], return []
  | decl :: program ->
    Context.decl_enter ~ctx ~in_:(fun ctx ->
        let ctx, sig_item, decl = infer_declaration ~raise ~options ~ctx decl in
        let ctx = Context.add_signature_item ctx sig_item in
        let ctx, sig_, decls = infer_program ~raise ~options ~ctx program in
        ( ctx
        , sig_item :: sig_
        , let%bind decl = decl
          and decls = decls in
          return (decl :: decls) ))


let type_program ~raise ~options ?env program =
  let ctx = Context.init ?env () in
  let ctx, _sig, program = infer_program ~raise ~options ~ctx program in
  Elaboration.run_program ~ctx ~raise program


let type_declaration ~raise ~options ?env decl =
  let ctx = Context.init ?env () in
  let ctx, _sig_item, decl = infer_declaration ~raise ~options ~ctx decl in
  Elaboration.run_declaration ~ctx ~raise decl


let type_expression ~raise ~options ?env ?tv_opt expr =
  let ctx = Context.init ?env () in
  let ctx, _type_, expr =
    match tv_opt with
    | Some type_ ->
      let ctx, expr = check_expression ~raise ~options ~ctx expr type_ in
      ctx, type_, expr
    | None ->
      let ctx, _type, expr = infer_expression ~raise ~options ~ctx expr in
      ctx, _type, expr
  in
  Elaboration.run_expr ~ctx ~raise expr
