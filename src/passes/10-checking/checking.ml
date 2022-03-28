open Simple_utils.Trace
module Errors=Errors
open Errors

module I = Ast_core
module O = Ast_typed
open O.Combinators

module Pair = Simple_utils.Pair
module Typing_context = Context.Typing
module App_context = Context.App
type typing_context = Typing_context.t
type context = Context.t

let untype_expression = Untyper.untype_expression
let untype_program = Untyper.untype_program
let assert_type_expression_eq = Helpers.assert_type_expression_eq

let rec type_module_expr ~raise ~init_context ~options : I.module_expr -> typing_context * O.module_expr = fun m_expr ->
  let return x =
    let ret = Location.wrap ~loc:m_expr.location x in
    let ctxt = Typing_context.context_of_module_expr ~outer_context:init_context ret in
    ctxt, ret
  in
  let is_bound ctxt v =
    trace_assert_option ~raise (unbound_module_variable v (I.ModuleVar.get_location v)) (Typing_context.get_module ctxt v)
  in
  match m_expr.wrap_content with
  | I.M_struct prg ->
    let prg = type_module ~init_context ~raise ~options prg in
    return (O.M_struct prg)
  | I.M_module_path path ->
    let _ctxt : typing_context = List.fold
      ~f:(fun acc v -> trace_option ~raise (unbound_module_variable v (I.ModuleVar.get_location v)) (Typing_context.get_module acc v))
      ~init:init_context
      (List.Ne.to_list path)
    in
    return (O.M_module_path path)
  | I.M_variable v ->
    let () = is_bound init_context v in
    return (O.M_variable v)

and type_module ~raise ~options ~init_context (p:I.module_) : O.module_ =
  let aux (c, acc:(typing_context * O.declaration list)) (d:I.declaration) =
    let (c, d') = type_declaration' ~raise ~options c d in
    (c, d' :: acc)
  in
  (* This context use all the declaration so you can use private declaration to type the module. It should not be returned*)
  let (_c, lst) =
      List.fold ~f:aux ~init:(init_context, []) p in
  List.rev lst

and type_declaration' : raise: typer_error raise -> options: Compiler_options.middle_end -> typing_context -> I.declaration -> typing_context * O.declaration =
fun ~raise ~options c d ->
let loc = d.location in
let return ?(loc = loc) c (d : O.declaration_content) = c,Location.wrap ~loc d in
match Location.unwrap d with
  | Declaration_type {type_binder ; _} when Ast_core.TypeVar.is_generalizable type_binder ->
    raise.raise (wrong_generalizable d.location type_binder)
  | Declaration_type {type_binder ; type_expr; type_attr={public} } -> (
    let tv = evaluate_type ~raise c type_expr in
    let tv = {tv with orig_var = Some type_binder} in
    let env' = Typing_context.add_type c type_binder tv in
    return env' @@ Declaration_type { type_binder ; type_expr = tv; type_attr={public} }
  )
  | Declaration_constant { binder = { ascr = None ; var ; attributes } ; attr  ; expr} -> (
    let av, expr = Ast_core.Combinators.get_type_abstractions expr in
    let c = List.fold_right av ~f:(fun v c -> Typing_context.add_type_var c v ()) ~init:c in
    let expr =
      trace ~raise (constant_declaration_tracer loc var expr None) @@
      type_expression' ~options (App_context.create None, c) expr in
    let rec aux t = function
      | [] -> t
      | (abs_var :: abs_vars) -> t_for_all abs_var Type (aux t abs_vars) in
    let type_expression = aux expr.type_expression (List.rev av) in
    let expr = { expr with type_expression } in
    let binder : O.expression_variable = var in
    let post_env = Typing_context.add_value c binder expr.type_expression in
    return post_env @@ Declaration_constant { binder = { var ; ascr = None ; attributes } ; expr ; attr }
  )
  | Declaration_constant { binder = { ascr = Some tv ; var ; attributes } ; attr ; expr } ->
    let av, tv = Ast_core.Helpers.destruct_for_alls tv in
    let av', expr = Ast_core.Combinators.get_type_abstractions expr in
    let av = av @ av' in
    let env = List.fold_right av ~f:(fun v c -> Typing_context.add_type_var c v ()) ~init:c in
    let tv = evaluate_type ~raise env tv in
    let expr =
      trace ~raise (constant_declaration_tracer loc var expr (Some tv)) @@
      type_expression' ~options ~tv_opt:tv (App_context.create @@ Some tv, env) expr in
    let rec aux t = function
      | [] -> t
      | (abs_var :: abs_vars) -> t_for_all abs_var Type (aux t abs_vars) in
    let type_expression = aux expr.type_expression (List.rev av) in
    let expr = { expr with type_expression } in
    let c = Typing_context.add_value c var expr.type_expression in
    return c @@ Declaration_constant { binder = { ascr = Some tv ; var ; attributes } ; expr ; attr }
  | Declaration_module { module_binder ; module_ ; module_attr = {public} } -> (
    let module_ctxt, module_ = type_module_expr ~raise ~init_context:c ~options module_ in
    let post_env = Typing_context.add_module c module_binder module_ctxt in
    return post_env @@ Declaration_module { module_binder; module_; module_attr = {public}}
  )

and evaluate_otype ~raise (c:typing_context) (t:O.type_expression) : O.type_expression =
  (* NOTE: this is similar to evaluate_type, but just look up for variables in environemnt
    feels wrong, but that's to allow re-evaluate body of T_abstractions *)
  let return tv' = make_t ~loc:t.location tv' t.type_meta in
  match t.type_content with
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:(evaluate_otype ~raise c) parameters in
    return (T_constant { language; injection; parameters })
  | T_arrow {type1;type2} ->
      let type1 = evaluate_otype ~raise c type1 in
      let type2 = evaluate_otype ~raise c type2 in
      return (T_arrow {type1;type2})
  | T_sum m -> (
    let lmap =
      let aux ({associated_type;michelson_annotation;decl_pos} : O.row_element) =
        let associated_type = evaluate_otype ~raise c associated_type in
        ({associated_type;michelson_annotation;decl_pos} : O.row_element)
      in
      O.LMap.map aux m.content
    in
    let sum : O.rows  = match Typing_context.get_sum lmap c with
      | None ->
        let layout = m.layout in
        {content = lmap; layout}
      | Some r -> r
    in
    let ty = make_t (T_sum sum) None in
    let () =
      let aux k _v acc = match Typing_context.get_constructor k c with
          | Some (_,type_) ->
            if Ast_typed.Misc.type_expression_eq (acc,type_) then type_
            else if I.LMap.mem (Label "M_left") m.content || I.LMap.mem (Label "M_right") m.content then type_
            else raise.raise (redundant_constructor k t.location)
          | None -> acc in
      let _ = O.LMap.fold aux m.content ty in ()
    in
    return @@ T_sum sum
  )
  | T_record m -> (
    let aux ({associated_type;michelson_annotation;decl_pos}: O.row_element) =
      let associated_type = evaluate_otype ~raise c associated_type in
      ({associated_type;michelson_annotation;decl_pos} : O.row_element)
    in
    let lmap = O.LMap.map aux m.content in
    let record : O.rows = match Typing_context.get_record lmap c with
    | None ->
      let layout = m.layout in
      {content=lmap;layout}
    | Some (_,r) ->  r
    in
    return @@ T_record record
  )
  | T_variable name -> (
    match Typing_context.get_type c name with
    | Some x -> x
    | None -> raise.raise (unbound_type_variable name t.location)
  )
  | T_module_accessor {module_path; element} -> (
    let f = fun acc el -> trace_option ~raise (unbound_module_variable el t.location) (Typing_context.get_module acc el) in
    let module_ = List.fold ~init:c ~f module_path in
    trace_option ~raise (unbound_type_variable element t.location) (Typing_context.get_type module_ element)
  )
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x ->
    let c = Typing_context.add_kind c x.ty_binder () in
    let type_ = evaluate_otype ~raise c x.type_ in
    return (T_abstraction {x with type_})
  | T_for_all x ->
    let c = Typing_context.add_type_var c x.ty_binder () in
    let type_ = evaluate_otype ~raise c x.type_ in
    return (T_for_all {x with type_})

and evaluate_type ~raise (c:typing_context) (t:I.type_expression) : O.type_expression =
  let return tv' = make_t ~loc:t.location tv' (Some t) in
  match t.type_content with
  | T_arrow {type1;type2} ->
      let type1 = evaluate_type ~raise c type1 in
      let type2 = evaluate_type ~raise c type2 in
      return (T_arrow {type1;type2})
  | T_sum m -> (
    let lmap =
      let aux ({associated_type;michelson_annotation;decl_pos} : I.row_element) =
        let associated_type = evaluate_type ~raise c associated_type in
        ({associated_type;michelson_annotation;decl_pos} : O.row_element)
      in
      O.LMap.map aux m.fields
    in
    let sum : O.rows  = match Typing_context.get_sum lmap c with
      | None ->
        let layout = Option.value ~default:default_layout m.layout in
        {content = lmap; layout}
      | Some r -> r
    in
    let ty = make_t (T_sum sum) None in
    let () =
      let aux k _v acc = match Typing_context.get_constructor k c with
          | Some (_,type_) ->
            if Ast_typed.Misc.type_expression_eq (acc,type_) then type_
            else if I.LMap.mem (Label "M_left") m.fields || I.LMap.mem (Label "M_right") m.fields then type_
            else raise.raise (redundant_constructor k t.location)
          | None -> acc in
      let _ = O.LMap.fold aux m.fields ty in ()
    in
    return @@ T_sum sum
  )
  | T_record m -> (
    let aux ({associated_type;michelson_annotation;decl_pos}: I.row_element) =
      let associated_type = evaluate_type ~raise c associated_type in
      ({associated_type;michelson_annotation;decl_pos} : O.row_element)
    in
    let lmap = O.LMap.map aux m.fields in
    let record : O.rows = match Typing_context.get_record lmap c with
    | None ->
      let layout = Option.value ~default:default_layout m.layout in
      {content=lmap;layout}
    | Some (_,r) ->  r
    in
    return @@ T_record record
  )
  | T_variable name -> (
    match Typing_context.get_type c name with
    | Some x -> x
    | None when I.TypeVar.is_generalizable name ->
       (* Case happening when trying to use a variable that is not in
          the context, but it is generalizable: we hint the user
          that the variable could be put in the extended context
          via an annotation *)
       raise.raise (not_annotated t.location)
    | None -> raise.raise (unbound_type_variable name t.location)
  )
  | T_app {type_operator;arguments} -> (
    let operator = trace_option ~raise (unbound_type_variable type_operator t.location) @@
      Typing_context.get_type c type_operator
    in
    let is_fully_applied location (t:O.type_expression) =
      match t.type_content with
      | T_abstraction x ->
        let rec aux : (O.type_expression * int) -> (O.type_expression * int) =
          fun (t,i) -> match t.type_content with T_abstraction x -> aux (x.type_,i+1) | _ -> (t,i)
        in
        let expected = snd @@ aux (x.type_,1) in
        raise.raise (type_constant_wrong_number_of_arguments None expected 0 location)
      | _ -> ()
    in
    let rec aux : O.type_expression * I.type_variable list -> O.type_expression * I.type_variable list =
      fun (op, lst) ->
        match op.type_content with
        | O.T_abstraction {ty_binder;kind=_;type_} -> (
          aux (type_ , lst @ [ty_binder])
        )
        | _ -> (op, lst)
    in
    let (ty_body,vars) = aux (operator, []) in
    let vargs =
      match List.zip vars arguments with
      | Unequal_lengths ->
        let actual = List.length arguments in
        let expected = List.length vars in
        raise.raise (type_constant_wrong_number_of_arguments (Some type_operator) expected actual t.location)
      | Ok x -> x
    in
    let aux : typing_context -> (I.type_variable * I.type_expression) -> typing_context =
      fun c (ty_binder,arg) ->
        let arg' = evaluate_type ~raise c arg in
        let () = is_fully_applied arg.location arg' in
        Typing_context.add_type c ty_binder arg'
    in
    let env' = List.fold_left ~f:aux ~init:c vargs in
    match ty_body.type_content with
    | T_constant {language;injection;parameters} ->
      let aux : O.type_expression -> O.type_expression =
        fun t ->
          let t' = evaluate_otype ~raise env' t in
          let () = is_fully_applied t.location t' in
          t'
      in
      let args = List.map ~f:aux parameters in
      return (T_constant {language;injection;parameters=args})
    | _ -> evaluate_otype ~raise env' ty_body
  )
  | T_module_accessor {module_path; element} -> (
    let f = fun acc el -> trace_option ~raise (unbound_module_variable el t.location) (Typing_context.get_module acc el) in
    let module_ = List.fold ~init:c ~f module_path in
    trace_option ~raise (unbound_type_variable element t.location) (Typing_context.get_type module_ element)
  )
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x ->
    let c = Typing_context.add_kind c x.ty_binder () in
    let type_ = evaluate_type ~raise c x.type_ in
    return (T_abstraction {x with type_})
  | T_for_all x ->
    let c = Typing_context.add_type_var c x.ty_binder () in
    let type_ = evaluate_type ~raise c x.type_ in
    return (T_for_all {x with type_})

and type_expression ~raise ~options : ?env:Environment.t -> ?tv_opt:O.type_expression -> I.expression -> O.expression
  = fun ?env ?tv_opt e ->
    let c   = Typing_context.init ?env () in
    let res = type_expression' ~raise ~options (App_context.create tv_opt, c) ?tv_opt e in
    res

and infer_t_insts ~raise ~loc app_context ( (tc,t) : O.expression_content * O.type_expression )  =
  match t with
  | { type_content = T_for_all _ ; type_meta = _; orig_var=_ ; location=_} ->
    (* TODO: This is some inference, and we should reconcile it with the inference pass. *)
    let avs, type_ = O.Helpers.destruct_for_alls t in
    let last = App_context.get_expect app_context in
    let args = match App_context.pop app_context with | None -> [] | Some args -> args in
    let table = Inference.infer_type_applications ~raise ~loc avs type_ args last in
    let lamb = make_e ~location:loc tc t in
    let x = Inference.build_type_insts ~raise ~loc lamb table avs in
    x.expression_content , x.type_expression
  | _ -> tc, t

and type_expression' ~raise ~options : context -> ?tv_opt:O.type_expression -> I.expression -> O.expression = fun (app_context, context) ?tv_opt e ->
  let return expr tv =
    let () =
      match tv_opt with
      | None -> ()
      | Some tv' -> assert_type_expression_eq ~raise e.location (tv' , tv) in
    let location = e.location in
    make_e ~location expr tv in
  let protocol_version = options.protocol_version in
  let return_e (expr : O.expression) = return expr.expression_content expr.type_expression in
  trace ~raise (expression_tracer e) @@
  fun ~raise -> match e.expression_content with
  (* Basic *)
  | E_variable name -> (
    let tv' = trace_option ~raise (unbound_variable name e.location) @@ Typing_context.get_value context name in
    let tc , tv = infer_t_insts ~raise ~loc:e.location app_context (E_variable name, tv') in
    return tc tv
  )
  | E_literal Literal_unit ->
      return (E_literal (Literal_unit)) (t_unit ())
  | E_literal (Literal_string s) ->
      return (E_literal (Literal_string s)) (t_string ())
  | E_literal (Literal_key s) ->
      return (E_literal (Literal_key s)) (t_key ())
  | E_literal (Literal_key_hash s) ->
      return (E_literal (Literal_key_hash s)) (t_key_hash ())
  | E_literal (Literal_chain_id s) ->
      return (E_literal (Literal_chain_id s)) (t_chain_id ())
  | E_literal (Literal_signature s) ->
      return (E_literal (Literal_signature s)) (t_signature ())
  | E_literal (Literal_bytes s) ->
      return (E_literal (Literal_bytes s)) (t_bytes ())
  | E_literal (Literal_int n) ->
      return (E_literal (Literal_int n)) (t_int ())
  | E_literal (Literal_nat n) ->
      return (E_literal (Literal_nat n)) (t_nat ())
  | E_literal (Literal_timestamp n) ->
      return (E_literal (Literal_timestamp n)) (t_timestamp ())
  | E_literal (Literal_mutez n) ->
      return (E_literal (Literal_mutez n)) (t_mutez ())
  | E_literal (Literal_address s) ->
      return (e_address s) (t_address ())
  | E_literal (Literal_operation op) ->
      return (e_operation op) (t_operation ())
  | E_literal (Literal_bls12_381_g1 b) ->
      return (e_bls12_381_g1 b) (t_bls12_381_g1 ())
  | E_literal (Literal_bls12_381_g2 b) ->
      return (e_bls12_381_g2 b) (t_bls12_381_g2 ())
  | E_literal (Literal_bls12_381_fr b) ->
      return (e_bls12_381_fr b) (t_bls12_381_fr ())
  | E_literal (Literal_chest _ | Literal_chest_key _) -> failwith "chest / chest_key not allowed in the syntax (only tests need this type)"
  | E_record_accessor {record;path} ->
      let e' = type_expression' ~raise ~options (app_context, context) record in
      let aux (prev:O.expression) (a:I.label) : O.expression =
          let property = a in
          let r_tv = trace_option ~raise (expected_record e.location @@ get_type prev) @@
            get_t_record prev.type_expression in
          let tv =
            trace_option ~raise (bad_record_access property prev e.location) @@
            O.LMap.find_opt property r_tv.content in
          let location = e.location in
          make_e ~location (E_record_accessor {record=prev; path=property}) tv.associated_type
      in
      let e = aux e' path in
      (* check type annotation of the final accessed element *)
      return_e e
  | E_constructor {constructor = Label s as constructor ; element} when String.equal s "M_left" || String.equal s "M_right" -> (
    let t = trace_option ~raise (michelson_or_no_annotation constructor e.location) @@ tv_opt in
    let expr' = type_expression' ~raise ~options (app_context, context) element in
    ( match t.type_content with
      | T_sum c ->
        let {associated_type ; _} : O.row_element = O.LMap.find (Label s) c.content in
        let () = assert_type_expression_eq ~raise expr'.location (associated_type, expr'.type_expression) in
        return (E_constructor {constructor = Label s; element=expr'}) t
      | _ -> raise.raise (michelson_or_no_annotation constructor e.location)
    )
  )
  | E_constructor {constructor; element} ->
      let (avs, c_tv, sum_tv) = trace_option ~raise (unbound_constructor constructor e.location) @@
        Typing_context.get_constructor_parametric constructor context in
      let expr' = type_expression' ~raise ~options (app_context, context) element in
      let table = Inference.infer_type_application ~raise ~loc:element.location avs Inference.TMap.empty c_tv expr'.type_expression in
      let table = match tv_opt with
        | Some tv_opt -> Inference.infer_type_application ~raise ~loc:e.location ~default_error:(fun loc t t' -> assert_equal loc t' t) avs table sum_tv tv_opt
        | None -> table in
      let () = trace_option ~raise (not_annotated e.location) @@
                 if (List.for_all avs ~f:(fun v -> O.Helpers.TMap.mem v table)) then Some () else None in
      let c_tv = Ast_typed.Helpers.psubst_type table c_tv in
      let sum_tv = Ast_typed.Helpers.psubst_type table sum_tv in
      let () = assert_type_expression_eq ~raise expr'.location (c_tv, expr'.type_expression) in
      return (E_constructor {constructor; element=expr'}) sum_tv
  (* Record *)
  | E_record m ->
      let m' = O.LMap.map (type_expression' ~raise ~options (app_context, context)) m in
      let _,lmap = O.LMap.fold_map ~f:(
        fun (Label k) e i ->
          let decl_pos = match int_of_string_opt k with Some i -> i | None -> i in
          i+1,({associated_type = get_type e ; michelson_annotation = None ; decl_pos}: O.row_element)
        ) m' ~init:0 in
      let record_type = match Typing_context.get_record lmap context with
        | None -> t_record ~layout:default_layout lmap
        | Some (orig_var,r) -> make_t_orig_var (T_record r) None orig_var
      in
      return (E_record m') record_type
  | E_record_update {record; path; update} ->
    let record = type_expression' ~raise ~options (app_context, context) record in
    let update = type_expression' ~raise ~options (app_context, context) update in
    let wrapped = get_type record in
    let tv =
      match wrapped.type_content with
      | T_record {content;_} -> (
          let O.{associated_type;_} = trace_option ~raise (bad_record_access path record update.location) @@
            O.LMap.find_opt path content in
          associated_type
      )
      | _ -> failwith (Format.asprintf "Update an expression which is not a record %a" O.PP.type_expression wrapped)
    in
    let () = assert_type_expression_eq ~raise update.location (tv, get_type update) in
    return (E_record_update {record; path; update}) wrapped
  (* Data-structure *)
  | E_lambda lambda ->
     let (lambda,lambda_type) = type_lambda ~raise ~options ~loc:e.location ~tv_opt (app_context, context) lambda in
     return (E_lambda lambda ) lambda_type
  | I.E_type_abstraction {type_binder;result} ->
    let context = Typing_context.add_type_var context type_binder () in
    let result  = type_expression' ~raise ~options ?tv_opt (app_context, context) result in
    return (E_type_abstraction {type_binder;result}) result.type_expression
  | E_constant {cons_name=( C_LIST_FOLD | C_MAP_FOLD | C_SET_FOLD | C_FOLD) as opname ;
                arguments=[
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None;attributes=_};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ ; sugar=_}) as _lambda ;
                    collect ;
                    init_record ;
                  ]} ->
      let open Stage_common.Constant in
      (* this special case is here to force annotation of the untyped lambda
         generated by pascaligo's for_collect loop *)
      let (v_col , v_initr ) = Pair.map ~f:(type_expression' ~raise ~options (app_context, context)) (collect , init_record ) in
      let tv_col = get_type v_col   in (* this is the type of the collection  *)
      let tv_out = get_type v_initr in (* this is the output type of the lambda*)
      let input_type = match tv_col.type_content with
        | O.T_constant {language=_ ; injection = (List | Set); parameters=[t]} ->
          make_t_ez_record (("0",tv_out)::[("1",t)])
        | O.T_constant {language=_ ; injection = (Map | Big_map) ; parameters=[k;v]} ->
          make_t_ez_record (("0",tv_out)::[("1",make_t_ez_record [("0",k);("1",v)])])
        | _ -> raise.raise @@ bad_collect_loop tv_col e.location in
      let e' = Typing_context.add_value context lname input_type in
      let body = type_expression' ~raise ~options ?tv_opt:(Some tv_out) (app_context, e') result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_arrow input_type output_type ()) in
      let lst' = [lambda'; v_col; v_initr] in
      let tv_lst = List.map ~f:get_type lst' in
      let (opname', tv) =
        type_constant ~raise ~options opname e.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name= C_LOOP_LEFT as opname;
                arguments = [
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None ; attributes=_};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ ; sugar=_}) as _lambda ;
                    init_record ;
                ]} ->
      let v_initr = type_expression' ~raise ~options (app_context, context) init_record in
      let tv_out = get_type v_initr in
      let input_type  = tv_out in
      let context = Typing_context.add_value context lname input_type in
      let body = type_expression' ~raise ~options (app_context, context) result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_arrow input_type output_type ()) in
      let lst' = [lambda';v_initr] in
      let tv_lst = List.map ~f:get_type lst' in
      let (opname',tv) = type_constant ~raise ~options opname e.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_CREATE_CONTRACT as cons_name;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~options (app_context, context)) arguments in
      let () = match lst' with
        | { expression_content = O.E_lambda l ; _ } :: _ ->
          let open Ast_typed.Misc in
          let fvs = Free_variables.lambda [] l in
          if Int.equal (List.length fvs) 0 then ()
          else raise.raise @@ fvs_in_create_contract_lambda e (List.hd_exn fvs)
        | _ -> raise.raise @@ create_contract_lambda S.C_CREATE_CONTRACT e
      in
      let tv_lst = List.map ~f:get_type lst' in
      let (name', tv) =
        type_constant ~raise ~options cons_name e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name=C_SET_ADD|C_CONS as cst;arguments=[key;set]} ->
      let key' =  type_expression' ~raise ~options (app_context, context) key in
      let tv_key = get_type key' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> match cst with
            C_SET_ADD -> t_set tv_key
          | C_CONS -> t_list tv_key
          | _ -> failwith "Only C_SET_ADD and C_CONS are possible because those were the two cases matched above"
      in
      let set' =  type_expression' ~raise ~options (app_context, context) ~tv_opt:tv set in
      let tv_set = get_type set' in
      let tv_lst = [tv_key;tv_set] in
      let (name', tv) = type_constant ~raise ~options cst e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';set']}) tv
  | E_constant {cons_name=C_MAP_ADD as cst; arguments=[key;value;map]} ->
      let key' = type_expression' ~raise ~options (app_context, context) key in
      let val' = type_expression' ~raise ~options (app_context, context) value in
      let tv_key = get_type key' in
      let tv_val = get_type val' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> t_map_or_big_map tv_key tv_val
      in
      let map' =  type_expression' ~raise ~options (app_context, context) ~tv_opt:tv map in
      let tv_map = get_type map' in
      let tv_lst = [tv_key;tv_val;tv_map] in
      let (name', tv) = type_constant ~raise ~options cst e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';val';map']}) tv
  | E_constant {cons_name = C_POLYMORPHIC_ADD;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~options (app_context, context)) arguments in
      let tv_lst = List.map ~f:get_type lst' in
      let decide = function
        | {O.expression_content = E_literal (Literal_string _); _ } -> Some S.C_CONCAT
        | {expression_content = E_constant {cons_name = C_ADD; _ }; _ } -> Some C_ADD
        | {expression_content = E_constant {cons_name = C_CONCAT; _ }; _ } -> Some C_CONCAT
        | {expression_content = E_constant {cons_name = C_SLICE; _ }; _ } -> Some C_CONCAT
        | {expression_content = E_literal (Literal_int _); _ } -> Some C_ADD
        | {expression_content = E_record_accessor {record; path}; _ } ->
            (let x = get_record_field_type record.type_expression path in
            match x with
            Some s when is_t_string s ->
              Some C_CONCAT
            | _ -> None )
        | {expression_content = E_variable _; type_expression = texpr ; location = _} ->
            if is_t_string texpr then
              Some C_CONCAT
            else
              None
        | _ -> None in
      let cst =
        Option.value ~default:S.C_ADD @@ List.find_map lst' ~f:decide in
      let (name', tv) =
        type_constant ~raise ~options cst e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name = C_POLYMORPHIC_SUB;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~options (app_context, context)) arguments in
      let tv_lst = List.map ~f:get_type lst' in
      let decide = function
        | Environment.Protocols.Ithaca, O.{ type_expression ; _ } when is_t_mutez type_expression ->
          Some S.C_SUB_MUTEZ
        | _ -> None in
      let cst =
        Option.value ~default:S.C_SUB @@ List.find_map lst' ~f:(fun e -> decide (protocol_version, e)) in
      let (name', tv) =
        type_constant ~raise ~options cst e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~options (app_context, context)) arguments in
      let tv_lst = List.map ~f:get_type lst' in
      let (name', tv) =
        type_constant ~raise ~options cons_name e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_application { lamb = ilamb ; args=_} ->
     (* TODO: This currently does not handle constraints (as those in inference). *)
     (* Get lambda and applications: (..((lamb arg1) arg2) ...) argk) *)
     let lamb, args = I.Helpers.destruct_applications e in
     (* Type-check all the involved subexpressions *)
     let args = List.map ~f:(type_expression' ~raise ~options (app_context, context)) args in
     let args_ty = List.map ~f:(fun v -> v.type_expression) args in
     let lamb = type_expression' ~raise ~options (App_context.push tv_opt args_ty app_context, context) lamb in
     (* Arguments are checked *)
     let _ : _ Inference.TMap.t = Inference.infer_type_applications ~raise ~loc:e.location [] lamb.type_expression args_ty tv_opt in
     (* Re-build term (i.e. re-add applications) *)
     let app = trace_option ~raise (should_be_a_function_type lamb.type_expression ilamb) @@
                 O.Helpers.build_applications_opt lamb args in
     return_e app
  (* Advanced *)
  | E_matching {matchee;cases} -> (
    let matchee' = type_expression' ~raise ~options (app_context, context) matchee in
    let aux : (I.expression, I.type_expression) I.match_case -> ((I.type_expression I.pattern * O.type_expression) list * (I.expression * typing_context)) =
      fun {pattern ; body} -> ([(pattern,matchee'.type_expression)], (body,context))
    in
    let eqs = List.map ~f:aux cases in
    let aux = fun ~raise context ?tv_opt i -> type_expression' ~raise ~options (App_context.create None, context) ?tv_opt i in
    match matchee.expression_content with
    | E_variable matcheevar ->
      let case_exp = Pattern_matching.compile_matching ~raise ~err_loc:e.location ~type_f:aux ~body_t:(tv_opt) matcheevar eqs in
      let case_exp = { case_exp with location = e.location } in
      return case_exp.expression_content case_exp.type_expression
    | _ ->
      let matcheevar = I.ValueVar.fresh () in
      let case_exp = Pattern_matching.compile_matching ~raise ~err_loc:e.location ~type_f:aux ~body_t:(tv_opt) matcheevar eqs in
      let case_exp = { case_exp with location = e.location } in
      let x = O.E_let_in { let_binder = matcheevar ; rhs = matchee' ; let_result = case_exp ; attr = {inline = false; no_mutation = false; public = true ; view= false } } in
      return x case_exp.type_expression
  )
  | E_let_in {let_binder = {var ; ascr = None ; attributes=_} ; rhs ; let_result; attr } ->
     let av, rhs = Ast_core.Combinators.get_type_abstractions rhs in
     let context = List.fold_right av ~f:(fun v c -> Typing_context.add_type_var c v ()) ~init:context in
     let rhs = type_expression' ~raise ~options (app_context, context) rhs in
     let binder = var in
     let rec aux t = function
       | [] -> t
       | (abs_var :: abs_vars) -> t_for_all abs_var Type (aux t abs_vars) in
     let type_expression = aux rhs.type_expression (List.rev av) in
     let rhs = { rhs with type_expression } in
     let e' = Typing_context.add_value context binder rhs.type_expression in
     let let_result = type_expression' ~raise ~options ?tv_opt (app_context, e') let_result in
     return (E_let_in {let_binder = binder; rhs; let_result; attr }) let_result.type_expression
  | E_let_in {let_binder = {var ; ascr = Some tv ; attributes=_} ; rhs ; let_result; attr } ->
    let av, tv = Ast_core.Helpers.destruct_for_alls tv in
    let av', rhs = Ast_core.Combinators.get_type_abstractions rhs in
    let av = av @ av' in
    let pre_context = context in
    let context = List.fold_right av ~f:(fun v c -> Typing_context.add_type_var c v ()) ~init:context in
    let tv = evaluate_type ~raise context tv in
    let rhs = type_expression' ~raise ~options ~tv_opt:tv (app_context, context) rhs in
    let rec aux t = function
      | [] -> t
      | (abs_var :: abs_vars) -> t_for_all abs_var Type (aux t abs_vars) in
    let type_expression = aux rhs.type_expression (List.rev av) in
    let rhs = { rhs with type_expression } in
    let binder  = var in
    let context = Typing_context.add_value pre_context binder type_expression in
    let let_result = type_expression' ~raise ~options ?tv_opt (app_context, context) let_result in
    return (E_let_in {let_binder = binder; rhs; let_result; attr }) let_result.type_expression
  | E_type_in {type_binder; _} when Ast_core.TypeVar.is_generalizable type_binder ->
    raise.raise (wrong_generalizable e.location type_binder)
  | E_type_in {type_binder; rhs ; let_result} ->
    let rhs = evaluate_type ~raise context rhs in
    let e' = Typing_context.add_type context type_binder rhs in
    let let_result = type_expression' ~raise ~options (app_context, e') let_result in
    return (E_type_in {type_binder; rhs; let_result}) let_result.type_expression
  | E_mod_in {module_binder; rhs; let_result} ->
    let rhs_ctxt,rhs = type_module_expr ~raise ~options ~init_context:context rhs in
    let e' = Typing_context.add_module context module_binder rhs_ctxt in
    let let_result = type_expression' ~raise ~options ?tv_opt (app_context, e') let_result in
    return (E_mod_in {module_binder; rhs; let_result}) let_result.type_expression
  | E_raw_code {language;code} ->
    let (code,type_expression) = trace_option ~raise (expected_ascription code) @@
      I.get_e_ascription code.expression_content in
    let code = type_expression' ~raise ~options (app_context, context) code in
    let type_expression = evaluate_type ~raise context type_expression in
    let code = {code with type_expression} in
    return (E_raw_code {language;code}) code.type_expression
  | E_recursive {fun_name; fun_type; lambda} ->
    let type_env = Typing_context.get_type_vars context in
    let av = Ast_core.Helpers.Free_type_variables.type_expression type_env fun_type in
    let fun_type = evaluate_type ~raise context fun_type in
    let e' = Typing_context.add_value context fun_name fun_type in
    let e' = List.fold_left av ~init:e' ~f:(fun e v -> Typing_context.add_type_var e v ()) in
    let (lambda,lambda_type) = type_lambda ~raise ~loc:e.location ~options ~tv_opt (app_context, e') lambda in
    let () = assert_type_expression_eq ~raise fun_type.location (fun_type,lambda_type) in
    return (E_recursive {fun_name;fun_type;lambda}) fun_type
  | E_ascription {anno_expr; type_annotation} ->
    let tv = evaluate_type ~raise context type_annotation in
    let app_context = App_context.update_expect (Some tv) app_context in
    let expr' = type_expression' ~raise ~options ~tv_opt:tv (app_context, context) anno_expr in
    return_e expr'
  | E_module_accessor {module_path; element} -> (
    let f = fun acc el -> trace_option ~raise (unbound_module_variable el (I.ModuleVar.get_location el)) (Typing_context.get_module acc el) in
    let module_env = List.fold ~init:context ~f module_path in
    let tv' = trace_option ~raise (unbound_variable element e.location) @@ Typing_context.get_value module_env element in
    let tc , tv = infer_t_insts ~raise ~loc:e.location app_context (E_module_accessor {module_path; element}, tv') in
    return tc tv
  )


and type_lambda ~raise ~options ~loc ~tv_opt (ac, e) { binder ; output_type ; result } =
      let top_i_t , top_o_t = match tv_opt with
        | None -> (None,None)
        | Some tv -> (
          match O.get_t_arrow tv with
          | None -> (None,None)
          | Some {type1 ; type2} -> (Some type1 , Some type2)
        )
      in
      let ascr_i_t =
        Option.map ~f:(evaluate_type ~raise e) binder.ascr in
      let ascr_o_t =
        Option.map ~f:(evaluate_type ~raise e) output_type
      in
      let input_type =
        match ascr_i_t , top_i_t with
        | Some t1 , Some t2 ->
          assert_type_expression_eq ~raise loc (t1,t2) ; Some t2
        | _ , Some t -> Some t
        | Some t , _ -> Some t
        | _ -> None
      in
      let output_type =
        match ascr_o_t , top_o_t with
        | Some t1 , Some t2 ->
          assert_type_expression_eq ~raise loc (t1,t2) ; Some t2
        | _ , Some t -> Some t
        | Some t , _ -> Some t
        | _ -> None
      in
      let binder = binder.var in
      let input_type = trace_option ~raise (missing_funarg_annotation binder) input_type in
      let e' = Typing_context.add_value e binder input_type in
      let body = type_expression' ~raise ~options ?tv_opt:output_type (ac, e') result in
      let output_type = body.type_expression in
      (({binder; result=body}:O.lambda),(t_arrow input_type output_type ()))

and type_constant ~raise ~options (name:I.constant') (loc:Location.t) (lst:O.type_expression list) (tv_opt:O.type_expression option) : O.constant' * O.type_expression =
  let typer = Constant_typers.constant_typers ~raise ~options loc name in
  let tv = typer lst tv_opt in
  (name, tv)

let type_program ~raise ~options ?env m = type_module ~raise ~options ~init_context:(Typing_context.init ?env ()) m
let type_declaration ~raise ~options ?env d = snd @@ type_declaration' ~raise ~options (Typing_context.init ?env ()) d
