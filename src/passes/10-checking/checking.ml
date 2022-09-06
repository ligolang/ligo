open Simple_utils.Trace
module Errors=Errors
open Errors

module I = Ast_core
module O = Ast_typed
open O.Combinators

module Pair = Simple_utils.Pair
module Typing_context = Context.Typing
module App_context = Context.App

open Ligo_prim

type typing_context = Typing_context.t
type context = Context.t

let type_value_attr : I.ValueAttr.t -> O.ValueAttr.t =
  fun {inline;no_mutation;view;public;hidden;thunk} -> {inline;no_mutation;view;public;hidden;thunk}
let untype_expression = Untyper.untype_expression
let untype_program = Untyper.untype_program
let assert_type_expression_eq = Helpers.assert_type_expression_eq

(*
  This function operates on the return type of Context.get_sum.
  If type match the constructor label and its argument type, warns user about ambiguous constructor
*)
let warn_ambiguous_constructor ~raise loc (var_chosen,c_arg_t) ignored =
  let ignored_match = List.find
    ~f:(fun (_,_,a,_) ->
      Option.is_some (O.Misc.assert_type_expression_eq (c_arg_t, a))
      )
      ignored
  in
  match ignored_match with
  | Some (var_ignored,_,_,_) ->
    raise.warning (`Checking_ambiguous_constructor (loc,var_chosen,var_ignored));
  | None -> ()

let rec evaluate_type ~raise (c:typing_context) (t:I.type_expression) : O.type_expression =
  let self ?(context=c) = evaluate_type ~raise context in
  let return tv' = make_t ~loc:t.location tv' (Some t) in
  match t.type_content with
  | T_arrow {type1;type2} ->
      let type1 = self type1 in
      let type2 = self type2 in
      return (T_arrow {type1;type2})
  | T_sum m -> (
    let rows =
      let layout = Option.value ~default:default_layout m.layout in
      let aux ({associated_type;michelson_annotation;decl_pos} : I.row_element) =
        let associated_type = self associated_type in
        ({associated_type;michelson_annotation;decl_pos} : O.row_element)
      in
      let fields = Record.map aux m.fields in
      O.{ fields ; layout }
    in
    return @@ T_sum rows
  )
  | T_record m -> (
    let rows =
      let layout = Option.value ~default:default_layout m.layout in
      let aux ({associated_type;michelson_annotation;decl_pos} : I.row_element) =
        let associated_type = self associated_type in
        ({associated_type;michelson_annotation;decl_pos} : O.row_element)
      in
      let fields = Record.map aux m.fields in
      O.{ fields ; layout }
    in
    return @@ T_record rows
  )
  | T_variable name -> (
    match Typing_context.get_type c name with
    | Some x -> x
    | None -> raise.error (unbound_type_variable name t.location)
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
        raise.error (type_app_wrong_arity None expected 0 location)
      | _ -> ()
    in
    let aux : I.type_expression -> O.type_expression =
      fun t ->
        let t' = self t in
        is_fully_applied t.location t' ;
        t'
    in
    let arguments = List.map ~f:aux arguments in
    let (vars , ty_body) = O.Helpers.destruct_type_abstraction operator in
    let vargs =
      match List.zip vars arguments with
      | Unequal_lengths ->
        let actual = List.length arguments in
        let expected = List.length vars in
        raise.error (type_app_wrong_arity (Some type_operator) expected actual t.location)
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
  )
  | T_module_accessor {module_path; element} -> (
    let f = fun acc el -> trace_option ~raise (unbound_module_variable el t.location) (Typing_context.get_module acc el) in
    let module_ = List.fold ~init:c ~f module_path in
    trace_option ~raise (unbound_type_variable element t.location) (Typing_context.get_type module_ element)
  )
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x ->
    let c = Typing_context.add_kind c x.ty_binder () in
    let type_ = self ~context:c x.type_ in
    return (T_abstraction {x with type_})
  | T_for_all x ->
    let c = Typing_context.add_type_var c x.ty_binder () in
    let type_ = self ~context:c x.type_ in
    return (T_for_all {x with type_})

and infer_t_insts ~raise ~options ~loc app_context ( (tc,t) : O.expression_content * O.type_expression )  =
  match t with
  | { type_content = T_for_all _ ; type_meta = _; orig_var=_ ; location=_} -> (
    (* TODO: This is some inference, and we should reconcile it with the inference pass. *)
    let last = App_context.get_expect app_context in
    let args = match App_context.pop app_context with | None -> [] | Some args -> args in
    let avs, type_ = O.Helpers.destruct_for_alls t in
    let _, type_no_arrows = O.Helpers.destruct_arrows type_ in
    match type_no_arrows.type_content with
    | T_constant { injection = External s ; parameters = _ ; _ } when String.is_prefix s ~prefix:"u_" ->
       let args = O.Helpers.destruct_tuples args in
       let t, table, ot = Constant_typers.external_typers ~raise ~options loc s args last in
       let argsv, _ = O.Helpers.destruct_arrows_n t (List.length avs) in
       let z = List.zip_exn avs argsv in
       let table = List.fold_right z ~f:(fun (av, t) table -> Inference.TMap.add av t table) ~init:table in
       let parameters, external_type = O.Helpers.destruct_arrows_n ot (List.length avs) in
       let parameters = O.t_tuple parameters in
       let ot = (t_arrow parameters external_type ()) in
       let lamb = make_e ~location:loc tc ot in
       let x = Inference.build_type_insts_ufunction ~raise ~loc lamb table avs in
       x.expression_content , x.type_expression
    | T_constant { injection = External s ; parameters = _ ; _ } ->
       let t, table, ot = Constant_typers.external_typers ~raise ~options loc s args last in
       let argsv, _ = O.Helpers.destruct_arrows_n t (List.length avs) in
       let z = List.zip_exn avs argsv in
       let table = List.fold_right z ~f:(fun (av, t) table -> Inference.TMap.add av t table) ~init:table in
       let lamb = make_e ~location:loc tc ot in
       let x = Inference.build_type_insts_function ~raise ~loc lamb table avs in
       x.expression_content , t
    | _ ->
       let table = Inference.infer_type_applications ~raise ~loc avs type_ args last in
       let lamb = make_e ~location:loc tc t in
       let x = Inference.build_type_insts ~raise ~loc lamb table avs in
       x.expression_content , x.type_expression
  )
  | _ -> tc, t

and type_expression ~raise ~options : context -> ?tv_opt:O.type_expression -> I.expression -> O.expression = fun (app_context, context) ?tv_opt e ->
  Context.Hashes.set_context context ;
  let self ?(raise=raise) ?(context = (app_context, context)) ?tv_opt e = type_expression ~raise ~options context ?tv_opt e in
  let return expr tv =
    let () =
      match tv_opt with
      | None -> ()
      | Some tv' -> assert_type_expression_eq ~raise e.location (tv' , tv) in
    let location = e.location in
    make_e ~location expr tv in
  let return_e (expr : O.expression) = return expr.expression_content expr.type_expression in
  trace ~raise (expression_tracer e) @@
  fun ~raise -> match e.expression_content with
  (* Basic *)
  | E_variable name -> (
    let tv' = trace_option ~raise (unbound_variable name e.location) @@ Typing_context.get_value context name in
    let tc , tv = infer_t_insts ~raise ~options ~loc:e.location app_context (E_variable name, tv') in
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
  | E_constructor {constructor = Label s as constructor ; element} when String.equal s "M_left" || String.equal s "M_right" -> (
    let t = trace_option ~raise (michelson_or_no_annotation constructor e.location) @@ tv_opt in
    let expr' = self element in
    ( match t.type_content with
      | T_sum c ->
        let {associated_type ; _} : O.row_element = Record.LMap.find (Label s) c.fields in
        let () = assert_type_expression_eq ~raise expr'.location (associated_type, expr'.type_expression) in
        return (E_constructor {constructor = Label s; element=expr'}) t
      | _ -> raise.error (michelson_or_no_annotation constructor e.location)
    )
  )
  | E_constructor {constructor; element} -> (
    let destructed_tv_opt =
      let open Simple_utils.Option in
      let* sum_t = tv_opt in
      let* x = O.get_sum_label_type sum_t constructor in
      return (sum_t, x)
    in
    let (avs, c_arg_t, sum_t) =
      match destructed_tv_opt with
      | Some (sum_t,c_tv) -> (
        let avs , _ = O.Helpers.destruct_type_abstraction c_tv in
        (avs,c_tv,sum_t)
      )
      | None -> (
        let matching_t_sum = Context.Typing.get_sum constructor context in
        (match matching_t_sum with
        | (v_ty,tvl,c_arg_t,sum_t) :: ignored  ->
          let () = warn_ambiguous_constructor ~raise e.location (v_ty,c_arg_t) ignored in
          (tvl,c_arg_t,sum_t)
        | [] -> raise.error (unbound_constructor constructor e.location)))
    in
    let c_arg = self element in
    let table = Inference.infer_type_application ~raise ~loc:element.location avs Inference.TMap.empty c_arg_t c_arg.type_expression in
    let () = if Option.is_none tv_opt then trace_option ~raise (not_annotated e.location) @@
      if (List.for_all avs ~f:(fun v -> O.Helpers.TMap.mem v table)) then Some () else None
    in
    let c_t = Ast_typed.Helpers.psubst_type table c_arg_t in
    let sum_t = Ast_typed.Helpers.psubst_type table sum_t in
    let () = assert_type_expression_eq ~raise c_arg.location (c_t, c_arg.type_expression) in
    return (E_constructor {constructor; element=c_arg}) sum_t
  )
  (* Record *)
  | E_record m -> (
    let field_types_opt =
      let open Simple_utils.Option in
      let* rec_t = tv_opt in
      let* x = O.get_record_fields rec_t in
      let* x = match List.zip (List.map ~f:snd x) (Record.LMap.to_list m) with
        | Ok x -> Some x
        | Unequal_lengths -> None
      in
      return x
    in
    let m' = match field_types_opt with
      | None -> Record.map self m
      | Some lst ->
        let lst = List.map ~f:(fun (tv_opt, exp) -> self ~tv_opt exp) lst in
        Record.of_list (List.zip_exn (Record.LMap.keys m) lst)
    in
    let _,lmap = Record.LMap.fold_map ~f:(
      fun (Label k) e i ->
        let decl_pos = match int_of_string_opt k with Some i -> i | None -> i in
        i+1,({associated_type = get_type e ; michelson_annotation = None ; decl_pos}: O.row_element)
      ) m' ~init:0 in
    let record_type = match Typing_context.get_record lmap context with
      | None -> t_record ~layout:default_layout lmap
      | Some (orig_var,r) -> make_t_orig_var (T_record r) None orig_var
    in
    return (E_record m') record_type
  )
  | E_accessor {record;path} ->
      let e' = self record in
      let aux (prev:O.expression) (a:Label.t) : O.expression =
          let property = a in
          let r_tv = trace_option ~raise (expected_record e.location @@ get_type prev) @@
            get_t_record prev.type_expression in
          let tv =
            trace_option ~raise (bad_record_access property prev e.location) @@
            Record.LMap.find_opt property r_tv.fields in
          let location = e.location in
          make_e ~location (E_accessor {record=prev; path=property}) tv.associated_type
      in
      let e = aux e' path in
      (* check type annotation of the final accessed element *)
      return_e e
  | E_update {record; path; update} ->
    let record = self record in
    let update = self update in
    let wrapped = get_type record in
    let tv =
      match wrapped.type_content with
      | T_record {fields;_} -> (
          let {associated_type;_} :  O.row_element = trace_option ~raise (bad_record_access path record update.location) @@
            Record.LMap.find_opt path fields in
          associated_type
      )
      | _ -> failwith (Format.asprintf "Update an expression which is not a record %a" O.PP.type_expression wrapped)
    in
    let () = assert_type_expression_eq ~raise update.location (tv, get_type update) in
    return (E_update {record; path; update}) wrapped
  (* Data-structure *)
  | E_lambda lambda ->
     let (lambda,lambda_type) = type_lambda ~raise ~options ~loc:e.location ~tv_opt (app_context, context) lambda in
     return (E_lambda lambda ) lambda_type
  | E_type_abstraction {type_binder;result} ->
    let context = Typing_context.add_type_var context type_binder () in
    let result  = self ?tv_opt ~context:(app_context, context) result in
    return (E_type_abstraction {type_binder;result}) result.type_expression
  | E_constant {cons_name=( C_LIST_ITER | C_MAP_ITER | C_SET_ITER | C_ITER) as opname ;
                arguments=[
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None;attributes};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ ; sugar=_}) as _lambda ;
                    collect ;
                  ]} ->
      let open Ligo_prim.Constant in
      (* this special case is here to force annotation of the untyped lambda
         generated by pascaligo's for_collect loop *)
      let v_col  = self collect in
      let tv_col = get_type v_col   in (* this is the type of the collection  *)
      let input_type = match tv_col.type_content with
        | O.T_constant {language=_ ; injection = (List | Set); parameters=[t]} -> t
        | O.T_constant {language=_ ; injection = (Map | Big_map) ; parameters=[k;v]} -> make_t_ez_record [("0",k);("1",v)]
        | _ -> raise.error @@ bad_collect_loop tv_col e.location in
      let e' = Typing_context.add_value context lname input_type in
      let body = self ?tv_opt:(Some (t_unit ())) ~context:(app_context,e') result in
      let output_type = body.type_expression in
      let tv_lambda =t_arrow input_type output_type () in
      let lambda' = make_e (E_lambda {binder = {var=lname;ascr=input_type;attributes} ; output_type ; result=body}) tv_lambda in
      let lst' = [lambda'; v_col] in
      let tv_lst = [tv_lambda;tv_col] in
      let (opname', tv) =
        type_constant ~raise ~options opname e.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name= C_LOOP_LEFT as opname;
                arguments = [
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None ; attributes};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ ; sugar=_}) as _lambda ;
                    init_record ;
                ]} ->
      let v_initr = self init_record in
      let tv_out = get_type v_initr in
      let input_type  = tv_out in
      let context = Typing_context.add_value context lname input_type in
      let body = self ~context:(app_context, context) result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = {var=lname;ascr=input_type;attributes} ; output_type ; result=body}) (t_arrow input_type output_type ()) in
      let lst' = [lambda';v_initr] in
      let tv_lst = List.map ~f:get_type lst' in
      let (opname',tv) = type_constant ~raise ~options opname e.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_SET_ADD|C_CONS as cst;arguments=[key;set]} ->
      let key' =  self key in
      let tv_key = get_type key' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> match cst with
            C_SET_ADD -> t_set tv_key
          | C_CONS -> t_list tv_key
          | _ -> failwith "Only C_SET_ADD and C_CONS are possible because those were the two cases matched above"
      in
      let set' =  self ~tv_opt:tv set in
      let tv_set = get_type set' in
      let tv_lst = [tv_key;tv_set] in
      let (name', tv) = type_constant ~raise ~options cst e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';set']}) tv
  | E_constant {cons_name=C_MAP_ADD as cst; arguments=[key;value;map]} ->
      let key' = self key in
      let val' = self value in
      let tv_key = get_type key' in
      let tv_val = get_type val' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> t_map tv_key tv_val
      in
      let map' = try_with (fun ~raise ~catch:_ -> self ~raise ~context:(app_context, context) ~tv_opt:tv map)
               (fun ~catch:_ _ -> let tv = match tv_opt with
                             Some tv -> tv
                           | None -> t_big_map tv_key tv_val
                         in self ~tv_opt:tv map) in
      let tv_map = get_type map' in
      let tv_lst = [tv_key;tv_val;tv_map] in
      let (name', tv) = type_constant ~raise ~options cst e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';val';map']}) tv
  | E_constant {cons_name;arguments} ->
      let lst' = List.map ~f:(self ~context:(app_context, context)) arguments in
      let tv_lst = List.map ~f:get_type lst' in
      let (name', tv) =
        type_constant ~raise ~options cons_name e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_application { lamb = ilamb ; args=_} ->
     (* TODO: This currently does not handle constraints (as those in inference). *)
     (* Get lambda and applications: (..((lamb arg1) arg2) ...) argk) *)
     let lamb, args = I.Helpers.destruct_applications e in
     (* Type-check all the involved subexpressions *)
     let args = List.map ~f:self args in
     let args_ty = List.map ~f:(fun v -> v.type_expression) args in
     let lamb = self ~context:(App_context.push tv_opt args_ty app_context, context) lamb in
     (* Arguments are checked *)
     let _ : _ Inference.TMap.t = Inference.infer_type_applications ~raise ~loc:e.location [] lamb.type_expression args_ty tv_opt in
     (* Re-build term (i.e. re-add applications) *)
     let app = trace_option ~raise (should_be_a_function_type lamb.type_expression ilamb) @@
                 O.Helpers.build_applications_opt lamb args in
     return_e app
  (* Advanced *)
  | E_matching {matchee;cases} -> (
    let matchee' = self ~context:(app_context, context) matchee in
    let cases = List.mapi ~f:(fun i x -> (i,x)) cases in (* index the cases to keep the order in which they are written *)
    let type_cases = fun ~raise (cases : (int * (S.expression, S.type_expression option) Match_expr.match_case) List.Ne.t) ->
      let cases = List.Ne.to_list cases in
      List.fold_map cases ~init:tv_opt
        ~f:(fun tv_opt (i,{pattern;body}) ->
          let context,pattern = type_pattern ~raise pattern matchee'.type_expression context in
          match tv_opt with
          | Some tv_opt ->
            let body = self ~raise ~context:(App_context.create (Some tv_opt), context) ~tv_opt body in
            Some tv_opt, (pattern,matchee'.type_expression,body,i)
          | None ->
            let body = self ~raise ~context:(App_context.create None, context) body in
            Some body.type_expression, (pattern,matchee'.type_expression,body,i)
        )
    in
    let eqs =
      (* here, we try to type-check cases bodies in multiple orders: [ B1 ; B2 ; B3 ] [ B2 ; B3 ; B1 ] [ B3 ; B1 ; B2 ] *)
      let permutations = List.Ne.(head_permute (of_list cases)) in
      let _,infered_eqs = Helpers.first_success ~raise type_cases permutations in
      List.map ~f:(fun (p,p_ty,body,_) -> (p,p_ty,body)) @@ List.sort infered_eqs ~compare:(fun (_,_,_,a) (_,_,_,b) -> Int.compare a b)
    in
    let syntax = options.syntax_for_errors in
    let () = Pattern_anomalies.check_anomalies ~raise ~syntax ~loc:e.location eqs matchee'.type_expression in
    match matchee.expression_content with
    | E_variable matcheevar ->
      let case_exp = Pattern_matching.compile_matching ~raise ~err_loc:e.location matcheevar eqs in
      let case_exp = { case_exp with location = e.location } in
      return case_exp.expression_content case_exp.type_expression
    | _ ->
      let matcheevar = ValueVar.fresh () in
      let case_exp = Pattern_matching.compile_matching ~raise ~err_loc:e.location matcheevar eqs in
      let case_exp = { case_exp with location = e.location } in
      let x = O.E_let_in { let_binder = {var=matcheevar;ascr=matchee'.type_expression;attributes={const_or_var=Some `Var}} ; rhs = matchee' ; let_result = case_exp ; attr = {inline = false; no_mutation = false; public = true ; view= false ; hidden = false ; thunk = false } } in
      return x case_exp.type_expression
  )
  | E_let_in {let_binder = {var ; ascr ; attributes} ; rhs ; let_result; attr } ->
    let av, rhs = Ast_core.Combinators.get_type_abstractions rhs in
    let pre_context = context in
    let context = List.fold av ~f:(fun c v -> Typing_context.add_type_var c v ()) ~init:context in
    let tv = Option.map ~f:(evaluate_type ~raise context) ascr in
    let rhs = self ?tv_opt:tv ~context:(app_context, context) rhs in
    let rhs = Ast_typed.Helpers.build_type_abstractions rhs (List.rev av) in
    let context = Typing_context.add_value pre_context var rhs.type_expression in
    let let_result = self ?tv_opt ~context:(app_context, context) let_result in
    let attr = type_value_attr attr in
    return (E_let_in {let_binder = {var;ascr=rhs.type_expression;attributes}; rhs; let_result; attr }) let_result.type_expression
  | E_type_in {type_binder; rhs ; let_result} ->
    let rhs = evaluate_type ~raise context rhs in
    let e' = Typing_context.add_type context type_binder rhs in
    let let_result = self ~context:(app_context, e') let_result in
    return let_result.expression_content let_result.type_expression
  | E_mod_in {module_binder; rhs; let_result} ->
    let rhs_ctxt,rhs = type_module_expr ~raise ~options ~init_context:context rhs in
    let e' = Typing_context.add_module context module_binder rhs_ctxt in
    let let_result = self ?tv_opt ~context:(app_context, e') let_result in
    return (E_mod_in {module_binder; rhs; let_result}) let_result.type_expression
  | E_raw_code {language;code} ->
    let (code,type_expression) = trace_option ~raise (not_annotated code.location) @@
      I.get_e_ascription code.expression_content in
    let code = self code in
    let type_expression = evaluate_type ~raise context type_expression in
    let code = {code with type_expression} in
    return (E_raw_code {language;code}) code.type_expression
  | E_recursive {fun_name; fun_type; lambda} ->
    let type_env = Typing_context.get_type_vars context in
    let av = Ast_core.Helpers.Free_type_variables.type_expression type_env fun_type in
    let fun_type = evaluate_type ~raise context fun_type in
    let e' = Typing_context.add_value context fun_name fun_type in
    let e' = List.fold_left av ~init:e' ~f:(fun e v -> Typing_context.add_type_var e v ()) in
    let lambda = Lambda.map Fun.id Option.return lambda in
    let (lambda,lambda_type) = type_lambda ~raise ~loc:e.location ~options ~tv_opt (app_context, e') lambda in
    let () = assert_type_expression_eq ~raise fun_type.location (fun_type,lambda_type) in
    return (E_recursive {fun_name;fun_type;lambda}) fun_type
  | E_ascription {anno_expr; type_annotation} ->
    let tv = evaluate_type ~raise context type_annotation in
    let app_context = App_context.update_expect (Some tv) app_context in
    let expr' = self ~tv_opt:tv ~context:(app_context, context) anno_expr in
    return_e expr'
  | E_module_accessor {module_path; element} -> (
    let f = fun acc el -> trace_option ~raise (unbound_module_variable el (ModuleVar.get_location el)) (Typing_context.get_module acc el) in
    let module_env = List.fold ~init:context ~f module_path in
    let tv' = trace_option ~raise (unbound_variable element e.location) @@ Typing_context.get_value module_env element in
    let tc , tv = infer_t_insts ~raise ~options ~loc:e.location app_context (E_module_accessor {module_path; element}, tv') in
    return tc tv
  )
  | E_assign {binder; expression} ->
    let variable_type = trace_option ~raise (unbound_variable binder.var (ValueVar.get_location binder.var)) @@ Typing_context.get_value context binder.var in
    let binder = {binder with ascr=variable_type} in
    let expression = self expression in
    let expression_type = expression.type_expression in
    let () = assert_type_expression_eq ~raise e.location (variable_type,expression_type) in
    return (E_assign {binder; expression}) @@ O.t_unit ()

and type_pattern ~raise (pattern : I.type_expression option Pattern.t) (expected_typ : O.type_expression) context =
  let open Pattern in
  match pattern.wrap_content, expected_typ.type_content with
    P_unit , O.T_constant { injection = Literal_types.Unit ; _ } -> context, (Location.wrap ~loc:pattern.location P_unit)
  | P_unit , _ ->
    raise.error (wrong_type_for_unit_pattern pattern.location expected_typ)
  | P_var v , _ ->
    Context.Typing.add_value context v.var expected_typ, (Location.wrap ~loc:pattern.location (P_var {v with ascr=Some expected_typ}))
  | P_list (Cons (hd, tl)) , O.T_constant { injection = Literal_types.List ; parameters ; _ } ->
    let list_elt_typ = List.hd_exn parameters in (* TODO: dont use _exn*)
    let list_typ = expected_typ in
    let context,hd = type_pattern ~raise hd list_elt_typ context in
    let context,tl = type_pattern ~raise tl list_typ context in
    context, (Location.wrap ~loc:pattern.location (Pattern.P_list (Cons (hd, tl))))
  | P_list (List lst) , O.T_constant { injection = List ; parameters ; _ } ->
    let list_elt_typ = List.hd_exn parameters in (* TODO: dont use _exn*)
    let context, lst = List.fold_right lst ~init:(context,[])
      ~f:(fun pattern (context,lst) ->
            let context, p = type_pattern ~raise pattern list_elt_typ context in
            context, p::lst
    ) in
    context, (Location.wrap ~loc:pattern.location (Pattern.P_list (List lst)))
  | P_variant (label,pattern') , O.T_sum sum_type ->
    let label_map = sum_type.fields in
    let c = Record.LMap.find_opt label label_map in
    let c = trace_option ~raise (pattern_do_not_conform_type pattern expected_typ) c in
    let sum_typ = c.associated_type in
    let context,pattern = type_pattern ~raise pattern' sum_typ context in
    context, (Location.wrap ~loc:pattern.location (P_variant (label,pattern)))
  | P_tuple tupl , O.T_record record_type ->
    let label_map = record_type.fields in
    if Record.LMap.cardinal label_map <> List.length tupl
    then raise.error @@ pattern_do_not_conform_type pattern expected_typ
    else
    let _, context, elts = List.fold_left tupl ~init:(0, context, []) ~f:(fun (idx,context,elts) pattern' ->
      let c = Record.LMap.find_opt (Label.of_int idx) label_map in
      let c = trace_option ~raise (pattern_do_not_conform_type pattern expected_typ) c in
      let tupl_elt_typ = c.associated_type in
      let context, elt = type_pattern ~raise pattern' tupl_elt_typ context in
      idx+1, context, elt::elts) in
    let elts = List.rev elts in
    context, (Location.wrap ~loc:pattern.location (P_tuple elts))
  | P_record (labels,patterns) , O.T_record record_type ->
    let label_map = record_type.fields in
    if Record.LMap.cardinal label_map <> List.length labels
    then raise.error @@ pattern_do_not_conform_type pattern expected_typ
    else
    let label_patterns = List.zip_exn labels patterns in (* TODO: dont use _exn*)
    let label_patterns = List.sort ~compare:(fun (l1,_) (l2,_) -> Label.compare l1 l2) label_patterns in
    let context,labels,patterns = List.fold_right label_patterns ~init:(context,[],[])
      ~f:(
        fun (label,pattern') (context,labels,patterns) ->
          let c = Record.LMap.find_opt label label_map in
          let c = trace_option ~raise (pattern_do_not_conform_type pattern expected_typ) c in
          let field_typ = c.associated_type in
          let context,pattern = type_pattern ~raise pattern' field_typ context in
          context, label::labels, pattern::patterns) in
    context, (Location.wrap ~loc:pattern.location (P_record (labels,patterns)))
  | _ -> raise.error @@ pattern_do_not_conform_type pattern expected_typ

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
      let input_type = trace_option ~raise (missing_funarg_annotation binder.var) input_type in
      let e' = Typing_context.add_value e binder.var input_type in
      let body = type_expression ~raise ~options ?tv_opt:output_type (ac, e') result in
      let output_type = body.type_expression in
      (({binder={binder with ascr=input_type}; output_type ; result=body}: _ Lambda.t),(t_arrow input_type output_type ()))

and type_constant ~raise ~options (name:Constant.constant') (loc:Location.t) (lst:O.type_expression list) (tv_opt:O.type_expression option) : Constant.constant' * O.type_expression =
  let typer = Constant_typers.constant_typers ~raise ~options loc name in
  let tv = typer lst tv_opt in
  (name, tv)

and type_module_expr ~raise ~init_context ~options : I.module_expr -> typing_context * O.module_expr = fun m_expr ->
  let return x =
    let ret = Location.wrap ~loc:m_expr.location x in
    let ctxt = Typing_context.context_of_module_expr ~outer_context:init_context ret in
    ctxt, ret
  in
  let access_module ctxt v =
    trace_option ~raise (unbound_module_variable v (ModuleVar.get_location v)) (Typing_context.get_module ctxt v)
  in
  match m_expr.wrap_content with
  | M_struct prg ->
    let prg = type_module ~init_context ~raise ~options prg in
    return (M_struct prg)
  | M_module_path path ->
    let _ctxt : typing_context = List.fold
      ~f:access_module
      ~init:init_context
      (List.Ne.to_list path)
    in
    return (M_module_path path)
  | M_variable v ->
    let _ = access_module init_context v in
    return (M_variable v)

and type_declaration : raise: (typer_error,_) raise -> options: Compiler_options.middle_end -> typing_context -> I.declaration -> typing_context * O.declaration =
fun ~raise ~options c d ->
let loc = d.location in
let return ?(loc = loc) c (d : O.declaration_content) = c,Location.wrap ~loc d in
match Location.unwrap d with
  | D_value { binder = { ascr ; var ; attributes } ; attr ; expr } ->
    let av, expr = Ast_core.Combinators.get_type_abstractions expr in
    let env = List.fold av ~f:(fun c v -> Typing_context.add_type_var c v ()) ~init:c in
    let tv = Option.map ~f:(evaluate_type ~raise env) ascr in
    let expr =
      trace ~raise (constant_declaration_tracer loc var expr tv) @@
      type_expression ~options ?tv_opt:tv (App_context.create tv, env) expr in
    let expr = Ast_typed.Helpers.build_type_abstractions expr (List.rev av) in
    let c = Typing_context.add_value c var expr.type_expression in
    let attr = type_value_attr attr in
    return c @@ D_value { binder = { ascr = tv ; var ; attributes } ; expr ; attr }
  | D_type {type_binder ; type_expr; type_attr={public;hidden} } -> (
    let tv = evaluate_type ~raise c type_expr in
    let tv = {tv with orig_var = Some type_binder} in
    let env' = Typing_context.add_type c type_binder tv in
    return env' @@ D_type { type_binder ; type_expr = tv; type_attr={public;hidden} }
  )
  | D_module { module_binder ; module_ ; module_attr = {public ; hidden} } -> (
    let module_ctxt, module_ = type_module_expr ~raise ~init_context:c ~options module_ in
    let post_env = Typing_context.add_module c module_binder module_ctxt in
    return post_env @@ D_module { module_binder; module_; module_attr = {public;hidden}}
  )

and type_decl ~raise ~options c : I.decl -> typing_context * O.decl =
  fun d ->
    let c,d = type_declaration ~raise ~options c d in
    c, d
and type_module ~raise ~options ~init_context (p:I.module_) : O.module_ =
  (* This context use all the declaration so you can use private declaration to type the module. It should not be returned*)
  let (_c, lst) =
      List.fold_map ~f:(type_decl ~raise ~options) ~init:init_context p in
  lst

let type_program ~raise ~options ?env (p : I.program) : O.program =
  let (_c, lst) =
      List.fold_map ~f:(type_declaration ~raise ~options) ~init:(Typing_context.init ?env ()) p in
  lst
let type_declaration ~raise ~options ?env d = snd @@ type_declaration ~raise ~options (Typing_context.init ?env ()) d
let type_expression ~raise ~options ?env ?tv_opt e =
    type_expression ~raise ~options (App_context.create tv_opt, Typing_context.init ?env ()) ?tv_opt e
