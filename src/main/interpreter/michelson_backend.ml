open Trace
open Ligo_interpreter_exc

(* returns the name of the prepended variable definition for $-substitutions in Test.compile_expression *)
let subst_vname s = "test_gen_"^s

let int_of_mutez t = Z.of_int64 @@ Memory_proto_alpha.Protocol.Alpha_context.Tez.to_mutez t

let string_of_contract t = Format.asprintf "%a" Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.pp t
let string_of_key_hash t = Format.asprintf "%a" Tezos_crypto.Signature.Public_key_hash.pp t

let compile_contract source_file entry_point =
  let open Ligo_compile in
  let syntax = "auto" in
  let options = Compiler_options.make () in
  let* michelson = Build.build_contract ~options syntax entry_point source_file in
  Of_michelson.build_contract ~disable_typecheck:false michelson

let clean_location_with v x =
  let open Tezos_micheline.Micheline in
  inject_locations (fun _ -> v) (strip_locations x)

let clean_locations e t =
  clean_location_with () e, clean_location_with () t

let simple_val_insertion ~loc michelson_ty michelson_value ligo_obj_ty : (Ast_typed.expression,_) result =
  let open Tezos_micheline in
  let open Ast_typed in
  let cano (x: unit Tezos_utils.Michelson.michelson) =
    let x = Tezos_micheline.Micheline.strip_locations
              (clean_location_with 0 x) in
    let* x = Proto_alpha_utils.Trace.trace_alpha_tzresult (throw_obj_exc loc) @@
      Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.prims_of_strings x (* feels wrong ... *)
    in
    ok x
  in
  let code = Tezos_utils.Michelson.(seq [i_drop ; (i_push michelson_ty michelson_value)]) in
  let* expr = cano code in
  let u = Format.asprintf "%a" Micheline_printer.print_expr
            (Micheline_printer.printable Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.string_of_prim expr)
  in
  let type_annotation = t_function (t_unit ()) ligo_obj_ty () in
  let code_block = make_e (e_string (Ligo_string.verbatim u)) type_annotation in
  let insertion = e_a_raw_code Stage_common.Backends.michelson code_block type_annotation in
  let applied = e_a_application insertion e_a_unit ligo_obj_ty in
  ok applied

let add_ast_env ?(name = Location.wrap (Var.fresh ())) env binder body =
       let open Ast_typed in
       let aux exp (s,(mv,_t,_o)) : (expression,_) result =
         let let_binder = Location.wrap @@ Var.of_name s in
         if Var.compare let_binder.wrap_content binder.Location.wrap_content <> 0 && Var.compare let_binder.wrap_content name.wrap_content <> 0 then
           ok @@ e_a_let_in let_binder mv exp false
         else
           ok @@ exp in
    let* typed_exp' = bind_fold_right_list aux body env in
    ok typed_exp'

let make_options ?param ctxt =
  let open Ligo_run.Of_michelson in
  let open Ligo_interpreter.Types in
  let default = { now = None ;
                  amount = "" ;
                  balance = "" ;
                  sender = None ;
                  source = None ;
                  parameter_ty = param } in
  match ctxt with
  | None ->
     make_dry_run_options default
  | Some ctxt ->
     let tezos_context = Tezos_state.get_alpha_context ctxt in
     let source = string_of_contract ctxt.source in
     let* options = make_dry_run_options ?tezos_context { default with source = Some source } in
     let timestamp = Timestamp.of_zint (Z.of_int64 (Proto_alpha_utils.Time.Protocol.to_seconds (Tezos_state.get_timestamp ctxt))) in
     ok @@ { options with now = timestamp }

let run_expression_unwrap ?ctxt ?(loc = Location.generated) (c_expr : Stacking.compiled_expression) =
  let* options = make_options ctxt in
  let* runres = Ligo_run.Of_michelson.run_expression ~options c_expr.expr c_expr.expr_ty in
  match runres with
  | Success (expr_ty, expr) ->
     let expr, expr_ty = clean_locations expr expr_ty in
     ok (expr, expr_ty)
  | Fail _ ->
     fail @@ Errors.generic_error loc "Running failed"

let compile_value typed_exp =
  let open Ligo_compile in
  let options = Compiler_options.make () in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  let* compiled_exp   = Of_mini_c.aggregate_and_compile_expression ~options [] mini_c_exp in
  ok compiled_exp

let compile_contract_ subst_lst arg_binder in_ty out_ty typed_exp =
  let open Ligo_compile in
  let options = Compiler_options.make () in
  let subst_lst = List.rev subst_lst in
  let* typed_exp' = add_ast_env subst_lst arg_binder typed_exp in
  let typed_exp = Ast_typed.e_a_lambda {result=typed_exp'; binder=arg_binder} in_ty out_ty in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  let* compiled_exp   = Of_mini_c.aggregate_and_compile ~options [] (ContractForm mini_c_exp) in
  ok compiled_exp

let make_function in_ty out_ty arg_binder body subst_lst =
  let* typed_exp' = add_ast_env subst_lst arg_binder body in
  ok @@ Ast_typed.e_a_lambda {result=typed_exp'; binder=arg_binder} in_ty out_ty

let compile_expression ~loc syntax exp_as_string source_file subst_lst =
  let open Ligo_compile in
  let options = Compiler_options.make () in
  let* (decl_list,env) = match source_file with
    | Some init_file -> Build.build_mini_c ~options syntax Env init_file
    | None -> ok ([],options.init_env)
  in
  let* typed_exp =
    match subst_lst with
    | [] ->
      let* typed_exp,_  = Utils.type_expression ~options source_file syntax exp_as_string env in
      ok typed_exp
    (* this handle $-substitution by prepeding `let test_gen_x = [%Micheson {| some_code |}] () in ..` to the compiled expression *)
    | lst ->
      let open Ast_typed in
      let aux env (s,(_,_,t)) : environment = (* adds substituted value types to the env, feels wrong ... *)
        let s = subst_vname s in
        let v = Location.wrap @@ Var.of_name s in
        Ast_typed.Environment.add_ez_binder v t env
      in
      let env' = List.fold_left ~f:aux ~init:env lst in
      let* (typed_exp,_) = Utils.type_expression ~options source_file syntax exp_as_string env' in
      let aux exp (s,(mv,mt,t)) : (expression,_) result =
        let s = subst_vname s in
        let let_binder = Location.wrap @@ Var.of_name s in
        let* applied = simple_val_insertion ~loc mt mv t in
        ok @@ e_a_let_in let_binder applied exp false
      in
      let* typed_exp' = bind_fold_right_list aux typed_exp lst in
      ok typed_exp'
  in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  let* compiled_exp   = Of_mini_c.aggregate_and_compile_expression ~options decl_list mini_c_exp in
  let* expr, expr_ty  = run_expression_unwrap ~loc compiled_exp in
  ok (expr, expr_ty, typed_exp.type_expression)

let rec val_to_ast ~loc ?(toplevel = true) : Ligo_interpreter.Types.value ->
                          Ast_typed.type_expression ->
                          (_, _ ) result =
  fun v ty ->
  let open Ligo_interpreter.Types in
  let open Ast_typed in
  match v with
  | V_Ct C_unit ->
     let* () = trace_option (Errors.generic_error loc "Expected unit")
                 (get_t_unit ty) in
     ok e_a_unit
  | V_Ct (C_bool b) ->
     let* () = trace_option (Errors.generic_error loc "Expected bool")
                 (get_t_bool ty) in
     ok @@ e_a_bool b
  | V_Ct (C_int x) ->
     let* () = trace_option (Errors.generic_error loc "Expected int")
                 (get_t_int ty) in
     ok @@ e_a_int x
  | V_Ct (C_nat x) ->
     let* () = trace_option (Errors.generic_error loc "Expected nat")
                 (get_t_nat ty) in
     ok @@ e_a_nat x
  | V_Ct (C_mutez x) ->
     let* () = trace_option (Errors.generic_error loc "Expected mutez")
                 (get_t_mutez ty) in
     ok @@ e_a_mutez x
  | V_Ct (C_timestamp t) ->
     let* () = trace_option (Errors.generic_error loc "Expected timestamp")
                 (get_t_timestamp ty) in
     ok @@ e_a_timestamp t
  | V_Ct (C_string s) ->
     let* () = trace_option (Errors.generic_error loc "Expected string")
                 (get_t_string ty) in
     ok @@ e_a_string (Simple_utils.Ligo_string.standard s)
  | V_Ct (C_bytes b) ->
     let* () = trace_option (Errors.generic_error loc "Expected bytes")
                 (get_t_bytes ty) in
     ok @@ e_a_bytes b
  | V_Ct (C_address a) when is_t_address ty ->
     let* () = trace_option (Errors.generic_error loc "Expected address")
                 (get_t_address ty) in
     let x = string_of_contract a in
     ok @@ e_a_address x
  | V_Ct (C_address _) ->
     fail @@ (Errors.generic_error loc "Expected address")
  | V_Ct (C_contract c) when is_t_contract ty ->
     let* ty = trace_option (Errors.generic_error loc "Expected contract")
                 (get_t_contract ty) in
     let x = string_of_contract c.address in
     (* TODO-er: if we want support for entrypoints, this should be fixed: *)
     let t = match c.entrypoint with
     | None -> e_a_contract (e_a_address x) ty
     | Some e ->
        e_a_contract_entrypoint (e_a_string (Ligo_string.Standard ("%" ^ e))) (e_a_address x) ty in
     ok @@ t
  | V_Ct (C_contract _) ->
     fail @@ (Errors.generic_error loc "Expected contract")
  | V_Ct (C_key_hash kh) ->
     let* () = trace_option (Errors.generic_error loc "Expected timestamp")
                 (get_t_key_hash ty) in
     let x = string_of_key_hash kh in
     ok @@ e_a_key_hash x
  | V_Construct (ctor, arg) when is_t_option ty ->
     let* ty' = trace_option (Errors.generic_error loc "Expected option") @@ get_t_option ty in
     if String.equal ctor "Some" then
       let* arg = val_to_ast ~loc arg ty' in
       ok @@ e_a_some arg
     else if String.equal ctor "None" then
       ok @@ e_a_none ty'
     else
       fail @@ Errors.generic_error loc "Expected either None or Some"
  | V_Construct (ctor, arg) when is_t_sum ty ->
     let* map_ty = trace_option (Errors.generic_error loc "Expected sum") @@ get_t_sum ty in
     let {associated_type=ty'} = LMap.find (Label ctor) map_ty.content in
     let* arg = val_to_ast ~loc arg ty' in
     ok @@ e_a_constructor ctor arg ty
  | V_Construct _ ->
     fail @@ Errors.generic_error loc "Expected sum type"
  | V_Func_val v ->
     make_ast_func ~toplevel ?name:v.rec_name v.env v.arg_binder v.body v.orig_lambda
  | V_Michelson (Ty_code (expr, expr_ty, ty_exp)) ->
     let* mini_c = trace Main_errors.decompile_michelson @@ Stacking.Decompiler.decompile_value expr_ty expr in
     let* typed = trace Main_errors.decompile_mini_c @@ Spilling.decompile mini_c ty_exp in
     ok @@ typed
  | V_Record map when is_t_record ty ->
     let* map_ty = trace_option (Errors.generic_error loc "Expected record") @@  get_t_record ty in
     make_ast_record ~loc map_ty map
  | V_Record _ ->
     fail @@ Errors.generic_error loc "Is it a tuple or a pair?"
  | V_List l ->
     let* ty = trace_option (Errors.generic_error loc "Expected list") @@ get_t_list ty in
     make_ast_list ~loc ty l
  | V_Set l ->
     let* ty = trace_option (Errors.generic_error loc "Expected set") @@ get_t_set ty in
     make_ast_set ~loc ty l
  | V_Map kv when is_t_big_map ty ->
     let* (key_ty, value_ty) = trace_option (Errors.generic_error loc "Expected big_map") @@ get_t_big_map ty in
     make_ast_big_map ~loc key_ty value_ty kv
  | V_Map kv when is_t_map ty ->
     let* (key_ty, value_ty) = trace_option (Errors.generic_error loc "Expected map") @@ get_t_map ty in
     make_ast_map ~loc key_ty value_ty kv
  | V_Map _ ->
     fail @@ Errors.generic_error loc "Expected either map or big_map"
  | V_BigMap id when is_t_big_map ty ->
     let* (key_ty, value_ty) = trace_option (Errors.generic_error loc "Expected big_map") @@ get_t_big_map ty in
     make_ast_big_map_id ~loc key_ty value_ty id
  | V_BigMap _ ->
     fail @@ Errors.generic_error loc "Expected big_map"
  | V_Ligo _ ->
     fail @@ Errors.generic_error loc "Cannot be abstracted: ligo"
  | V_Michelson (Contract _) ->
     fail @@ Errors.generic_error loc "Cannot be abstracted: michelson-contract"

and make_ast_func ?(toplevel = true) ?name env arg body orig =
  let open Ast_typed in
  let* fv = Self_ast_typed.Helpers.get_fv body in
  let* env = make_subst_ast_env_exp ~toplevel env fv in
  let* typed_exp' = add_ast_env ?name:name env arg body in
  let lambda = { result=typed_exp' ; binder=arg} in
  let typed_exp' = match name with
    | None ->
       let in_ty,out_ty =
         get_t_function_exn orig.type_expression in
       e_a_lambda lambda in_ty out_ty
    | Some fun_name ->
       e_a_recursive {fun_name ;
                      fun_type = orig.type_expression ;
                      lambda } in
  ok typed_exp'

and make_ast_record ~loc map_ty map =
  let open Ligo_interpreter.Types in
  let kv_list = Ast_typed.Helpers.kv_list_of_t_record_or_tuple ~layout:map_ty.layout map_ty.content in
  let* kv_list = bind_map_list (fun (l, ty) -> let value = LMap.find l map in let* ast = val_to_ast ~loc value ty.associated_type in ok @@ (l, ast)) kv_list in
  ok @@ Ast_typed.ez_e_a_record ~layout:map_ty.layout kv_list

and make_ast_list ~loc ty l =
  let* l = bind_map_list (fun v -> val_to_ast ~loc v ty) l in
  ok @@ List.fold_right l ~f:Ast_typed.e_a_cons ~init:(Ast_typed.e_a_nil ty)

and make_ast_set ~loc ty l =
  let* l = bind_map_list (fun v -> val_to_ast ~loc v ty) l in
  let l = List.dedup_and_sort ~compare l in
  ok @@ List.fold_right l ~f:Ast_typed.e_a_set_add ~init:(Ast_typed.e_a_set_empty ty)

and make_ast_big_map ~loc key_ty value_ty kv =
  let* kv = bind_map_list (fun (k, v) ->
                let* k = val_to_ast ~loc k key_ty in
                let* v = val_to_ast ~loc v value_ty in
                ok (k, v)) kv in
  let kv = List.dedup_and_sort ~compare kv in
  ok @@ List.fold_right kv ~f:(fun (k, v) r -> Ast_typed.e_a_big_map_add k v r) ~init:(Ast_typed.e_a_big_map_empty key_ty value_ty)

and make_ast_big_map_id ~loc key_ty value_ty (id, kv) =
  let* kv = bind_map_list (fun (k, v) ->
                match v with
                | Some v ->
                   let* k = val_to_ast ~loc k key_ty in
                   let* v = val_to_ast ~loc v value_ty in
                   ok @@ Ast_typed.e_a_big_map_add k v
                | None ->
                   let* k = val_to_ast ~loc k key_ty in
                   ok @@ Ast_typed.e_a_big_map_remove k
) kv in
  let e = Ast_typed.e_a_big_map_identifier key_ty value_ty (Ast_typed.e_a_nat id) in
  ok @@ List.fold_right kv ~f:(fun mk t -> mk t) ~init:e

and make_ast_map ~loc key_ty value_ty kv =
  let* kv = bind_map_list (fun (k, v) ->
                let* k = val_to_ast ~loc k key_ty in
                let* v = val_to_ast ~loc v value_ty in
                ok (k, v)) kv in
  let kv = List.dedup_and_sort ~compare kv in
  ok @@ List.fold_right kv ~f:(fun (k, v) r -> Ast_typed.e_a_map_add k v r) ~init:(Ast_typed.e_a_map_empty key_ty value_ty)

and compile_simple_value ?ctxt ~loc ?(toplevel = true) : Ligo_interpreter.Types.value ->
                       Ast_typed.type_expression ->
                       (_, _ ) result =
  fun v ty ->
  let* typed_exp = val_to_ast ~loc ~toplevel v ty in
  let* () = Check.check_obj_ligo typed_exp in
  let* compiled_exp = compile_value typed_exp in
  let* expr, _ = run_expression_unwrap ?ctxt ~loc compiled_exp in
  (* TODO-er: check the ignored second component: *)
  let expr_ty = clean_location_with () compiled_exp.expr_ty in
  ok (expr, expr_ty, typed_exp.type_expression)

and make_subst_ast_env_exp ?(toplevel = true) env fv =
  let open Ligo_interpreter.Types in
  let lst = if toplevel then
               (List.rev env)
             else
               [] in
  let op (l, fv) (evl, v : _ * Ligo_interpreter.Types.value_expr) =
    let loc = Location.get_location evl in
    let ev = Location.unwrap evl in
    if not (List.mem fv evl ~equal:Self_ast_typed.Helpers.eq_vars) then
      ok (l, fv)
    else
      match v with
      | { ast_type = Some ty } ->
         let* expr = val_to_ast ~toplevel:false ~loc v.eval_term ty in
         let l = (Var.to_name ev, (expr,ty, None)) :: l in
         let* fv' = Self_ast_typed.Helpers.get_fv expr in
         ok @@ (l, fv @ fv')
      | _ ->
         ok @@ (l, fv) in
  let* (l, _) = bind_fold_right_list op ([], fv) lst in
  ok @@ (List.rev l)

let get_literal_type : Ast_typed.literal -> Ast_typed.type_expression =
  fun t ->
  let open Ast_typed in
  match t with
  | (Literal_unit) -> t_unit ()
  | (Literal_int _) -> t_int ()
  | (Literal_nat _) -> t_nat ()
  | (Literal_mutez _) -> t_mutez ()
  | (Literal_string _) -> t_string ()
  | (Literal_bytes _) -> t_bytes ()
  | (Literal_timestamp _) -> t_timestamp ()
  | (Literal_address _) -> t_address ()
  | (Literal_signature _) -> t_signature ()
  | (Literal_key _) -> t_key ()
  | (Literal_key_hash _) -> t_key_hash ()
  | (Literal_chain_id _) -> t_chain_id ()
  | (Literal_operation _) -> t_operation ()

let compile_literal ~loc : Ast_typed.literal -> (_, _ ) result =
  fun v ->
  let open Ligo_interpreter.Types in
  let type_lit = get_literal_type v in
  let typed_exp = Ast_typed.e_a_literal v type_lit in
  let* compiled_exp = compile_value typed_exp in
  let* expr, expr_ty = run_expression_unwrap ~loc compiled_exp in
  ok (expr, expr_ty, typed_exp.type_expression)

let storage_retreival_dummy_ty = Tezos_utils.Michelson.prim "int"
