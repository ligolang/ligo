open Trace
open Ligo_interpreter_exc

(* returns the name of the prepended variable definition for $-substitutions in Test.compile_expression *)
let subst_vname s = "test_gen_"^s

let int_of_mutez t = Z.of_int64 @@ Memory_proto_alpha.Protocol.Alpha_context.Tez.to_mutez t
let compile_contract source_file entry_point =
  let open Ligo_compile in
  let syntax = "auto" in
  let options = Compiler_options.make () in
  let* michelson = Build.build_contract ~options syntax entry_point source_file in
  Of_michelson.build_contract ~disable_typecheck:false michelson

let simple_val_insertion ~loc michelson_ty michelson_value ligo_obj_ty : (Ast_typed.expression,_) result =
  let open Tezos_micheline in
  let open Ast_typed in
  let cano (x: unit Tezos_utils.Michelson.michelson) =
    let open Tezos_micheline.Micheline in
    let x = inject_locations (fun _ -> 0) (strip_locations x) in
    let x = strip_locations x in
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
        (* let (_,t) = get_t_function_exn t in *)
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
  let* options        = Run.Of_michelson.make_dry_run_options {now=None ; amount="" ; balance="" ; sender=None ; source=None ; parameter_ty = None } in
  let* runres         = Run.Of_michelson.run_expression ~options compiled_exp.expr compiled_exp.expr_ty in
  let* (expr_ty,expr) = match runres with | Success x -> ok x | Fail _ -> fail @@ Errors.generic_error loc "evaluation of storage failed" in
  let expr = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr) in
  let expr_ty = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations expr_ty) in
  ok (expr, expr_ty, typed_exp.type_expression)

let rec compile_simple_val ~loc : Ligo_interpreter.Types.value -> (_, _ ) result =
  fun v ->
    (* TODO, this should take a type expression *)
    let open Ligo_interpreter.Types in
    let open Tezos_utils.Michelson in
    match v with
    | V_Ct (C_int x)    -> ok (int x    , prim "int"    , Ast_typed.t_int ())
    | V_Ct (C_string x) -> ok (string x , prim "string" , Ast_typed.t_string ())
    | V_Ct (C_nat x)    -> ok (int x    , prim "nat"    , Ast_typed.t_nat ())
    | V_Ct (C_bytes x)  -> ok (bytes x  , prim "bytes"  , Ast_typed.t_bytes ())
    | V_Ct (C_unit)     -> ok (d_unit   , prim "unit"   , Ast_typed.t_unit ())
    | V_Ct (C_address x) ->
      let x = Format.asprintf "%a" Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.pp x in
      ok (string x, prim "address" , Ast_typed.t_address ())
    | V_Record m when LMap.cardinal m = 2 -> 
      let a = LMap.find (Label "0") m in
      let b = LMap.find (Label "1") m in
      let* (mva,mta,lta) = compile_simple_val ~loc a in
      let* (mvb,mtb,ltb) = compile_simple_val ~loc b in
      ok (d_pair mva mvb , t_pair mta mtb, Ast_typed.t_pair lta ltb)
    | _ -> fail @@ Errors.generic_error loc "The value in meta langugage is too complex to be compiled to michelson"

let storage_retreival_dummy_ty = Tezos_utils.Michelson.prim "unit"