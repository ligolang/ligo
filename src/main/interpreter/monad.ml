open Trace
(*
  That monad do not seem very useful now,
  but it could become useful if we want to support multiple testing mode (against node, or memory)
*)


module LT = Ligo_interpreter.Types
module LC = Ligo_interpreter.Combinators
module Int_repr = Ligo_interpreter.Int_repr_copied
module Exc = Ligo_interpreter_exc

type execution_trace = unit
type 'a result_monad = ('a,Errors.interpreter_error) result

let ( let>>= ) o f = Trace.bind f o

let corner_case ?(loc = Location.generated) () = Errors.generic_error loc "Corner case, please report to devs."
let add_warning _ = ()

let wrap_compare compare a b =
  let res = compare a b in
  if (res = 0) then 0 else if (res > 0) then 1 else -1

let clean_locations ty = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations ty)

module Command = struct
  type 'a t =
    | Get_big_map : Location.t * LT.type_expression * LT.type_expression * LT.value * Z.t -> LT.expression t
    | Mem_big_map : Location.t * LT.type_expression * LT.type_expression * LT.value * Z.t -> bool t
    | Bootstrap_contract : int * LT.value * LT.value * Ast_typed.type_expression  -> unit t
    | Nth_bootstrap_contract : int -> Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.t t
    | Nth_bootstrap_typed_address : Location.t * int -> (Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.t * Ast_typed.type_expression * Ast_typed.type_expression) t
    | Reset_state : Location.t * LT.value * LT.value -> unit t
    | External_call : Location.t * LT.contract * (execution_trace, string) Tezos_micheline.Micheline.node * Z.t -> Tezos_state.state_error option t
    | State_error_to_value : Tezos_state.state_error -> LT.value t
    | Get_storage : Location.t * LT.value * Ast_typed.type_expression -> Ast_typed.expression t
    | Get_storage_of_address : Location.t * LT.value -> LT.value t
    | Get_size : LT.value -> LT.value t
    | Get_balance : Location.t * LT.value -> LT.value t
    | Get_last_originations : unit -> LT.value t
    | Check_obj_ligo : LT.expression -> unit t
    | Compile_expression : Location.t * LT.value * string * string * LT.value option -> LT.value t
    | Mutate_expression : Location.t * Z.t * string * string -> (string * string) t
    | Mutate_count : Location.t * string * string -> LT.value t
    | Mutate_some_value : Location.t * Z.t * LT.value * Ast_typed.type_expression -> (Ast_typed.expression * LT.mutation) option t
    | Mutate_all_value : Location.t * LT.value * Ast_typed.type_expression -> (Ast_typed.expression * LT.mutation) list t
    | Compile_contract_from_file : string * string -> (LT.value * LT.value) t
    | Compile_meta_value : Location.t * LT.value * Ast_typed.type_expression -> LT.value t
    | Run : Location.t * LT.func_val * LT.value -> LT.value t
    | Eval : Location.t * LT.value * Ast_typed.type_expression -> LT.value t
    | Compile_contract : Location.t * LT.value * Ast_typed.type_expression -> LT.value t
    | To_contract : Location.t * LT.value * string option * Ast_typed.type_expression -> LT.value t
    | Check_storage_address : Location.t * Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.t * Ast_typed.type_expression -> unit t
    | Contract_exists : Location.t * LT.value -> bool t
    | Inject_script : Location.t * LT.value * LT.value * Z.t -> LT.value t
    | Set_now : Location.t * Z.t -> unit t
    | Set_source : LT.value -> unit t
    | Set_baker : LT.value -> unit t
    | Get_bootstrap : Location.t * LT.value -> LT.value t
    | Michelson_equal : Location.t * LT.value * LT.value -> bool t
    | Int_compare_wrapped : 'a Int_repr.num * 'a Int_repr.num -> int t
    | Int_compare : 'a Int_repr.num * 'a Int_repr.num -> int t
    | Int_abs : Int_repr.z Int_repr.num -> Int_repr.n Int_repr.num t
    | Int_of_zint : Z.t -> Int_repr.z Int_repr.num t
    | Int_to_zint : 'a Int_repr.num -> Z.t t
    | Int_of_int64 : int64 -> Int_repr.z Int_repr.num t
    | Int_to_int64 : _ Int_repr.num -> int64 option t
    | Int_is_nat : Int_repr.z Int_repr.num -> Int_repr.n Int_repr.num option t
    | Int_neg : _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_add : _ Int_repr.num * _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_add_n : Int_repr.n Int_repr.num * Int_repr.n Int_repr.num -> Int_repr.n Int_repr.num t
    | Int_mul : _ Int_repr.num * _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_mul_n : Int_repr.n Int_repr.num * Int_repr.n Int_repr.num -> Int_repr.n Int_repr.num t
    | Int_ediv :
      _ Int_repr.num * _ Int_repr.num ->
      (Int_repr.z Int_repr.num * Int_repr.n Int_repr.num) option t
    | Int_ediv_n :
      Int_repr.n Int_repr.num * Int_repr.n Int_repr.num ->
      (Int_repr.n Int_repr.num * Int_repr.n Int_repr.num) option t
    | Int_sub : _ Int_repr.num * _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_shift_left : 'a Int_repr.num * Int_repr.n Int_repr.num -> 'a Int_repr.num option t
    | Int_shift_right : 'a Int_repr.num * Int_repr.n Int_repr.num -> 'a Int_repr.num option t
    | Int_logor : ('a Int_repr.num * 'a Int_repr.num) -> 'a Int_repr.num t
    | Int_logand : (_ Int_repr.num * Int_repr.n Int_repr.num) -> Int_repr.n Int_repr.num t
    | Int_logxor : (Int_repr.n Int_repr.num * Int_repr.n Int_repr.num) -> Int_repr.n Int_repr.num t
    | Int_lognot : _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_of_int : int -> Int_repr.z Int_repr.num t
    | Int_int : Int_repr.n Int_repr.num -> Int_repr.z Int_repr.num t

  let eval
    : type a.
      a t ->
      Tezos_state.context ->
      execution_trace ref option ->
      (a * Tezos_state.context) result_monad
    = fun command ctxt _log ->
    match command with
    | Get_big_map (loc, k_ty, v_ty, _k, _m) ->
      (* TODO-er: hack to get the micheline type... *)
      let* none_compiled = Michelson_backend.compile_value (Ast_typed.e_a_none v_ty) in
      let val_ty = clean_locations none_compiled.expr_ty in
      let inner_ty = match val_ty with
        | Prim (_, "option", [l], _) ->
           l
        | _ -> failwith "None has a non-option type?" in
      let* key,key_ty,_ = Michelson_backend.compile_simple_value ~ctxt ~loc _k k_ty in
      let* storage' = Tezos_state.get_big_map ~loc ctxt _m key key_ty in
      begin
        match storage' with
        | Some storage' ->
           let code = storage'
                      |> Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.strings_of_prims
                      |> Tezos_micheline.Micheline.inject_locations (fun _ -> ()) in
           let* mini_c = trace Main_errors.decompile_michelson @@ Stacking.Decompiler.decompile_value inner_ty code in
           let* typed = trace Main_errors.decompile_mini_c @@ Spilling.decompile mini_c v_ty in
           let typed = Ast_typed.e_a_some typed in
           ok @@ (typed, ctxt)
        | None -> ok @@ (Ast_typed.e_a_none v_ty, ctxt)
      end
    | Mem_big_map (loc, k_ty, _v_ty, _k, _m) ->
      let* key,key_ty,_ = Michelson_backend.compile_simple_value ~ctxt ~loc _k k_ty in
      let* storage' = Tezos_state.get_big_map ~loc ctxt _m key key_ty in
      ok (Option.is_some storage', ctxt)
    | Nth_bootstrap_contract (n) ->
      let* contract = Tezos_state.get_bootstrapped_contract n in
      ok (contract,ctxt)
    | Nth_bootstrap_typed_address (loc, n) ->
      let* contract = Tezos_state.get_bootstrapped_contract n in
      let* storage_ty =
        trace_option (Errors.generic_error loc "Storage type not available" ) @@
          List.Assoc.find ~equal:(Tezos_state.compare_account) ctxt.storage_tys contract in
      let* parameter_ty =
        trace_option (Errors.generic_error loc "Parameter type not available" ) @@
          List.Assoc.find ~equal:(Tezos_state.compare_account) ctxt.parameter_tys contract in
      let* contract = Tezos_state.get_bootstrapped_contract n in
      ok ((contract, parameter_ty, storage_ty),ctxt)
    | Bootstrap_contract (mutez, contract, storage, contract_ty) ->
      let* contract = trace_option (corner_case ()) @@ LC.get_michelson_contract contract in
      let* input_ty, _ = trace_option (corner_case ()) @@ Ast_typed.get_t_function contract_ty in
      let* parameter_ty, _ = trace_option (corner_case ()) @@ Ast_typed.get_t_pair input_ty in
      let* (storage,_,storage_ty) = trace_option (corner_case ()) @@ LC.get_michelson_expr storage in
      let ctxt =
        { ctxt with next_bootstrapped_contracts = (mutez, contract, storage, parameter_ty, storage_ty) :: ctxt.next_bootstrapped_contracts } in
      ok ((),ctxt)
    | Reset_state (loc,n,amts) ->
      let* amts = trace_option (corner_case ()) @@ LC.get_list amts in
      let* amts = bind_map_list
        (fun x ->
          let* x = trace_option (corner_case ()) @@ LC.get_nat x in
          ok (Z.to_int64 x) )
        amts
      in
      let* n = trace_option (corner_case ()) @@ LC.get_nat n in
      let* ctxt = Tezos_state.init_ctxt ~loc ~initial_balances:amts ~n:(Z.to_int n) (List.rev ctxt.next_bootstrapped_contracts) in
      ok ((),ctxt)
    | External_call (loc, { address; entrypoint }, param, amt) -> (
      let* x = Tezos_state.transfer ~loc ctxt address ?entrypoint param amt in
      match x with
      | Success ctxt -> ok (None, ctxt)
      | Fail errs -> ok (Some errs, ctxt)
    )
    | State_error_to_value errs -> (
      match Tezos_state.get_contract_rejection_data errs with
      | Some (addr,v) ->
        let t = Michelson_backend.storage_retreival_dummy_ty in
        let v = LT.V_Michelson (Ty_code (v, t, Ast_typed.t_int ())) in
        let addr = LT.V_Ct (C_address addr) in
        let err = LC.v_ctor "Rejected" (LC.v_pair (v,addr)) in
        ok (LC.v_ctor "Fail" err, ctxt)
      | None ->
        ok (LC.v_ctor "Fail" (LC.v_ctor "Other" (LC.v_unit ())), ctxt)
    )
    | Get_storage (loc, addr, ty_expr) ->
      let* addr = trace_option (corner_case ()) @@ LC.get_address addr in
      let* (storage',ty) = Tezos_state.get_storage ~loc ctxt addr in
      let storage = storage'
        |> Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.strings_of_prims
        |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())
      in
      let ret = LT.V_Michelson (Ty_code (storage,ty,ty_expr)) in
      let* ret = Michelson_backend.val_to_ast ~loc ret ty_expr in
      ok (ret, ctxt)
    | Get_balance (loc,addr) ->
      let* addr = trace_option (corner_case ()) @@ LC.get_address addr in
      let* balance = Tezos_state.get_balance ~loc ctxt addr in
      let mutez = Michelson_backend.int_of_mutez balance in
      let balance = LT.V_Ct (C_mutez mutez) in
      ok (balance, ctxt)
    | Get_storage_of_address (loc, addr) ->
      let* addr = trace_option (corner_case ()) @@ LC.get_address addr in
      let* (storage',ty) = Tezos_state.get_storage ~loc ctxt addr in
      let storage = storage'
        |> Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.strings_of_prims
        |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())
      in
      let* ligo_ty =
        trace_option (Errors.generic_error loc "Not supported (yet) when the provided account has been fetched from Test.get_last_originations" ) @@
          List.Assoc.find ~equal:(Tezos_state.compare_account) ctxt.storage_tys addr
      in
      let ret = LT.V_Michelson (Ty_code (storage,ty,ligo_ty)) in
      ok (ret, ctxt)
    | Check_obj_ligo e ->
      let* () = Check.check_obj_ligo e in
      ok ((), ctxt)
    | Compile_expression (loc, source_file, syntax, exp_as_string, subst_opt) ->
      let* file_opt = trace_option (corner_case ()) @@ LC.get_string_option source_file in
      let* substs =
        match subst_opt with
        | None -> ok []
        | Some substs ->
          let* lst = trace_option (corner_case ()) @@ LC.get_list substs in
          let aux = fun el ->
            let* (s,c) = trace_option (corner_case ()) @@ LC.get_pair el in
            let* s = trace_option (corner_case ()) @@ LC.get_string s in
            let* i = trace_option (corner_case ()) @@ LC.get_michelson_expr c in
            ok (s, i)
          in
          bind_map_list aux lst
      in
      let aux = fun exp_str (s,_) -> (* TODO: a bit naive .. *)
        Str.substitute_first (Str.regexp ("\\$"^s)) (fun _ -> Michelson_backend.subst_vname s) exp_str
      in
      let exp_as_string' = List.fold_left ~f:aux ~init:exp_as_string substs in
      let* (mich_v, mich_ty, object_ty) = Michelson_backend.compile_expression ~loc ~add_warning syntax exp_as_string' file_opt substs in
      ok (LT.V_Michelson (LT.Ty_code (mich_v, mich_ty, object_ty)), ctxt)
    | Compile_meta_value (loc,x,ty) ->
      let* x = Michelson_backend.compile_simple_value ~ctxt ~loc x ty in
      ok (LT.V_Michelson (LT.Ty_code x), ctxt)
    | Get_size (contract_code) ->
       begin
         match contract_code with
         | LT.V_Michelson (LT.Contract contract_code) ->
            let* s = Ligo_compile.Of_michelson.measure contract_code in
            ok @@ (LT.V_Ct (C_int (Z.of_int s)), ctxt)
         | _ -> fail @@ Errors.generic_error Location.generated
                          "Trying to measure a non-contract"
       end
    | Compile_contract_from_file (source_file, entrypoint) ->
      let* contract_code =
        Michelson_backend.compile_contract ~add_warning source_file entrypoint in
      let* size =
        let* s = Ligo_compile.Of_michelson.measure contract_code in
        ok @@ LT.V_Ct (C_int (Z.of_int s))
      in
      let contract = LT.V_Michelson (LT.Contract contract_code) in
      ok ((contract,size), ctxt)
    | Run (loc, f, v) ->
      let open Ligo_interpreter.Types in
      let* fv = Self_ast_typed.Helpers.get_fv f.orig_lambda in
      let* subst_lst = Michelson_backend.make_subst_ast_env_exp ~toplevel:true f.env fv in
      let* in_ty, out_ty = trace_option (Errors.generic_error loc "Trying to run a non-function?") @@ Ast_typed.get_t_function f.orig_lambda.type_expression in
      let* func_typed_exp = Michelson_backend.make_function in_ty out_ty f.arg_binder f.body subst_lst in
      let* () = Check.check_obj_ligo func_typed_exp in
      let* func_code = Michelson_backend.compile_value func_typed_exp in
      let* arg_code,_,_ = Michelson_backend.compile_simple_value ~ctxt ~loc ~toplevel:true v in_ty in
      let* input_ty,_ = Ligo_run.Of_michelson.fetch_lambda_types func_code.expr_ty in
      let* options = Michelson_backend.make_options ~param:input_ty (Some ctxt) in
      let* runres = Ligo_run.Of_michelson.run_function ~options func_code.expr func_code.expr_ty arg_code in
      let* (expr_ty,expr) = match runres with | Success x -> ok x | Fail _ -> fail @@ Errors.generic_error loc "Running failed" in
      let expr, expr_ty =
        clean_locations expr, clean_locations expr_ty in
      let ret = LT.V_Michelson (Ty_code (expr, expr_ty, f.body.type_expression)) in
      ok (ret, ctxt)
    | Eval (loc, v, expr_ty) ->
      let* value = Michelson_backend.compile_simple_value ~ctxt ~loc v expr_ty in
      ok (LT.V_Michelson (Ty_code value), ctxt)
    | Compile_contract (loc, v, _ty_expr) ->
       let* compiled_expr, compiled_expr_ty = match v with
         | LT.V_Func_val { arg_binder ; body ; orig_lambda ; env } ->
            let* fv = Self_ast_typed.Helpers.get_fv orig_lambda in
            let* subst_lst = Michelson_backend.make_subst_ast_env_exp ~toplevel:true env fv in
            let* in_ty, out_ty =
              trace_option (Errors.generic_error loc "Trying to run a non-function?") @@
                Ast_typed.get_t_function orig_lambda.type_expression in
            let* compiled_expr =
              Michelson_backend.compile_contract_ subst_lst arg_binder in_ty out_ty body in
            let expr = clean_locations compiled_expr.expr in
            (* TODO-er: check the ignored second component: *)
            let expr_ty = clean_locations compiled_expr.expr_ty in
            ok (expr, expr_ty)
         | _ ->
            fail @@ Errors.generic_error loc "Contract does not reduce to a function value?" in
        let* (param_ty, storage_ty) =
        match Self_michelson.fetch_contract_inputs compiled_expr_ty with
        | Some (param_ty, storage_ty) -> ok (param_ty, storage_ty)
        | _ -> fail @@ Errors.generic_error loc "Compiled expression has not the correct input of contract" in
      let open Tezos_utils in
      let param_ty = clean_locations param_ty in
      let storage_ty = clean_locations storage_ty in
      let expr = clean_locations compiled_expr in
      let contract = Michelson.contract param_ty storage_ty expr in
      ok (LT.V_Michelson (Contract contract), ctxt)
    | To_contract (loc, v, entrypoint, _ty_expr) ->
      begin
        match v with
        | LT.V_Ct (LT.C_address address) ->
           let contract : LT.constant_val =
             LT.C_contract { address ; entrypoint } in
           ok @@ (LT.V_Ct contract, ctxt)
        | _ ->
           fail @@ Errors.generic_error loc
                     "Should be caught by the typer"
      end
    | Check_storage_address (loc, addr, ty) ->
      let* ligo_ty =
        trace_option (Errors.generic_error loc "Not supported (yet) when the provided account has been fetched from Test.get_last_originations" ) @@
          List.Assoc.find ~equal:(Tezos_state.compare_account) ctxt.storage_tys addr in
      let* _,ty = trace_option (Errors.generic_error loc "Argument expected to be a typed_address" ) @@
                    Ast_typed.get_t_typed_address ty in
      let* () = trace_option (Errors.generic_error loc "Storage type does not match expected type") @@
          (Ast_typed.assert_type_expression_eq (ligo_ty, ty)) in
      ok ((), ctxt)
    | Contract_exists (loc, addr) ->
      let* addr = trace_option (corner_case ()) @@ LC.get_address addr in
      let* info = Tezos_state.contract_exists ~loc ctxt addr in
      ok @@ (info, ctxt)
    | Inject_script (loc, code, storage, amt) -> (
      let* contract_code = trace_option (corner_case ()) @@ LC.get_michelson_contract code in
      let* (storage,_,ligo_ty) = trace_option (corner_case ()) @@ LC.get_michelson_expr storage in
      let* (contract, res) = Tezos_state.originate_contract ~loc ctxt contract_code storage amt in
      match res with
      | Tezos_state.Success ctxt ->
        let addr = LT.V_Ct ( C_address contract ) in
        let storage_tys = (contract, ligo_ty) :: (ctxt.storage_tys) in
        ok (addr, {ctxt with storage_tys})
      | Tezos_state.Fail errs -> raise (Exc.Exc (Object_lang_ex (loc,errs)))
    )
    | Mutate_some_value (_loc, z, v, v_type) ->
      let n = Z.to_int z in
      let* expr = Michelson_backend.val_to_ast ~toplevel:true ~loc:Location.generated v v_type in
      let module Fuzzer = Fuzz.Ast_typed.Mutator in
      let ret = Fuzzer.some_mutate_expression ~n expr in
      ok @@ (ret, ctxt)
    | Mutate_all_value (_loc, v, v_type) ->
      let* expr = Michelson_backend.val_to_ast ~toplevel:true ~loc:Location.generated v v_type in
      let module Fuzzer = Fuzz.Ast_typed.Mutator in
      let exprs = Fuzzer.all_mutate_expression expr in
      ok @@ (exprs, ctxt)
    | Mutate_expression (_loc, z, syntax, expr) ->
      let open Ligo_compile in
      let n = Z.to_int z in
      let options = Compiler_options.make () in
      let* meta  = Of_source.make_meta syntax None in
      let* c_unit_exp, _ = Of_source.compile_string ~options ~meta expr in
      let module Gen = Fuzz.Lst in
      let* buffer = match meta.syntax with
        | CameLIGO ->
           begin
             let module Fuzzer = Fuzz.Cameligo.Mutator(Gen) in
             let* raw = trace Main_errors.parser_tracer @@
                          Parsing.Cameligo.parse_expression c_unit_exp in
             let _, mutated_prg = Fuzzer.mutate_expression ~n raw in
             trace Main_errors.pretty_tracer @@
               ok (Parsing.Cameligo.pretty_print_expression mutated_prg)
           end
        | ReasonLIGO ->
           begin
             let module Fuzzer = Fuzz.Reasonligo.Mutator(Gen) in
             let* raw = trace Main_errors.parser_tracer @@
                          Parsing.Reasonligo.parse_expression c_unit_exp in
             let _, mutated_prg = Fuzzer.mutate_expression ~n raw in
             trace Main_errors.pretty_tracer @@
               ok (Parsing.Reasonligo.pretty_print_expression mutated_prg)
           end
        | PascaLIGO ->
           begin
             let module Fuzzer = Fuzz.Pascaligo.Mutator(Gen) in
             let* raw = trace Main_errors.parser_tracer @@
                          Parsing.Pascaligo.parse_expression c_unit_exp in
             let _, mutated_prg = Fuzzer.mutate_expression ~n raw in
             trace Main_errors.pretty_tracer @@
               ok (Parsing.Pascaligo.pretty_print_expression mutated_prg)
           end
        | JsLIGO ->
           begin
             let module Fuzzer = Fuzz.Jsligo.Mutator(Gen) in
             let* raw = trace Main_errors.parser_tracer @@
                          Parsing.Jsligo.parse_expression c_unit_exp in
             let _, mutated_prg = Fuzzer.mutate_expression ~n raw in
             trace Main_errors.pretty_tracer @@
               ok (Parsing.Jsligo.pretty_print_expression mutated_prg)
           end in
      let expr = Buffer.contents buffer in
      ok ((syntax, expr), ctxt)
    | Mutate_count (_loc, syntax, expr) ->
      begin
        let open Ligo_compile in
        let options = Compiler_options.make () in
        let* meta  = Of_source.make_meta syntax None in
        let* c_unit_exp, _ = Of_source.compile_string ~options ~meta expr in
        let module Gen = Fuzz.Lst in
        let* count = match meta.syntax with
          | CameLIGO ->
             begin
               let module Fuzzer = Fuzz.Cameligo.Mutator(Gen) in
               let* raw = trace Main_errors.parser_tracer @@
                            Parsing.Cameligo.parse_expression c_unit_exp in
               let mutated_prgs = Fuzzer.mutate_expression_list raw in
               ok @@ List.length mutated_prgs
             end
          | ReasonLIGO ->
             begin
               let module Fuzzer = Fuzz.Reasonligo.Mutator(Gen) in
               let* raw = trace Main_errors.parser_tracer @@
                            Parsing.Reasonligo.parse_expression c_unit_exp in
               let mutated_prgs = Fuzzer.mutate_expression_list raw in
               ok @@ List.length mutated_prgs
             end
          | PascaLIGO ->
             begin
               let module Fuzzer = Fuzz.Pascaligo.Mutator(Gen) in
               let* raw = trace Main_errors.parser_tracer @@
                            Parsing.Pascaligo.parse_expression c_unit_exp in
               let mutated_prgs = Fuzzer.mutate_expression_list raw in
               ok @@ List.length mutated_prgs
             end
          | JsLIGO ->
             begin
               let module Fuzzer = Fuzz.Jsligo.Mutator(Gen) in
               let* raw = trace Main_errors.parser_tracer @@
                            Parsing.Jsligo.parse_expression c_unit_exp in
               let mutated_prgs = Fuzzer.mutate_expression_list raw in
               ok @@ List.length mutated_prgs
             end in
        ok (LT.V_Ct (C_nat (Z.of_int count)) , ctxt)
      end
    | Set_now (loc, now) ->
      let* ctxt = Tezos_state.set_timestamp ~loc ctxt now in
      ok ((), ctxt)
    | Set_source source ->
      let* source = trace_option (corner_case ()) @@ LC.get_address source in
      ok ((), {ctxt with source })
    | Set_baker baker ->
      let* baker = trace_option (corner_case ()) @@ LC.get_address baker in
      ok ((), {ctxt with baker })
    | Get_bootstrap (loc,x) -> (
      let* x = trace_option (corner_case ()) @@ LC.get_int x in
      match List.nth ctxt.bootstrapped (Z.to_int x) with
      | Some x -> ok (LT.V_Ct (C_address x), ctxt)
      | None -> fail (Errors.generic_error loc "This bootstrap account do not exist")
    )
    | Michelson_equal (loc,a,b) ->
      let* (a,_,_) = trace_option (Errors.generic_error loc "Can't compare contracts") @@
        LC.get_michelson_expr a in
      let* (b,_,_) = trace_option (Errors.generic_error loc "Can't compare contracts") @@
        LC.get_michelson_expr b in
      ok ((a=b), ctxt)
    | Get_last_originations () ->
      let aux (src, lst) =
        let src = LC.v_address src in
        let lst = LT.V_List (List.map ~f:LC.v_address lst) in
        (src, lst)
      in
      let v = LT.V_Map (List.map ~f:aux ctxt.last_originations) in
      ok (v,ctxt)
    | Int_compare_wrapped (x, y) ->
      ok (wrap_compare Int_repr.compare x y, ctxt)
    | Int_compare (x, y) -> ok (Int_repr.compare x y, ctxt)
    | Int_abs z -> ok (Int_repr.abs z, ctxt)
    | Int_of_int i -> ok (Int_repr.of_int i, ctxt)
    | Int_of_zint z -> ok (Int_repr.of_zint z, ctxt)
    | Int_to_zint z -> ok (Int_repr.to_zint z, ctxt)
    | Int_of_int64 i -> ok (Int_repr.of_int64 i, ctxt)
    | Int_to_int64 i -> ok (Int_repr.to_int64 i, ctxt)
    | Int_is_nat z -> ok (Int_repr.is_nat z, ctxt)
    | Int_neg n -> ok (Int_repr.neg n, ctxt)
    | Int_add (x, y) -> ok (Int_repr.add x y, ctxt)
    | Int_add_n (x, y) -> ok (Int_repr.add_n x y, ctxt)
    | Int_mul (x, y) -> ok (Int_repr.mul x y, ctxt)
    | Int_mul_n (x, y) -> ok (Int_repr.mul_n x y, ctxt)
    | Int_ediv (x, y) -> ok (Int_repr.ediv x y, ctxt)
    | Int_ediv_n (x, y) -> ok (Int_repr.ediv_n x y, ctxt)
    | Int_sub (x, y) -> ok (Int_repr.sub x y, ctxt)
    | Int_shift_left (x, y) -> ok (Int_repr.shift_left x y, ctxt)
    | Int_shift_right (x, y) -> ok (Int_repr.shift_right x y, ctxt)
    | Int_logor (x, y) -> ok (Int_repr.logor x y, ctxt)
    | Int_logand (x, y) -> ok (Int_repr.logand x y, ctxt)
    | Int_logxor (x, y) -> ok (Int_repr.logxor x y, ctxt)
    | Int_lognot n -> ok (Int_repr.lognot n, ctxt)
    | Int_int n -> ok (Int_repr.int n, ctxt)
end

type 'a t =
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Call : 'a Command.t -> 'a t
  | Return : 'a -> 'a t
  | Fail_ligo : Errors.interpreter_error -> 'a t
  | Try_catch : 'a t * (Ligo_interpreter.Types.exception_type -> 'a t) -> 'a t

let rec eval
  : type a.
    a t ->
    Tezos_state.context ->
    execution_trace ref option ->
    (a * Tezos_state.context) result_monad
  = fun e ctxt log ->
  match e with
  | Bind (e', f) ->
    let>>= (v, ctxt) = eval e' ctxt log in
    eval (f v) ctxt log
  | Call command -> Command.eval command ctxt log
  | Return v -> ok (v, ctxt)
  | Fail_ligo err -> fail err
  | Try_catch (e', handler) ->
    match Trace.to_stdlib_result (eval e' ctxt log) with
    | Ok r -> ok r
    | Error (`Main_interpret_target_lang_error (loc, e)) ->
       eval (handler (LT.Object_lang_ex (loc, e))) ctxt log
    | Error (`Main_interpret_meta_lang_eval (loc, s)) ->
       eval (handler (LT.Meta_lang_ex {location = loc; reason = Reason s})) ctxt log
    | Error (`Main_interpret_meta_lang_failwith (loc, v)) ->
       eval (handler (LT.Meta_lang_ex {location = loc; reason = Val v})) ctxt log
    | Error _ ->
       failwith "Interpreter error not handled"
    | exception Exc.Exc exc ->
       eval (handler exc) ctxt log

let fail err : 'a t = Fail_ligo err
let return (x: 'a) : 'a t = Return x
let call (command : 'a Command.t) : 'a t = Call command
let try_catch (c : 'a t) (handler : Ligo_interpreter.Types.exception_type -> 'a t) : 'a t = Try_catch (c, handler)
let ( let>> ) o f = Bind (call o, f)
let ( let* ) o f = Bind (o, f)

let rec bind_list = function
  | [] -> return []
  | hd::tl ->
    let* hd = hd in
    let* tl = bind_list tl in
    return @@ hd :: tl

let bind_map_list f lst = bind_list (List.map ~f:f lst)

let bind_fold_list f init lst =
  let aux x y =
    let* x = x in
    f x y
  in
  List.fold_left ~f:aux ~init:(return init) lst

let rec iter_while f lst =
  match lst with
  | [] ->
     return None
  | (x :: xs) ->
     let* b = f x in
     match b with
     | None ->
        iter_while f xs
     | Some x ->
        return (Some x)
