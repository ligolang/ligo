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

let corner_case ?(loc = Location.generated) () = Errors.generic_error loc "Corner case, please report to devs."
let add_warning _ = ()

let wrap_compare compare a b =
  let res = compare a b in
  if (res = 0) then 0 else if (res > 0) then 1 else -1

let clean_locations ty = Tezos_micheline.Micheline.inject_locations (fun _ -> ()) (Tezos_micheline.Micheline.strip_locations ty)

module Command = struct
  type 'a t =
    | Get_big_map : Location.t * Ligo_interpreter.Types.calltrace * LT.type_expression * LT.type_expression * LT.value * Z.t -> LT.expression t
    | Mem_big_map : Location.t * Ligo_interpreter.Types.calltrace * LT.type_expression * LT.type_expression * LT.value * Z.t -> bool t
    | Bootstrap_contract : int * LT.value * LT.value * Ast_typed.type_expression  -> unit t
    | Nth_bootstrap_contract : int -> Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.t t
    | Nth_bootstrap_typed_address : Location.t * int -> (Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.t * Ast_typed.type_expression * Ast_typed.type_expression) t
    | Reset_state : Location.t * LT.calltrace * LT.value * LT.value -> unit t
    | External_call : Location.t * Ligo_interpreter.Types.calltrace * LT.contract * (execution_trace, string) Tezos_micheline.Micheline.node * Z.t -> Tezos_state.state_error option t
    | State_error_to_value : Tezos_state.state_error -> LT.value t
    | Get_storage : Location.t * Ligo_interpreter.Types.calltrace * LT.value * Ast_typed.type_expression -> Ast_typed.expression t
    | Get_storage_of_address : Location.t * Ligo_interpreter.Types.calltrace * LT.value -> LT.value t
    | Get_size : LT.value -> LT.value t
    | Get_balance : Location.t * Ligo_interpreter.Types.calltrace * LT.value -> LT.value t
    | Get_last_originations : unit -> LT.value t
    | Check_obj_ligo : LT.expression -> unit t
    | Compile_contract_from_file : string * string -> (LT.value * LT.value) t
    | Compile_meta_value : Location.t * LT.value * Ast_typed.type_expression -> LT.value t
    | Run : Location.t * LT.func_val * LT.value -> LT.value t
    | Eval : Location.t * LT.value * Ast_typed.type_expression -> LT.value t
    | Compile_contract : Location.t * LT.value * Ast_typed.type_expression -> LT.value t
    | To_contract : Location.t * LT.value * string option * Ast_typed.type_expression -> LT.value t
    | Check_storage_address : Location.t * Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.t * Ast_typed.type_expression -> unit t
    | Contract_exists : Location.t * LT.calltrace * LT.value -> bool t
    | Inject_script : Location.t * Ligo_interpreter.Types.calltrace * LT.value * LT.value * Z.t -> LT.value t
    | Set_now : Location.t * Ligo_interpreter.Types.calltrace * Z.t -> unit t
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
      raise:Errors.interpreter_error raise ->
      a t ->
      Tezos_state.context ->
      execution_trace ref option ->
      (a * Tezos_state.context)
    = fun ~raise command ctxt _log ->
    match command with
    | Get_big_map (loc, calltrace, k_ty, v_ty, k, m) ->
      (* TODO-er: hack to get the micheline type... *)
      let none_compiled = Michelson_backend.compile_value ~raise (Ast_typed.e_a_none v_ty) in
      let val_ty = clean_locations none_compiled.expr_ty in
      let inner_ty = match val_ty with
        | Prim (_, "option", [l], _) ->
           l
        | _ -> failwith "None has a non-option type?" in
      let key,key_ty,_ = Michelson_backend.compile_simple_value ~raise ~ctxt ~loc k k_ty in
      let storage' = Tezos_state.get_big_map ~raise ~loc ~calltrace ctxt m key key_ty in
      begin
        match storage' with
        | Some storage' ->
           let code = storage'
                      |> Tezos_protocol_009_PsFLoren.Protocol.Michelson_v1_primitives.strings_of_prims
                      |> Tezos_micheline.Micheline.inject_locations (fun _ -> ()) in
           let mini_c = trace ~raise Main_errors.decompile_michelson @@ Stacking.Decompiler.decompile_value inner_ty code in
           let typed = trace ~raise Main_errors.decompile_mini_c @@ Spilling.decompile mini_c v_ty in
           let typed = Ast_typed.e_a_some typed in
           (typed, ctxt)
        | None -> (Ast_typed.e_a_none v_ty, ctxt)
      end
    | Mem_big_map (loc, calltrace, k_ty, _v_ty, k, m) ->
      let key,key_ty,_ = Michelson_backend.compile_simple_value ~raise ~ctxt ~loc k k_ty in
      let storage' = Tezos_state.get_big_map ~raise ~loc ~calltrace ctxt m key key_ty in
      (Option.is_some storage', ctxt)
    | Nth_bootstrap_contract (n) ->
      let contract = Tezos_state.get_bootstrapped_contract ~raise n in
      (contract,ctxt)
    | Nth_bootstrap_typed_address (loc, n) ->
      let contract = Tezos_state.get_bootstrapped_contract ~raise n in
      let storage_ty =
        trace_option ~raise (Errors.generic_error loc "Storage type not available" ) @@
          List.Assoc.find ~equal:(Tezos_state.compare_account) ctxt.storage_tys contract in
      let parameter_ty =
        trace_option ~raise (Errors.generic_error loc "Parameter type not available" ) @@
          List.Assoc.find ~equal:(Tezos_state.compare_account) ctxt.parameter_tys contract in
      let contract = Tezos_state.get_bootstrapped_contract ~raise n in
      ((contract, parameter_ty, storage_ty),ctxt)
    | Bootstrap_contract (mutez, contract, storage, contract_ty) ->
      let contract = trace_option ~raise (corner_case ()) @@ LC.get_michelson_contract contract in
      let input_ty, _ = trace_option ~raise (corner_case ()) @@ Ast_typed.get_t_function contract_ty in
      let parameter_ty, _ = trace_option ~raise (corner_case ()) @@ Ast_typed.get_t_pair input_ty in
      let (storage,_,storage_ty) = trace_option ~raise (corner_case ()) @@ LC.get_michelson_expr storage in
      let ctxt =
        { ctxt with next_bootstrapped_contracts = (mutez, contract, storage, parameter_ty, storage_ty) :: ctxt.next_bootstrapped_contracts } in
      ((),ctxt)
    | Reset_state (loc,calltrace,n,amts) ->
      let amts = trace_option ~raise (corner_case ()) @@ LC.get_list amts in
      let amts = List.map ~f:
        (fun x ->
          let x = trace_option ~raise (corner_case ()) @@ LC.get_nat x in
          (Z.to_int64 x) )
        amts
      in
      let n = trace_option ~raise (corner_case ()) @@ LC.get_nat n in
      let ctxt = Tezos_state.init_ctxt ~raise ~loc ~calltrace ~initial_balances:amts ~n:(Z.to_int n) (List.rev ctxt.next_bootstrapped_contracts) in
      ((),ctxt)
    | External_call (loc, calltrace, { address; entrypoint }, param, amt) -> (
      let x = Tezos_state.transfer ~raise ~loc ~calltrace ctxt address ?entrypoint param amt in
      match x with
      | Success ctxt -> (None, ctxt)
      | Fail errs -> (Some errs, ctxt)
    )
    | State_error_to_value errs -> (
      match Tezos_state.get_contract_rejection_data errs with
      | Some (addr,v) ->
        let t = Michelson_backend.storage_retreival_dummy_ty in
        let v = LT.V_Michelson (Ty_code (v, t, Ast_typed.t_int ())) in
        let addr = LT.V_Ct (C_address addr) in
        let err = LC.v_ctor "Rejected" (LC.v_pair (v,addr)) in
        (LC.v_ctor "Fail" err, ctxt)
      | None ->
        (LC.v_ctor "Fail" (LC.v_ctor "Other" (LC.v_unit ())), ctxt)
    )
    | Get_storage (loc, calltrace, addr, ty_expr) ->
      let addr = trace_option ~raise (corner_case ()) @@ LC.get_address addr in
      let (storage',ty) = Tezos_state.get_storage ~raise ~loc ~calltrace ctxt addr in
      let storage = storage'
        |> Tezos_protocol_009_PsFLoren.Protocol.Michelson_v1_primitives.strings_of_prims
        |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())
      in
      let ret = LT.V_Michelson (Ty_code (storage,ty,ty_expr)) in
      let ret = Michelson_backend.val_to_ast ~raise ~loc ret ty_expr in
      (ret, ctxt)
    | Get_balance (loc, calltrace, addr) ->
      let addr = trace_option ~raise (corner_case ()) @@ LC.get_address addr in
      let balance = Tezos_state.get_balance ~raise ~loc ~calltrace ctxt addr in
      let mutez = Michelson_backend.int_of_mutez balance in
      let balance = LT.V_Ct (C_mutez mutez) in
      (balance, ctxt)
    | Get_storage_of_address (loc, calltrace, addr) ->
      let addr = trace_option ~raise (corner_case ()) @@ LC.get_address addr in
      let (storage',ty) = Tezos_state.get_storage ~raise ~loc ~calltrace ctxt addr in
      let storage = storage'
        |> Tezos_protocol_009_PsFLoren.Protocol.Michelson_v1_primitives.strings_of_prims
        |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())
      in
      let ligo_ty =
        trace_option ~raise (Errors.generic_error loc "Not supported (yet) when the provided account has been fetched from Test.get_last_originations" ) @@
          List.Assoc.find ~equal:(Tezos_state.compare_account) ctxt.storage_tys addr
      in
      let ret = LT.V_Michelson (Ty_code (storage,ty,ligo_ty)) in
      (ret, ctxt)
    | Check_obj_ligo e ->
      let _ = trace ~raise Main_errors.self_ast_typed_tracer @@ Self_ast_typed.expression_obj e in
      ((), ctxt)
    | Compile_meta_value (loc,x,ty) ->
      let x = Michelson_backend.compile_simple_value ~raise ~ctxt ~loc x ty in
      (LT.V_Michelson (LT.Ty_code x), ctxt)
    | Get_size (contract_code) ->
       begin
         match contract_code with
         | LT.V_Michelson (LT.Contract contract_code) ->
            let s = Ligo_compile.Of_michelson.measure ~raise contract_code in
            (LT.V_Ct (C_int (Z.of_int s)), ctxt)
         | _ -> raise.raise @@ Errors.generic_error Location.generated
                          "Trying to measure a non-contract"
       end
    | Compile_contract_from_file (source_file, entrypoint) ->
      let contract_code =
        Michelson_backend.compile_contract ~raise ~add_warning source_file entrypoint in
      let size =
        let s = Ligo_compile.Of_michelson.measure ~raise contract_code in
        LT.V_Ct (C_int (Z.of_int s))
      in
      let contract = LT.V_Michelson (LT.Contract contract_code) in
      ((contract,size), ctxt)
    | Run (loc, f, v) ->
      let open Ligo_interpreter.Types in
      let fv = Self_ast_typed.Helpers.Free_variables.expression f.orig_lambda in
      let subst_lst = Michelson_backend.make_subst_ast_env_exp ~raise ~toplevel:true f.env fv in
      let in_ty, out_ty = trace_option ~raise (Errors.generic_error loc "Trying to run a non-function?") @@ Ast_typed.get_t_function f.orig_lambda.type_expression in
      let func_typed_exp = Michelson_backend.make_function in_ty out_ty f.arg_binder f.body subst_lst in
      let _ = trace ~raise Main_errors.self_ast_typed_tracer @@ Self_ast_typed.expression_obj func_typed_exp in
      let func_code = Michelson_backend.compile_value ~raise func_typed_exp in
      let arg_code,_,_ = Michelson_backend.compile_simple_value ~raise ~ctxt ~loc ~toplevel:true v in_ty in
      let input_ty,_ = Ligo_run.Of_michelson.fetch_lambda_types ~raise func_code.expr_ty in
      let options = Michelson_backend.make_options ~raise ~param:input_ty (Some ctxt) in
      let runres = Ligo_run.Of_michelson.run_function ~raise ~options func_code.expr func_code.expr_ty arg_code in
      let (expr_ty,expr) = match runres with | Success x -> x | Fail x -> raise.raise @@ Errors.target_lang_failwith loc x in
      let expr, expr_ty =
        clean_locations expr, clean_locations expr_ty in
      let ret = LT.V_Michelson (Ty_code (expr, expr_ty, f.body.type_expression)) in
      (ret, ctxt)
    | Eval (loc, v, expr_ty) ->
      let value = Michelson_backend.compile_simple_value ~raise ~ctxt ~loc v expr_ty in
      (LT.V_Michelson (Ty_code value), ctxt)
    | Compile_contract (loc, v, _ty_expr) ->
       let compiled_expr, compiled_expr_ty = match v with
         | LT.V_Func_val { arg_binder ; body ; orig_lambda ; env ; rec_name } ->
            let fv = Self_ast_typed.Helpers.Free_variables.expression orig_lambda in
            let subst_lst = Michelson_backend.make_subst_ast_env_exp ~raise ~toplevel:true env fv in
            let in_ty, out_ty =
              trace_option ~raise (Errors.generic_error loc "Trying to run a non-function?") @@
                Ast_typed.get_t_function orig_lambda.type_expression in
            let compiled_expr =
              Michelson_backend.compile_contract_ ~raise subst_lst arg_binder rec_name in_ty out_ty body in
            let expr = clean_locations compiled_expr.expr in
            (* TODO-er: check the ignored second component: *)
            let expr_ty = clean_locations compiled_expr.expr_ty in
            (expr, expr_ty)
         | _ ->
            raise.raise @@ Errors.generic_error loc "Contract does not reduce to a function value?" in
        let (param_ty, storage_ty) =
        match Self_michelson.fetch_contract_inputs compiled_expr_ty with
        | Some (param_ty, storage_ty) -> (param_ty, storage_ty)
        | _ -> raise.raise @@ Errors.generic_error loc "Compiled expression has not the correct input of contract" in
      let open Tezos_utils in
      let param_ty = clean_locations param_ty in
      let storage_ty = clean_locations storage_ty in
      let expr = clean_locations compiled_expr in
      let contract = Michelson.contract param_ty storage_ty expr in
      (LT.V_Michelson (Contract contract), ctxt)
    | To_contract (loc, v, entrypoint, _ty_expr) ->
      begin
        match v with
        | LT.V_Ct (LT.C_address address) ->
           let contract : LT.constant_val =
             LT.C_contract { address ; entrypoint } in
           (LT.V_Ct contract, ctxt)
        | _ ->
           raise.raise @@ Errors.generic_error loc
                     "Should be caught by the typer"
      end
    | Check_storage_address (loc, addr, ty) ->
      let ligo_ty =
        trace_option ~raise (Errors.generic_error loc "Not supported (yet) when the provided account has been fetched from Test.get_last_originations" ) @@
          List.Assoc.find ~equal:(Tezos_state.compare_account) ctxt.storage_tys addr in
      let _,ty = trace_option ~raise (Errors.generic_error loc "Argument expected to be a typed_address" ) @@
                    Ast_typed.get_t_typed_address ty in
      let () = trace_option ~raise (Errors.generic_error loc "Storage type does not match expected type") @@
          (Ast_typed.assert_type_expression_eq (ligo_ty, ty)) in
      ((), ctxt)
    | Contract_exists (loc, calltrace, addr) ->
      let addr = trace_option ~raise (corner_case ()) @@ LC.get_address addr in
      let info = Tezos_state.contract_exists ~raise ~loc ~calltrace ctxt addr in
      (info, ctxt)
    | Inject_script (loc, calltrace, code, storage, amt) -> (
      let contract_code = trace_option ~raise (corner_case ()) @@ LC.get_michelson_contract code in
      let (storage,_,ligo_ty) = trace_option ~raise (corner_case ()) @@ LC.get_michelson_expr storage in
      let (contract, res) = Tezos_state.originate_contract ~raise ~loc ~calltrace ctxt contract_code storage amt in
      match res with
      | Tezos_state.Success ctxt ->
        let addr = LT.V_Ct ( C_address contract ) in
        let storage_tys = (contract, ligo_ty) :: (ctxt.storage_tys) in
        (addr, {ctxt with storage_tys})
      | Tezos_state.Fail errs -> raise.raise (Errors.target_lang_error loc calltrace errs)
    )
    | Set_now (loc, calltrace, now) ->
      let ctxt = Tezos_state.set_timestamp ~raise ~loc ~calltrace ctxt now in
      ((), ctxt)
    | Set_source source ->
      let source = trace_option ~raise (corner_case ()) @@ LC.get_address source in
      ((), {ctxt with source })
    | Set_baker baker ->
      let baker = trace_option ~raise (corner_case ()) @@ LC.get_address baker in
      ((), {ctxt with baker })
    | Get_bootstrap (loc,x) -> (
      let x = trace_option ~raise (corner_case ()) @@ LC.get_int x in
      match List.nth ctxt.bootstrapped (Z.to_int x) with
      | Some x -> (LT.V_Ct (C_address x), ctxt)
      | None -> raise.raise (Errors.generic_error loc "This bootstrap account do not exist")
    )
    | Michelson_equal (loc,a,b) ->
      let (a,_,_) = trace_option ~raise (Errors.generic_error loc "Can't compare contracts") @@
        LC.get_michelson_expr a in
      let (b,_,_) = trace_option ~raise (Errors.generic_error loc "Can't compare contracts") @@
        LC.get_michelson_expr b in
      ((a=b), ctxt)
    | Get_last_originations () ->
      let aux (src, lst) =
        let src = LC.v_address src in
        let lst = LT.V_List (List.map ~f:LC.v_address lst) in
        (src, lst)
      in
      let v = LT.V_Map (List.map ~f:aux ctxt.last_originations) in
      (v,ctxt)
    | Int_compare_wrapped (x, y) ->
      (wrap_compare Int_repr.compare x y, ctxt)
    | Int_compare (x, y) -> (Int_repr.compare x y, ctxt)
    | Int_abs z -> (Int_repr.abs z, ctxt)
    | Int_of_int i -> (Int_repr.of_int i, ctxt)
    | Int_of_zint z -> (Int_repr.of_zint z, ctxt)
    | Int_to_zint z -> (Int_repr.to_zint z, ctxt)
    | Int_of_int64 i -> (Int_repr.of_int64 i, ctxt)
    | Int_to_int64 i -> (Int_repr.to_int64 i, ctxt)
    | Int_is_nat z -> (Int_repr.is_nat z, ctxt)
    | Int_neg n -> (Int_repr.neg n, ctxt)
    | Int_add (x, y) -> (Int_repr.add x y, ctxt)
    | Int_add_n (x, y) -> (Int_repr.add_n x y, ctxt)
    | Int_mul (x, y) -> (Int_repr.mul x y, ctxt)
    | Int_mul_n (x, y) -> (Int_repr.mul_n x y, ctxt)
    | Int_ediv (x, y) -> (Int_repr.ediv x y, ctxt)
    | Int_ediv_n (x, y) -> (Int_repr.ediv_n x y, ctxt)
    | Int_sub (x, y) -> (Int_repr.sub x y, ctxt)
    | Int_shift_left (x, y) -> (Int_repr.shift_left x y, ctxt)
    | Int_shift_right (x, y) -> (Int_repr.shift_right x y, ctxt)
    | Int_logor (x, y) -> (Int_repr.logor x y, ctxt)
    | Int_logand (x, y) -> (Int_repr.logand x y, ctxt)
    | Int_logxor (x, y) -> (Int_repr.logxor x y, ctxt)
    | Int_lognot n -> (Int_repr.lognot n, ctxt)
    | Int_int n -> (Int_repr.int n, ctxt)
end

type 'a t =
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Call : 'a Command.t -> 'a t
  | Return : 'a -> 'a t
  | Fail_ligo : Errors.interpreter_error -> 'a t
  | Try_or : 'a t * 'a t -> 'a t

let rec eval
  : type a.
    raise:Errors.interpreter_error raise ->
    a t ->
    Tezos_state.context ->
    execution_trace ref option ->
    a * Tezos_state.context
  = fun ~raise e ctxt log ->
  match e with
  | Bind (e', f) ->
    let (v, ctxt) = eval ~raise e' ctxt log in
    eval ~raise (f v) ctxt log
  | Call command -> Command.eval ~raise command ctxt log
  | Return v -> (v, ctxt)
  | Fail_ligo err -> raise.raise err
  | Try_or (e', handler) ->
    try_with
      (eval e' ctxt log)
      (function
            `Main_interpret_target_lang_error _
          | `Main_interpret_target_lang_failwith _
          | `Main_interpret_meta_lang_eval _
          | `Main_interpret_meta_lang_failwith _ ->
            eval ~raise handler ctxt log
          | e -> raise.raise e)

let fail err : 'a t = Fail_ligo err
let return (x: 'a) : 'a t = Return x
let call (command : 'a Command.t) : 'a t = Call command
let try_or (c : 'a t) (handler : 'a t) : 'a t = Try_or (c, handler)
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
