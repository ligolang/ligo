open Trace
open Ligo_interpreter.Types
open Ligo_interpreter.Combinators
include Ast_typed.Types

module Errors = Errors
type interpreter_error = Errors.interpreter_error
module Env = Ligo_interpreter.Environment

let apply_comparison : Ast_typed.constant' -> value list -> value Monad.t =
  fun c operands ->
    let open Monad in
    match (c,operands) with
    | ( comp , [ V_Ct (C_int a'      ) ; V_Ct (C_int b'      ) ] )
    | ( comp , [ V_Ct (C_timestamp a') ; V_Ct (C_timestamp b') ] )
    | ( comp , [ V_Ct (C_nat a'      ) ; V_Ct (C_nat b'      ) ] ) ->
      let>> i = Int_compare_wrapped (a', b') in
      let>> cmpres = Int_of_int i in
      let>> cmpres = Int_compare (cmpres, Ligo_interpreter.Int_repr_copied.zero) in
      let x = match comp with
        (*TODO those Int.(XX) ... should be added as a command in the monad and replaced by
          int_repr_copied.Compare.(XX)*)
        | C_EQ -> (cmpres = 0)
        | C_NEQ -> (cmpres <> 0)
        | C_LT -> (cmpres < 0)
        | C_LE -> (cmpres <= 0)
        | C_GT -> (cmpres > 0)
        | C_GE -> (cmpres >= 0)
        | _ -> failwith "apply compare must be called with a comparative constant"
      in
      return @@ v_bool x
    | ( comp , [ V_Ct (C_mutez a'    ) ; V_Ct (C_mutez b'    ) ] ) ->
      let>> i = Tez_compare_wrapped (a', b') in
      let>> cmpres = Int_of_int i in
      let>> cmpres = Int_compare (cmpres, Ligo_interpreter.Int_repr_copied.zero) in
      let x = match comp with
        (*TODO those Int.(XX) ... should be added as a command in the monad and replaced by
          int_repr_copied.Compare.(XX)*)
        | C_EQ -> (cmpres = 0)
        | C_NEQ -> (cmpres <> 0)
        | C_LT -> (cmpres < 0)
        | C_LE -> (cmpres <= 0)
        | C_GT -> (cmpres > 0)
        | C_GE -> (cmpres >= 0)
        | _ -> failwith "apply compare must be called with a comparative constant"
      in
      return @@ v_bool x
    | ( comp     , [ V_Ct (C_string a'  ) ; V_Ct (C_string b'  ) ] )
    | ( comp     , [ V_Ct (C_address a' ) ; V_Ct (C_address b' ) ] )
    | ( comp     , [ V_Ct (C_key_hash a') ; V_Ct (C_key_hash b') ] ) ->
    (* TODO : monad, allign with Michelson *)
      let f_op = match comp with
        | C_EQ -> fun a b -> (String.compare a b = 0)
        | C_NEQ -> fun a b -> (String.compare a b != 0)
        (* the above might not be alligned with Michelson interpreter. Do we care ? *)
        | C_LT -> fun a b -> (String.compare a b < 0)
        | C_LE -> fun a b -> (String.compare a b <= 0)
        | C_GT -> fun a b -> (String.compare a b > 0)
        | C_GE -> fun a b -> (String.compare a b >= 0)
        | _ -> failwith "apply compare must be called with a comparative constant" in
      Monad.return @@ v_bool (f_op a' b')

    | ( comp     , [ V_Ct (C_bytes a'  ) ; V_Ct (C_bytes b'  ) ] ) ->
    (* TODO : monad, allign with Michelson *)
      let f_op = match comp with
        | C_EQ -> fun a b -> (Bytes.compare a b = 0)
        | C_NEQ -> fun a b -> (Bytes.compare a b != 0)
        (* the above might not be alligned with Michelson interpreter. Do we care ? *)
        | C_LT -> fun a b -> (Bytes.compare a b < 0)
        | C_LE -> fun a b -> (Bytes.compare a b <= 0)
        | C_GT -> fun a b -> (Bytes.compare a b > 0)
        | C_GE -> fun a b -> (Bytes.compare a b >= 0)
        | _ -> failwith "apply compare must be called with a comparative constant" in
      Monad.return @@ v_bool (f_op a' b')
    | _ ->
      let () = List.iter (fun el -> Format.printf "%a" Ligo_interpreter.PP.pp_value el) operands in
      failwith "unsupported comparison"

let rec apply_operator : Location.t -> Ast_typed.constant' -> value list -> value Monad.t =
  fun loc c operands ->
  let open Monad in
  let return_ct v = return @@ V_Ct v in
  let return_none () = return @@ v_none () in
  let return_some v  = return @@ v_some v in
  ( match (c,operands) with
    (* nullary *)
    | ( C_NONE , [] ) -> return_none ()
    | ( C_UNIT , [] ) -> return @@ V_Ct C_unit
    | ( C_NIL  , [] ) -> return @@ V_List []
    (* unary *)
    | ( C_FAILWITH , [ V_Ct ct ] ) ->
      let s = Format.asprintf "%a" Ligo_interpreter.PP.pp_ct ct in
      Errors.contract_failure s
    | ( C_SIZE   , [(V_Set l | V_List l)] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Map l            ] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Ct (C_string s ) ] ) -> return_ct @@ C_nat (Z.of_int @@ String.length s)
    | ( C_SIZE   , [ V_Ct (C_bytes b  ) ] ) -> return_ct @@ C_nat (Z.of_int @@ Bytes.length b)
    | ( C_NOT    , [ V_Ct (C_bool a'  ) ] ) -> return_ct @@ C_bool (not a')
    | ( C_INT    , [ V_Ct (C_nat a')    ] ) -> return_ct @@ C_int a'
    | ( C_ABS    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_nat (Z.abs a')
    | ( C_NEG    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (Z.neg a')
    | ( C_SOME   , [ v                  ] ) -> return_some v
    | ( C_IS_NAT , [ V_Ct (C_int a')    ] ) ->
      if a' > Z.zero then return_some @@ V_Ct (C_nat a')
      else return_none ()
    | ( C_FOLD_CONTINUE  , [ v ] ) -> return @@ v_pair (v_bool true  , v)
    | ( C_FOLD_STOP      , [ v ] ) -> return @@ v_pair (v_bool false , v)
    | ( C_ASSERTION , [ v ] ) ->
      if (is_true v) then return_ct @@ C_unit
      else Errors.contract_failure "failed assertion"
    | C_MAP_FIND_OPT , [ k ; V_Map l ] -> ( match List.assoc_opt k l with
      | Some v -> return @@ v_some v
      | None -> return @@ v_none ()
    )
    | C_MAP_FIND , [ k ; V_Map l ] -> ( match List.assoc_opt k l with
      | Some v -> return @@ v
      | None -> Errors.contract_failure "failed map find"
    )
    (* binary *)
    | ( (C_EQ | C_NEQ | C_LT | C_LE | C_GT | C_GE) , _ ) -> apply_comparison c operands
    | ( C_SUB    , [ V_Ct (C_int a' | C_nat a') ; V_Ct (C_int b' | C_nat b') ] ) -> return_ct @@ C_int (Z.sub a' b')
    | ( C_CONS   , [ v                  ; V_List vl          ] ) -> return @@ V_List (v::vl)
    | ( C_ADD    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_ADD    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_ADD    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] ) -> let>> r = Int_add (a',b') in return_ct (C_int r)
    | ( C_ADD    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> let>> r = Int_add_n (a',b') in return_ct (C_nat r)
    | ( C_MUL    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_MUL    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] ) -> let>> r = Int_mul (a',b') in return_ct (C_int r)
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> let>> r = Int_mul_n (a',b') in return_ct (C_nat r)
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_mutez b')  ] ) ->
      let>> a' = Int_to_int64 a' in
      begin
        match a' with
        | None -> call Fail_overflow
        | Some a' ->
          let res = Tez.(b' *? a') in
          let>> res = Lift_tz_result res in
          return_ct (C_mutez res)
      end
    | ( C_MUL    , [ V_Ct (C_mutez a')  ; V_Ct (C_nat b')  ] ) ->
      let>> b' = Int_to_int64 b' in
      begin
        match b' with
        | None -> call Fail_overflow
        | Some b' ->
          let res = Tez.(a' *? b') in
          let>> res = Lift_tz_result res in
          return_ct (C_mutez res)
      end
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] ) ->
      let>> a = Int_ediv (a',b') in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_int res
        | None -> Errors.contract_failure "div/0"
      end
    | ( C_DIV    , [ V_Ct (C_nat a')  ; V_Ct (C_nat b')  ] ) ->
      let>> a = Int_ediv_n (a',b') in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_nat res
        | None -> Errors.contract_failure "div/0"
      end
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_nat b'  )  ] ) ->
      let>> a' = Int_of_int64 (Tez.to_mutez a') in
      let>> res = Int_ediv (a', b') in
      begin
        match res with
        | None -> Errors.contract_failure "div/0"
        | Some (q, _r) ->
            let>> q' = Int_to_int64 q in
              match q' with
              | Some q ->
                  begin
                    match Tez.of_mutez q with
                    | Some q -> return_ct @@ C_mutez q
                    (* Cannot overflow *)
                    | _ -> Errors.contract_failure "div/0"
                  end
              (* Cannot overflow *)
              | _ -> Errors.contract_failure "div/0"
      end
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_mutez b')  ] ) ->
      let abs : Tez.t -> _ Monad.t = fun x ->
        let>> x' = Int_of_int64 (Tez.to_mutez x) in
        let>> x' = Int_abs x' in
        return x'
      in
      let* a' = abs a' in
      let* b' = abs b' in
      let>> div = Int_ediv_n (a', b') in
      begin
        match div with
            | None -> Errors.contract_failure "div/0"
            | Some (q, _r) -> return_ct @@ (C_nat q)
      end
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_int b')    ] )
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_nat b')    ] )
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_int b')    ] ) ->
      let>> a = Int_ediv (a',b') in
      begin
        match a with
        | Some (_,r) -> return_ct @@ C_nat r
        | None -> Errors.contract_failure "div/0"
      end
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_nat b')    ] ) ->
      let>> a = Int_ediv_n (a',b') in
      begin
        match a with
        | Some (_,r) -> return_ct @@ C_nat r
        | None -> Errors.contract_failure "div/0"
      end
    | ( C_CONCAT , [ V_Ct (C_string a') ; V_Ct (C_string b') ] ) -> return_ct @@ C_string (a' ^ b')
    | ( C_CONCAT , [ V_Ct (C_bytes a' ) ; V_Ct (C_bytes b' ) ] ) -> return_ct @@ C_bytes  (Bytes.cat a' b')
    | ( C_OR     , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' || b')
    | ( C_AND    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' && b')
    | ( C_XOR    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   ( (a' || b') && (not (a' && b')) )
    | ( C_LIST_EMPTY, []) -> return @@ V_List ([])
    | ( C_LIST_MAP , [ V_Func_val {arg_binder ; body ; env}  ; V_List (elts) ] ) ->
      let* elts =
        Monad.bind_map_list
          (fun elt ->
            let env' = Env.extend env (arg_binder,elt) in
            eval_ligo body env')
          elts
      in
      return (V_List elts)
    | ( C_MAP_MAP , [ V_Func_val {arg_binder ; body ; env}  ; V_Map (elts) ] ) ->
      let* elts =
        Monad.bind_map_list
          (fun (k,v) ->
            let env' = Env.extend env (arg_binder,v_pair (k,v)) in
            let* v' = eval_ligo body env' in
            return @@ (k,v')
          )
          elts
      in
      return (V_Map elts)
    | ( C_LIST_ITER , [ V_Func_val {arg_binder ; body ; env}  ; V_List (elts) ] ) ->
      Monad.bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env (arg_binder,elt) in
          eval_ligo body env'
        )
        (V_Ct C_unit) elts
    | ( C_MAP_ITER , [ V_Func_val {arg_binder ; body ; env}  ; V_Map (elts) ] ) ->
      Monad.bind_fold_list
        (fun _ kv ->
          let env' = Env.extend env (arg_binder,v_pair kv) in
          eval_ligo body env'
        )
        (V_Ct C_unit) elts
    | ( C_FOLD_WHILE , [ V_Func_val {arg_binder ; body ; env}  ; init ] ) -> (
      let rec aux b el =
        let env' = Env.extend env (arg_binder, el) in
        let* res = eval_ligo body env' in
        let (b',el') = Option.unopt_failwith "bad pair" (extract_fold_while_result res) in
        if b then aux b' el' else return el' in
      aux true init
    )
    (* tertiary *)
    | ( C_SLICE , [ V_Ct (C_nat st) ; V_Ct (C_nat ed) ; V_Ct (C_string s) ] ) ->
      (*TODO : allign with tezos*)
      return @@ V_Ct (C_string (String.sub s (Z.to_int st) (Z.to_int ed)))
    | ( C_LIST_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_List elts ; init ] ) ->
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env (arg_binder,  fold_args) in
          eval_ligo body env'
        )
        init elts
    | ( C_MAP_EMPTY , []) -> return @@ V_Map ([])
    | ( C_MAP_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_Map kvs ; init ] ) ->
      Monad.bind_fold_list
        (fun prev kv ->
          let fold_args = v_pair (prev, v_pair kv) in
          let env' = Env.extend env (arg_binder,  fold_args) in
          eval_ligo body env'
        )
        init kvs
    | ( C_MAP_MEM , [ k ; V_Map kvs ] ) -> return @@ v_bool (List.mem_assoc k kvs)
    | ( C_MAP_ADD , [ k ; v ; V_Map kvs as vmap] ) ->
      if (List.mem_assoc k kvs) then return vmap
      else return (V_Map ((k,v)::kvs))
    | ( C_MAP_REMOVE , [ k ; V_Map kvs] ) -> return @@ V_Map (List.remove_assoc k kvs)
    | ( C_MAP_UPDATE , [ k ; V_Construct (option,v) ; V_Map kvs] ) -> (match option with
      | "Some" -> return @@ V_Map ((k,v)::(List.remove_assoc k kvs))
      | "None" -> return @@ V_Map (List.remove_assoc k kvs)
      | _ -> failwith "update without an option"
    )
    | ( C_SET_EMPTY, []) -> return @@ V_Set ([])
    | ( C_SET_ADD , [ v ; V_Set l ] ) -> return @@ V_Set (List.sort_uniq compare (v::l))
    | ( C_SET_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_Set elts ; init ] ) ->
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env (arg_binder, fold_args) in
          eval_ligo body env'
        )
        init elts
    | ( C_SET_ITER , [ V_Func_val {arg_binder ; body ; env}  ; V_Set (elts) ] ) ->
      Monad.bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env (arg_binder,elt) in
          eval_ligo body env'
        )
        (V_Ct C_unit) elts
    | ( C_SET_MEM    , [ v ; V_Set (elts) ] ) -> return @@ v_bool (List.mem v elts)
    | ( C_SET_REMOVE , [ v ; V_Set (elts) ] ) -> return @@ V_Set (List.filter (fun el -> not (el = v)) elts)
    | ( C_NOW , [] ) -> let>> now = Now in return_ct @@ C_timestamp now
    | ( C_AMOUNT , [] ) -> let>> amt = Amount in return_ct @@ C_mutez amt
    | ( C_BALANCE , [] ) -> let>> blc = Balance in return_ct @@ C_mutez blc
    | ( C_SENDER, [] ) -> let>> snd = Sender in return_ct @@ C_address (string_of_int snd)
    | ( C_SOURCE, [] ) -> let>> src = Source in return_ct @@ C_address (string_of_int src)
    | ( C_CHAIN_ID, [] ) -> let>> id = Chain_id in return_ct @@ C_bytes id
    | ( (C_CONTRACT_ENTRYPOINT_OPT | C_CONTRACT_OPT) , [ V_Ct (C_address addr) ] ) ->
      (* TODO: For now contract_entrypoint does not perform extra checks
         but the contract types are held in the context so it is possible *)
      let ctr = counter_of_address addr in
      let>> v = Get_contract ctr in
      ( match v with
        | Some v -> return (V_Construct ("Some", v))
        | None -> return (V_Construct ("None", V_Ct C_unit))
      )
    | ( (C_CONTRACT | C_CONTRACT_ENTRYPOINT) , [ V_Ct (C_address addr) ] ) ->
      let ctr = counter_of_address addr in
      let>> v = Get_contract ctr in
      ( match v with
        | Some v -> return v
        | None -> return (Errors.contract_not_found addr)
      )
    | ( C_CALL , [ param ; V_Ct (C_mutez amt) ; V_Ct (C_address addr) ] ) -> (
      let ctr = counter_of_address addr in
      let>> (code,storage) = Get_script (ctr,loc) in
      match code with
        | V_Func_val {arg_binder ; body ; env} ->
          let param_storage = V_Record (LMap.of_list [ (Label "0", param) ; (Label "1", storage) ]) in
          let f_env' = Env.extend env (arg_binder, param_storage) in
          return_ct (C_operation (Internal_transaction { addr=ctr ; code = body ; env = f_env' ; amount = amt }))
        | _ -> failwith "transaction code is not a function"
    )
    | ( C_CREATE_CONTRACT , [ code ; delegate ; V_Ct (C_mutez amount) ; storage ] ) ->
      ignore delegate;
      let>> ctr = Generate_addr in
      let op : Ligo_interpreter.Types.operation = Create_contract { addr=ctr ; code ; amount ; storage } in
      let addr = string_of_int ctr in
      return @@ V_Record (LMap.of_list [
        (Label "0", V_Ct (C_operation op)) ;
        (Label "1", V_Ct (C_address addr)) ])
    | ( C_ADDRESS , [ addr ] ) ->
      return addr
    | ( C_SELF , [ _ep ] ) ->
      let>> s = Self in
      return s
    | ( C_SELF_ADDRESS , [] ) ->
      let>> s = Self in
      return s
    | ( C_IMPLICIT_ACCOUNT , [ V_Ct (C_key_hash pkh) ] ) ->
      let>> s = Implicit_account pkh in
      return s
    | ( C_BYTES_PACK , [ v ] ) ->
      let>> s = Serialize_pack_data v in
      return (V_packed s)
    | ( C_BYTES_UNPACK , [ V_packed s ] ) ->
      let>> s = Serialize_unpack_data s in
      return @@ V_Construct ("Some" , s)
    (*
    >>>>>>>>
      Test operators
    >>>>>>>>
    *)
    | ( C_TEST_ORIGINATE, [ code ; storage ] ) ->
      let>> ctr = Generate_addr in
      let>> () = Inject_script (ctr, code, storage) in
      return_ct @@ C_address (string_of_int ctr)
    | ( C_TEST_SET_BALANCE , [ V_Ct (C_address addr) ; V_Ct (C_mutez amt) ] ) ->
      let>> () = Set_balance (counter_of_address addr, amt) in
      return_ct C_unit
    | ( C_TEST_SET_NOW , [ V_Ct (C_timestamp t) ] ) ->
      let>> () = Set_now t in
      return_ct C_unit
    | ( C_TEST_SET_SOURCE , [ V_Ct (C_address addr) ] ) ->
      let>> () = Set_source (counter_of_address addr) in
      return_ct C_unit
    | ( C_TEST_EXTERNAL_CALL , [ V_Ct (C_address addr) ; param ; V_Ct (C_mutez amt) ] ) -> (
      let ctr = counter_of_address addr in
      let>> (code,storage) = Get_script (ctr,loc) in
      let* result = match code with
          | V_Func_val {arg_binder ; body ; env} ->
            let param_storage = V_Record (LMap.of_list [ (Label "0", param) ; (Label "1", storage) ]) in
            let f_env' = Env.extend env (arg_binder, param_storage) in
            Try (
              let>> () = Credit_balance (ctr, amt) in
              let>> () = External_call (ctr, amt) in
              let* res = eval_ligo body f_env' in
              let>> () = Update_storage (ctr, res) in
              return res
            )
          | _ -> failwith "code is not a function"
      in
      match result with
      | None ->
        Errors.contract_failure @@ Format.asprintf "external contract call at address '%s'" addr
      | Some r ->
        let ops = get_operations r in
        let aux : value -> operation -> _ = fun _ op ->
          match op with
          | Create_contract { addr ; code ; amount ; storage } ->
            let>> () = Inject_script (addr, code, storage) in
            let>> () = Set_balance (addr, amount) in
            return_ct C_unit
          | Internal_transaction { addr ; code ; env ; amount } ->
            let* result = Try (
              let>> () = Debit_balance amount in
              let>> () = Credit_balance (addr, amount) in
              let>> () = Internal_call (addr, amount) in
              let* res = eval_ligo code env in
              let>> () = Update_storage (addr, res) in
              return_ct C_unit
            ) in
            begin
              match result with
              | None ->
                Errors.contract_failure @@ Format.asprintf "internal contract call at address '%d' has failed" addr
              | Some r -> return r
            end
        in
        Monad.bind_fold_list aux r ops
    )
    | ( C_TEST_GET_STORAGE , [ V_Ct (C_address addr) ] ) ->
      let>> storage = Get_storage (counter_of_address addr) in
      return storage
    | ( C_TEST_GET_BALANCE , [ V_Ct (C_address addr) ] ) ->
      let>> balance = Get_balance (counter_of_address addr) in
      return balance
    | ( C_TEST_ASSERT_FAILURE , [ V_Func_val {arg_binder=_ ; body ; env} ] ) ->
      let* failed = Try (eval_ligo body env) in
      return_ct (C_bool (Option.is_none failed))
    | ( C_TEST_LOG , [ v ]) ->
      let () = Format.printf "%a\n" Ligo_interpreter.PP.pp_value v in
      return_ct C_unit
    | _ -> call @@ Fail_prim_not_supported (loc,c)
  )

(* TODO
hash on bytes
C_BLAKE2b
C_SHA256
C_SHA512
hash on key
C_HASH_KEY

need exts
C_CHECK_SIGNATURE
*)

(*interpreter*)
and eval_literal : Ast_typed.literal -> value Monad.t = function
  | Literal_unit        -> Monad.return @@ V_Ct (C_unit)
  | Literal_int i       -> Monad.return @@ V_Ct (C_int i)
  | Literal_nat n       -> Monad.return @@ V_Ct (C_nat n)
  | Literal_timestamp i -> Monad.return @@ V_Ct (C_timestamp i)
  | Literal_string s    -> Monad.return @@ V_Ct (C_string (Ligo_string.extract s))
  | Literal_bytes s     -> Monad.return @@ V_Ct (C_bytes s)
  | Literal_mutez t     ->
    let cast_to_mutez v = match Tez.of_mutez v with
      | None -> failwith "tez literal is negative"
      | Some r -> r in
   Monad.(
      let>> t = Int_to_int64 t in
      begin
        match t with
          | Some t ->
            Monad.return @@ V_Ct (C_mutez (cast_to_mutez t))
          | None -> call Fail_overflow
      end
  )
  | Literal_address s   -> Monad.return @@ V_Ct (C_address s)
  | Literal_signature s -> Monad.return @@ V_Ct (C_signature s)
  | Literal_key s       -> Monad.return @@ V_Ct (C_key s)
  | Literal_key_hash s  -> Monad.return @@ V_Ct (C_key_hash s)
  | Literal_chain_id s  -> Monad.return @@ V_Ct (C_key_hash s)
  | Literal_operation _o -> failwith "no literal operations"

and eval_ligo : Ast_typed.expression -> env -> value Monad.t
  = fun term env ->
    let open Monad in
    match term.expression_content with
    | E_application {lamb = f; args} -> (
        let* f' = eval_ligo f env in
        let* args' = eval_ligo args env in
        match f' with
          | V_Func_val {arg_binder ; body ; env} ->
            let f_env' = Env.extend env (arg_binder, args') in
            eval_ligo body f_env'
          | V_Func_rec (fun_name, arg_names, body, f_env) ->
            let f_env' = Env.extend f_env (arg_names, args') in
            let f_env'' = Env.extend f_env' (fun_name, f') in
            eval_ligo body f_env''
          | _ -> failwith "trying to apply on something that is not a function"
      )
    | E_lambda {binder; result;} ->
      return @@ V_Func_val {arg_binder=binder ; body=result ; env}
    | E_let_in {let_binder ; rhs; let_result} -> (
      let* rhs' = eval_ligo rhs env in
      eval_ligo (let_result) (Env.extend env (let_binder,rhs'))
    )
    | E_type_in {type_binder=_ ; rhs=_; let_result} -> (
      eval_ligo (let_result) env
    )
    | E_mod_in    _ -> failwith "Module are not handled in interpreter yet"
    | E_mod_alias _ -> failwith "Module are not handled in interpreter yet"
    | E_literal l ->
      eval_literal l
    | E_variable var ->
      let v = Option.unopt_failwith "unbound variable" (Env.lookup env var) in
      return v
    | E_record recmap ->
      let* lv' = Monad.bind_map_list
        (fun (label,(v:Ast_typed.expression)) ->
          let* v' = eval_ligo v env in
          return (label,v'))
        (LMap.to_kv_list_rev recmap)
      in
      return @@ V_Record (LMap.of_list lv')
    | E_record_accessor { record ; path} -> (
      let* record' = eval_ligo record env in
      match record' with
      | V_Record recmap ->
        let a = LMap.find path recmap in
        return a
      | _ -> failwith "trying to access a non-record"
    )
    | E_record_update {record ; path ; update} -> (
      let* record' = eval_ligo record env in
      match record' with
      | V_Record recmap ->
        if LMap.mem path recmap then
          let* field' = eval_ligo update env in
          return @@ V_Record (LMap.add path field' recmap)
        else
          failwith "field l does not exist in record"
      | _ -> failwith "this expression isn't a record"
    )
    | E_constant {cons_name ; arguments} -> (
      let* arguments' = Monad.bind_map_list
        (fun (ae:Ast_typed.expression) -> eval_ligo ae env)
        arguments in
      apply_operator term.location cons_name arguments'
    )
    | E_constructor { constructor = Label c ; element } when (String.equal c "true" || String.equal c "false")
     && element.expression_content = Ast_typed.e_unit () -> return @@ V_Ct (C_bool (bool_of_string c))
    | E_constructor { constructor = Label c ; element } ->
      let* v' = eval_ligo element env in
      return @@ V_Construct (c,v')
    | E_matching { matchee ; cases} -> (
      let* e' = eval_ligo matchee env in
      match cases, e' with
      | Match_list cases , V_List [] ->
        eval_ligo cases.match_nil env
      | Match_list cases , V_List (head::tail) ->
        let {hd;tl;body;tv=_} = cases.match_cons in
        let env' = Env.extend (Env.extend env (hd,head)) (tl, V_List tail) in
        eval_ligo body env'
      | Match_variant {cases;_}, V_Ct (C_bool b) ->
        let ctor_body (case : matching_content_case) = (case.constructor, case.body) in
        let cases = LMap.of_list (List.map ctor_body cases) in
        let get_case c =
            (LMap.find (Label c) cases) in
        let match_true  = get_case "true" in
        let match_false = get_case "false" in
        if b then eval_ligo match_true env
        else eval_ligo match_false env
      | Match_variant {cases ; tv=_} , V_Construct (matched_c , proj) ->
        let {constructor=_ ; pattern ; body} =
          List.find
            (fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal matched_c c)
            cases in
        let env' = Env.extend env (pattern, proj) in
        eval_ligo body env'
      | Match_option cases, V_Construct ("Some" , proj) ->
        let {opt;body;tv=_} = cases.match_some in
        let env' = Env.extend env (opt,proj) in
        eval_ligo body env'
      | Match_option cases, V_Construct ("None" , V_Ct C_unit) ->
        eval_ligo cases.match_none env
      | Match_record {fields ; body ; tv = _} , V_Record rv ->
        let aux : label -> ( expression_variable * _ ) -> env -> env =
          fun l (v,_) env ->
            let iv = match LMap.find_opt l rv with
              | Some x -> x
              | None -> failwith "label do not match"
            in
            Env.extend env (v,iv)
        in
        let env' = LMap.fold aux fields env in
        eval_ligo body env'
      | _ -> failwith "not yet supported case"
    )
    | E_recursive {fun_name; fun_type=_; lambda} ->
      return @@ V_Func_rec (fun_name, lambda.binder, lambda.result, env)
    | E_raw_code _ -> failwith "can't evaluate a raw code insertion"
    | E_module_accessor {module_name=_; element=_} ->
      failwith "Can't evalute module yet"

open Proto_alpha_utils.Memory_proto_alpha

let ( let>>= ) o f = Trace.bind f o

let eval : ?options:options -> Ast_typed.module_fully_typed -> (env , Errors.interpreter_error) result =
  fun ?(options = default_options) (Module_Fully_Typed prg) ->
    let init_ctxt = Ligo_interpreter.Mini_proto.option_to_context options in
    let aux : env -> declaration location_wrap -> (env, Errors.interpreter_error) Trace.result =
      fun top_env el ->
        match Location.unwrap el with
        | Ast_typed.Declaration_type _ -> ok top_env
        | Ast_typed.Declaration_constant {binder; expr ; inline=_ ; _} ->
          let>>= v =
            try
              let>>= (v, _ctxt) = Monad.eval (eval_ligo expr top_env) init_ctxt None in
              ok v
            with Temporary_hack s -> fail @@ Errors.failwith s
          in
          let top_env' = Env.extend top_env (binder, v) in
          ok top_env'
        | Ast_typed.Declaration_module {module_binder; module_=_} ->
          let>>= module_env =
            failwith "Module are not handled in interpreter yet"
          in
          let top_env' = Env.extend top_env (Location.wrap @@ Var.of_name module_binder, module_env) in
          ok top_env'
        | Ast_typed.Module_alias _ -> failwith "Module are not handled in interpreter yet"
    in
    bind_fold_list aux (Env.empty_env) prg

let eval_test : ?options:options -> Ast_typed.module_fully_typed -> string -> (bool , Errors.interpreter_error) result =
  fun ?(options = default_options) prg test_entry ->
    let>>= env = eval ~options prg in
  let v = Env.to_kv_list env in
  let aux : expression_variable * value -> bool = fun (ev, _) ->
    let name = Var.to_name @@ Location.unwrap ev in
    String.equal name test_entry
  in
  match List.find_opt aux v with
  | Some (_,v) -> if is_bool v then ok @@ is_true v else fail @@ Errors.bad_test v test_entry
  | None -> fail @@ Errors.test_entry_not_found test_entry

let () = Printexc.record_backtrace true
