open Trace
open Ligo_interpreter.Types
open Ligo_interpreter.Combinators
include Ast_typed.Types
module Env = Ligo_interpreter.Environment
module Ligo_interpreter_exc = Ligo_interpreter_exc
open Ligo_interpreter_exc


type interpreter_error = Errors.interpreter_error

let apply_comparison : Location.t -> Ast_typed.constant' -> value list -> value Monad.t =
  fun loc c operands ->
    let open Monad in
    match (c,operands) with
    | ( comp , [ V_Ct (C_int a'      ) ; V_Ct (C_int b'      ) ] )
    | ( comp , [ V_Ct (C_timestamp a') ; V_Ct (C_timestamp b') ] )
    | ( comp , [ V_Ct (C_nat a'      ) ; V_Ct (C_nat b'      ) ] ) ->
      let>> i = Int_compare_wrapped (a', b') in
      let>> cmpres = Int_of_int i in
      let>> cmpres = Int_compare (cmpres, Ligo_interpreter.Int_repr_copied.zero) in
      let x = match comp with
        | C_EQ -> (cmpres = 0)
        | C_NEQ -> (cmpres <> 0)
        | C_LT -> (cmpres < 0)
        | C_LE -> (cmpres <= 0)
        | C_GT -> (cmpres > 0)
        | C_GE -> (cmpres >= 0)
        | _ -> raise (Meta_lang_ex {location = loc ; reason = Reason "Not comparable" })
      in
      return @@ v_bool x
    | ( _     , [ V_Ct (C_address b ) ; V_Ct (C_address a ) ] ) ->
      return @@ v_bool @@ Tezos_state.compare_account a b
    | ( comp     , [ V_Ct (C_string a'  ) ; V_Ct (C_string b'  ) ] ) ->
      let f_op = match comp with
        | C_EQ -> fun a b -> (String.compare a b = 0)
        | C_NEQ -> fun a b -> (String.compare a b != 0)
        | C_LT -> fun a b -> (String.compare a b < 0)
        | C_LE -> fun a b -> (String.compare a b <= 0)
        | C_GT -> fun a b -> (String.compare a b > 0)
        | C_GE -> fun a b -> (String.compare a b >= 0)
        | _ -> raise (Meta_lang_ex {location = loc ; reason = Reason "Not comparable" }) in
      Monad.return @@ v_bool (f_op a' b')

    | ( comp     , [ V_Ct (C_bytes a'  ) ; V_Ct (C_bytes b'  ) ] ) ->
      let f_op = match comp with
        | C_EQ -> fun a b -> (Bytes.compare a b = 0)
        | C_NEQ -> fun a b -> (Bytes.compare a b != 0)
        | C_LT -> fun a b -> (Bytes.compare a b < 0)
        | C_LE -> fun a b -> (Bytes.compare a b <= 0)
        | C_GT -> fun a b -> (Bytes.compare a b > 0)
        | C_GE -> fun a b -> (Bytes.compare a b >= 0)
        | _ -> raise (Meta_lang_ex {location = loc ; reason = Reason "Not comparable" }) in
      Monad.return @@ v_bool (f_op a' b')
    | _ -> raise (Meta_lang_ex {location = loc ; reason = Reason "Not comparable" })

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
      else raise (Meta_lang_ex {location = loc ; reason = Reason "Failed assertion"})
    | C_MAP_FIND_OPT , [ k ; V_Map l ] -> ( match List.Assoc.find ~equal:Caml.(=) l k with
      | Some v -> return @@ v_some v
      | None -> return @@ v_none ()
    )
    | C_MAP_FIND , [ k ; V_Map l ] -> ( match List.Assoc.find ~equal:Caml.(=) l k with
      | Some v -> return @@ v
      | None -> raise (Meta_lang_ex {location = loc ; reason = Reason (Predefined.Tree_abstraction.pseudo_module_to_string c)})
    )
    (* binary *)
    | ( (C_EQ | C_NEQ | C_LT | C_LE | C_GT | C_GE) , _ ) -> apply_comparison loc c operands
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
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] ) ->
      let>> a = Int_ediv (a',b') in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_int res
        | None -> raise (Meta_lang_ex {location = loc ; reason = Reason "Dividing by zero"})
      end
    | ( C_DIV    , [ V_Ct (C_nat a')  ; V_Ct (C_nat b')  ] ) ->
      let>> a = Int_ediv_n (a',b') in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_nat res
        | None -> raise (Meta_lang_ex {location = loc ; reason = Reason "Dividing by zero"})
      end
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_int b')    ] )
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_nat b')    ] )
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_int b')    ] ) -> (
      let>> a = Int_ediv (a',b') in
      match a with
      | Some (_,r) -> return_ct @@ C_nat r
      | None -> raise (Meta_lang_ex {location = loc ; reason = Reason "Dividing by zero"})
    )
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_nat b')    ] ) -> (
      let>> a = Int_ediv_n (a',b') in
      match a with
      | Some (_,r) -> return_ct @@ C_nat r
      | None -> raise (Meta_lang_ex {location = loc ; reason = Reason "Dividing by zero"})
    )
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
        let (b',el') = try Option.value_exn (extract_fold_while_result res) with _ -> (failwith "bad pair") in
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
    | ( C_MAP_MEM , [ k ; V_Map kvs ] ) -> return @@ v_bool (List.Assoc.mem ~equal:Caml.(=) kvs k)
    | ( C_MAP_ADD , [ k ; v ; V_Map kvs] ) -> return (V_Map ((k,v) :: List.Assoc.remove ~equal:Caml.(=) kvs k))
    | ( C_MAP_REMOVE , [ k ; V_Map kvs] ) -> return @@ V_Map (List.Assoc.remove ~equal:Caml.(=) kvs k)
    | ( C_MAP_UPDATE , [ k ; V_Construct (option,v) ; V_Map kvs] ) -> (match option with
      | "Some" -> return @@ V_Map ((k,v)::(List.Assoc.remove ~equal:Caml.(=) kvs k))
      | "None" -> return @@ V_Map (List.Assoc.remove ~equal:Caml.(=) kvs k)
      | _ -> assert false
    )
    | ( C_SET_EMPTY, []) -> return @@ V_Set ([])
    | ( C_SET_ADD , [ v ; V_Set l ] ) -> return @@ V_Set (List.dedup_and_sort ~compare (v::l))
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
    | ( C_SET_MEM    , [ v ; V_Set (elts) ] ) -> return @@ v_bool (List.mem ~equal:Caml.(=) elts v)
    | ( C_SET_REMOVE , [ v ; V_Set (elts) ] ) -> return @@ V_Set (List.filter ~f:(fun el -> not (el = v)) elts)
    | ( C_ADDRESS , [ addr ] ) ->
      return addr
    (*
    >>>>>>>>
      Test operators
    >>>>>>>>
    *)
    | ( C_TEST_COMPILE_EXPRESSION_SUBST, [ file_opt ; V_Ligo (syntax,ligo_exp) ; subst ] ) ->
      let>> code = Compile_expression (loc, file_opt, syntax, ligo_exp, Some subst) in
      return code
    | ( C_TEST_COMPILE_EXPRESSION, [ file_opt ; V_Ligo (syntax,ligo_exp) ] ) ->
      let>> code = Compile_expression (loc, file_opt, syntax, ligo_exp, None) in
      return code
    | ( C_TEST_ORIGINATE, [ V_Ct (C_string source_file) ; V_Ct (C_string entryp) ; storage ] ) ->
      let>> (code,size) = Compile_contract (source_file,entryp) in
      let>> addr = Inject_script (loc, code, storage) in
      return @@ V_Record (LMap.of_list [ (Label "0", addr) ; (Label "1", code) ; (Label "2", size) ])
    | ( C_TEST_SET_NOW , [ V_Ct (C_timestamp t) ] ) ->
      let>> () = Set_now (loc,t) in
      return_ct C_unit
    | ( C_TEST_SET_SOURCE , [ addr ] ) ->
      let>> () = Set_source addr in
      return_ct C_unit
    | ( C_TEST_SET_BAKER , [ addr ] ) ->
      let>> () = Set_baker addr in
      return_ct C_unit
    | ( C_TEST_EXTERNAL_CALL_EXN , [ addr ; param ; amt ] ) -> (
      let>> err_opt = External_call (loc,addr,param,amt) in
      match err_opt with
      | None -> return_ct C_unit
      | Some e -> raise (Object_lang_ex (loc,e))
    )
    | ( C_TEST_EXTERNAL_CALL , [ addr ; param ; amt ] ) -> (
      let>> err_opt = External_call (loc,addr,param,amt) in
      match err_opt with
      | None -> return (LC.v_ctor "Success" @@ LC.v_unit ())
      | Some e ->
        let>> a = State_error_to_value e in
        return a
    )
    | ( C_TEST_GET_STORAGE , [ addr ] ) ->
      let>> storage = Get_storage (loc, addr) in
      return storage
    | ( C_TEST_GET_BALANCE , [ addr ] ) ->
      let>> balance = Get_balance (loc, addr) in
      return balance
    | ( C_TEST_MICHELSON_EQUAL , [ a ; b ] ) ->
      let>> b = Michelson_equal (loc,a,b) in
      return_ct (C_bool b)
    | ( C_TEST_LOG , [ v ]) ->
      let () = Format.printf "%a\n" Ligo_interpreter.PP.pp_value v in
      return_ct C_unit
    | ( C_TEST_STATE_RESET , [ n ; amts ] ) ->
      let>> () = Reset_state (loc,n,amts) in
      return_ct C_unit
    | ( C_TEST_GET_NTH_BS , [ n ] ) ->
      let>> x = Get_bootstrap (loc,n) in
      return x
    | ( C_TEST_LAST_ORIGINATIONS , [ _ ] ) ->
      let>> x = Get_last_originations () in
      return x
    | ( C_TEST_COMPILE_META_VALUE , [ a ] ) ->
      let>> x = Compile_meta_value (loc,a) in
      return x
    | ( C_FAILWITH , [ a ] ) ->
      raise (Meta_lang_ex {location = loc ; reason = Val a})
    | _ -> Fail_ligo (Errors.generic_error loc "Unbound primitive.")
  )

(*interpreter*)
and eval_literal : Ast_typed.literal -> value Monad.t = function
  | Literal_unit        -> Monad.return @@ V_Ct (C_unit)
  | Literal_int i       -> Monad.return @@ V_Ct (C_int i)
  | Literal_nat n       -> Monad.return @@ V_Ct (C_nat n)
  | Literal_timestamp i -> Monad.return @@ V_Ct (C_timestamp i)
  | Literal_string s    -> Monad.return @@ V_Ct (C_string (Ligo_string.extract s))
  | Literal_bytes s     -> Monad.return @@ V_Ct (C_bytes s)
  | _   -> Monad.fail @@ Errors.generic_error Location.generated "Unsupported literal"

and eval_ligo : Ast_typed.expression -> env -> value Monad.t
  = fun term env ->
    let open Monad in
    match term.expression_content with
    | E_application {lamb = f; args} -> (
        let* f' = eval_ligo f env in
        let* args' = eval_ligo args env in
        match f' with
          | V_Func_val {arg_binder ; body ; env; _} ->
            let f_env' = Env.extend env (arg_binder, args') in
            eval_ligo body f_env'
          | V_Func_rec (fun_name, arg_names, body, f_env) ->
            let f_env' = Env.extend f_env (arg_names, args') in
            let f_env'' = Env.extend f_env' (fun_name, f') in
            eval_ligo body f_env''
          | _ -> failwith "trying to apply on something that is not a function"
      )
    | E_lambda {binder; result;} ->
      return @@ V_Func_val {orig_lambda = term ; arg_binder=binder ; body=result ; env}
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
      let v = try Option.value_exn (Env.lookup env var) with _ -> (failwith "unbound variable") in
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
      | Match_variant {cases;_}, V_List [] ->
        let {constructor=_ ; pattern=_ ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal "Nil" c)
            cases in
        eval_ligo body env
      | Match_variant {cases;_}, V_List lst ->
        let {constructor=_ ; pattern ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal "Cons" c)
            cases in
        let hd = List.hd_exn lst in
        let tl = V_List (List.tl_exn lst) in
        let proj = v_pair (hd,tl) in
        let env' = Env.extend env (pattern, proj) in
        eval_ligo body env'
      | Match_variant {cases;_}, V_Ct (C_bool b) ->
        let ctor_body (case : matching_content_case) = (case.constructor, case.body) in
        let cases = LMap.of_list (List.map ~f:ctor_body cases) in
        let get_case c =
            (LMap.find (Label c) cases) in
        let match_true  = get_case "true" in
        let match_false = get_case "false" in
        if b then eval_ligo match_true env
        else eval_ligo match_false env
      | Match_variant {cases ; tv=_} , V_Construct (matched_c , proj) ->
        let {constructor=_ ; pattern ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal matched_c c)
            cases in
        let env' = Env.extend env (pattern, proj) in
        eval_ligo body env'
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
      | _ , v -> failwith ("not yet supported case "^ Format.asprintf "%a" Ligo_interpreter.PP.pp_value v^ Format.asprintf "%a" Ast_typed.PP.expression term)
    )
    | E_recursive {fun_name; fun_type=_; lambda} ->
      return @@ V_Func_rec (fun_name, lambda.binder, lambda.result, env)
    | E_raw_code {language ; code} -> (
      match code.expression_content with
      | E_literal (Literal_string x) ->
        let exp_as_string = Ligo_string.extract x in
        return @@ V_Ligo (language , exp_as_string)
      | _ -> failwith "impossible"
    )
    | E_module_accessor {module_name=_; element=_} ->
      failwith "Can't evalute module yet"

let ( let>>= ) o f = Trace.bind f o

let eval : Ast_typed.module_fully_typed -> (env , Errors.interpreter_error) result =
  fun (Module_Fully_Typed prg) ->
    let aux : env * Tezos_state.context -> declaration location_wrap -> (env * Tezos_state.context, Errors.interpreter_error) Trace.result =      
      fun (top_env,state) el ->
        match Location.unwrap el with
        | Ast_typed.Declaration_type _ -> ok (top_env,state)
        | Ast_typed.Declaration_constant {binder; expr ; inline=_ ; _} ->
          let>>= (v,state) =
            try Monad.eval (eval_ligo expr top_env) state None
            with
              | Object_lang_ex (loc,e) -> fail @@ Errors.target_lang_error loc e
              | Meta_lang_ex {location ; reason = Val x} -> fail @@ Errors.meta_lang_failwith location x
              | Meta_lang_ex {location ; reason = Reason x} -> fail @@ Errors.meta_lang_eval location x
          in
          let top_env' = Env.extend top_env (binder, v) in
          ok (top_env',state)
        | Ast_typed.Declaration_module {module_binder; module_=_} ->
          let>>= module_env =
            failwith "Module are not handled in interpreter yet"
          in
          let top_env' = Env.extend top_env (Location.wrap @@ Var.of_name module_binder, module_env) in
          ok (top_env',state)
        | Ast_typed.Module_alias _ -> failwith "Module are not handled in interpreter yet"
    in
    let* initial_state = Tezos_state.init_ctxt () in
    let* (env,_) = bind_fold_list aux (Env.empty_env, initial_state) prg in
    ok env

let eval_test : Ast_typed.module_fully_typed -> string -> (value , Errors.interpreter_error) result =
  fun prg test_entry ->
    let>>= env = eval prg in
    let v = Env.to_kv_list env in
    let aux : expression_variable * value -> bool = fun (ev, _) ->
      let name = Var.to_name @@ Location.unwrap ev in
      String.equal name test_entry
    in
    match List.find ~f:aux v with
    | Some (_,v) -> ok v
    | None -> fail @@ Errors.test_entry_not_found test_entry

let () = Printexc.record_backtrace true
