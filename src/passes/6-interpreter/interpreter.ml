open Trace
open Ligo_interpreter.Types
open Ligo_interpreter.Combinators
include Stage_common.Types

module Env = Ligo_interpreter.Environment


let apply_comparison : Ast_typed.constant' -> value list -> value result =
  fun c operands -> match (c,operands) with
    | ( comp , [ V_Ct (C_int a'      ) ; V_Ct (C_int b'      ) ] )
    | ( comp , [ V_Ct (C_nat a'      ) ; V_Ct (C_nat b'      ) ] )
    | ( comp , [ V_Ct (C_mutez a'    ) ; V_Ct (C_mutez b'    ) ] )
    | ( comp , [ V_Ct (C_timestamp a') ; V_Ct (C_timestamp b') ] ) ->
      let f_op = match comp with
        | C_EQ -> Int.equal
        | C_NEQ -> fun a b -> not (Int.equal a b)
        | C_LT -> (<)
        | C_LE -> (<=)
        | C_GT -> (>)
        | C_GE -> (>=)
        | _ -> failwith "apply compare must be called with a comparative constant" in
      ok @@ v_bool (f_op a' b')

    | ( comp     , [ V_Ct (C_string a'  ) ; V_Ct (C_string b'  ) ] )
    | ( comp     , [ V_Ct (C_address a' ) ; V_Ct (C_address b' ) ] )
    | ( comp     , [ V_Ct (C_key_hash a') ; V_Ct (C_key_hash b') ] ) ->
      let f_op = match comp with
        | C_EQ -> fun a b -> (String.compare a b = 0)
        | C_NEQ -> fun a b -> (String.compare a b != 0)
        (* the above might not be alligned with Michelson interpreter. Do we care ? *)
        | C_LT -> fun a b -> (String.compare a b < 0)
        | C_LE -> fun a b -> (String.compare a b <= 0)
        | C_GT -> fun a b -> (String.compare a b > 0)
        | C_GE -> fun a b -> (String.compare a b >= 0)
        | _ -> failwith "apply compare must be called with a comparative constant" in
      ok @@ v_bool (f_op a' b')

    | ( comp     , [ V_Ct (C_bytes a'  ) ; V_Ct (C_bytes b'  ) ] ) ->
      let f_op = match comp with
        | C_EQ -> fun a b -> (Bytes.compare a b = 0)
        | C_NEQ -> fun a b -> (Bytes.compare a b != 0)
        (* the above might not be alligned with Michelson interpreter. Do we care ? *)
        | C_LT -> fun a b -> (Bytes.compare a b < 0)
        | C_LE -> fun a b -> (Bytes.compare a b <= 0)
        | C_GT -> fun a b -> (Bytes.compare a b > 0)
        | C_GE -> fun a b -> (Bytes.compare a b >= 0)
        | _ -> failwith "apply compare must be called with a comparative constant" in
      ok @@ v_bool (f_op a' b')
    | _ ->
      let () = List.iter (fun el -> Format.printf "%s" (Ligo_interpreter.PP.pp_value el)) operands in
      simple_fail "unsupported comparison"

(* applying those operators does not involve extending the environment *)
let rec apply_operator : Ast_typed.constant' -> value list -> value result =
  fun c operands ->
  let return_ct v = ok @@ V_Ct v in
  let return_none () = ok @@ v_none () in
  let return_some v  = ok @@ v_some v in
  ( match (c,operands) with
    (* nullary *)
    | ( C_NONE , [] ) -> return_none ()
    | ( C_UNIT , [] ) -> ok @@ V_Ct C_unit
    | ( C_NIL  , [] ) -> ok @@ V_List []
    (* unary *)
    | ( C_FAILWITH , [ V_Ct (C_string a') ] ) ->
      (*TODO This raise is here until we properly implement effects*)
      raise (Temporary_hack a')
      (*TODO This raise is here until we properly implement effects*)

    | ( C_SIZE   , [(V_Set l | V_List l)] ) -> return_ct @@ C_nat (List.length l)
    | ( C_SIZE   , [ V_Map l            ] ) -> return_ct @@ C_nat (List.length l)
    | ( C_SIZE   , [ V_Ct (C_string s ) ] ) -> return_ct @@ C_nat (String.length s)
    | ( C_SIZE   , [ V_Ct (C_bytes b  ) ] ) -> return_ct @@ C_nat (Bytes.length b)
    | ( C_NOT    , [ V_Ct (C_bool a'  ) ] ) -> return_ct @@ C_bool (not a')
    | ( C_INT    , [ V_Ct (C_nat a')    ] ) -> return_ct @@ C_int a'
    | ( C_ABS    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (abs a')
    | ( C_NEG    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (-a')
    | ( C_SOME   , [ v                  ] ) -> return_some v
    | ( C_IS_NAT , [ V_Ct (C_int a')    ] ) ->
      if a' > 0 then return_some @@ V_Ct (C_nat a')
      else return_none ()
    | ( C_CONTINUE  , [ v ] ) -> ok @@ v_pair (v_bool true  , v)
    | ( C_STOP      , [ v ] ) -> ok @@ v_pair (v_bool false , v)
    | ( C_ASSERTION , [ v ] ) ->
      let%bind pass = is_true v in
      if pass then return_ct @@ C_unit
      else raise (Temporary_hack "failed assertion")
    | C_MAP_FIND_OPT , [ k ; V_Map l ] -> ( match List.assoc_opt k l with
      | Some v -> ok @@ v_some v
      | None -> ok @@ v_none ()
    )
    | C_MAP_FIND , [ k ; V_Map l ] -> ( match List.assoc_opt k l with
      | Some v -> ok @@ v
      | None -> raise (Temporary_hack "failed map find")
    )
    (* binary *)
    | ( (C_EQ | C_NEQ | C_LT | C_LE | C_GT | C_GE) , _ ) -> apply_comparison c operands
    | ( C_SUB    , [ V_Ct (C_int a' | C_nat a') ; V_Ct (C_int b' | C_nat b') ] ) -> return_ct @@ C_int (a' - b')
    | ( C_CONS   , [ v                  ; V_List vl          ] ) -> ok @@ V_List (v::vl)
    | ( C_ADD    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] ) -> return_ct @@ C_int   (a' + b')
    | ( C_ADD    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_nat   (a' + b')
    | ( C_ADD    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] ) -> return_ct @@ C_int   (a' + b')
    | ( C_ADD    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_int   (a' + b')
    | ( C_MUL    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] ) -> return_ct @@ C_int   (a' * b')
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_nat   (a' * b')
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_mutez b')  ] ) -> return_ct @@ C_mutez (a' * b')
    | ( C_MUL    , [ V_Ct (C_mutez a')  ; V_Ct (C_mutez b')  ] ) -> return_ct @@ C_mutez (a' * b')
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] ) -> return_ct @@ C_int   (a' / b')
    | ( C_DIV    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_nat   (a' / b')
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_mutez (a' / b')
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_mutez b')  ] ) -> return_ct @@ C_nat   (a' / b')
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_int b')    ] ) -> return_ct @@ C_nat   (a' mod b')
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_nat b')    ] ) -> return_ct @@ C_nat   (a' mod b')
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_int b')    ] ) -> return_ct @@ C_nat   (a' mod b')
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_nat b')    ] ) -> return_ct @@ C_nat   (a' mod b')
    | ( C_CONCAT , [ V_Ct (C_string a') ; V_Ct (C_string b') ] ) -> return_ct @@ C_string (a' ^ b')
    | ( C_CONCAT , [ V_Ct (C_bytes a' ) ; V_Ct (C_bytes b' ) ] ) -> return_ct @@ C_bytes  (Bytes.cat a' b')
    | ( C_OR     , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' || b')
    | ( C_AND    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' && b')
    | ( C_XOR    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   ( (a' || b') && (not (a' && b')) ) 
    | ( C_LIST_MAP , [ V_Func_val (arg_name, body, env) ; V_List (elts) ] ) ->
      let%bind elts' = bind_map_list
        (fun elt ->
          let env' = Env.extend env (arg_name,elt) in
          eval body env')
        elts in
      ok @@ V_List elts'
    | ( C_MAP_MAP , [ V_Func_val (arg_name, body, env) ; V_Map (elts) ] ) ->
      let%bind elts' = bind_map_list
        (fun (k,v) ->
          let env' = Env.extend env (arg_name,v_pair (k,v)) in
          let%bind v' = eval body env' in
          ok @@ (k,v')
        )
        elts in
      ok @@ V_Map elts'
    | ( C_LIST_ITER , [ V_Func_val (arg_name, body, env) ; V_List (elts) ] ) ->
      bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env (arg_name,elt) in
          eval body env'
        )
        (V_Ct C_unit) elts
    | ( C_MAP_ITER , [ V_Func_val (arg_name, body, env) ; V_Map (elts) ] ) ->
      bind_fold_list
        (fun _ kv ->
          let env' = Env.extend env (arg_name,v_pair kv) in
          eval body env'
        )
        (V_Ct C_unit) elts
    | ( C_FOLD_WHILE , [ V_Func_val (arg_name, body, env) ; init ] ) ->
      let rec aux el =
        let%bind (b,folded_val) = extract_pair el in
        let env' = Env.extend env (arg_name, folded_val) in
        let%bind res = eval body env' in
        let%bind continue = is_true b in
        if continue then aux res else ok folded_val in
      aux @@ v_pair (v_bool true,init)
    (* tertiary *)
    | ( C_SLICE , [ V_Ct (C_nat st) ; V_Ct (C_nat ed) ; V_Ct (C_string s) ] ) ->
      generic_try (simple_error "bad slice") @@ (fun () ->
        V_Ct (C_string (String.sub s st ed))
      )
    | ( C_LIST_FOLD , [ V_Func_val (arg_name, body, env) ; V_List elts ; init ] ) ->
      bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env (arg_name,  fold_args) in
          eval body env'
        )
        init elts
    | ( C_MAP_FOLD , [ V_Func_val (arg_name, body, env) ; V_Map kvs ; init ] ) ->
      bind_fold_list
        (fun prev kv ->
          let fold_args = v_pair (prev, v_pair kv) in
          let env' = Env.extend env (arg_name,  fold_args) in
          eval body env'
        )
        init kvs
    | ( C_MAP_MEM , [ k ; V_Map kvs ] ) -> ok @@ v_bool (List.mem_assoc k kvs)
    | ( C_MAP_ADD , [ k ; v ; V_Map kvs as vmap] ) ->
      if (List.mem_assoc k kvs) then ok vmap
      else ok (V_Map ((k,v)::kvs)) 
    | ( C_MAP_REMOVE , [ k ; V_Map kvs] ) -> ok @@ V_Map (List.remove_assoc k kvs)
    | ( C_MAP_UPDATE , [ k ; V_Construct (option,v) ; V_Map kvs] ) -> (match option with
      | "Some" -> ok @@ V_Map ((k,v)::(List.remove_assoc k kvs))
      | "None" -> ok @@ V_Map (List.remove_assoc k kvs)
      | _ -> simple_fail "update without an option"
    )
    | ( C_SET_ADD , [ v ; V_Set l ] ) -> ok @@ V_Set (List.sort_uniq compare (v::l))
    | ( C_SET_FOLD , [ V_Func_val (arg_name, body, env) ; V_Set elts ; init ] ) ->
      bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env (arg_name, fold_args) in
          eval body env'
        )
        init elts
    | ( C_SET_ITER , [ V_Func_val (arg_name, body, env) ; V_Set (elts) ] ) ->
      bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env (arg_name,elt) in
          eval body env'
        )
        (V_Ct C_unit) elts
    | ( C_SET_MEM    , [ v ; V_Set (elts) ] ) -> ok @@ v_bool (List.mem v elts)
    | ( C_SET_REMOVE , [ v ; V_Set (elts) ] ) -> ok @@ V_Set (List.filter (fun el -> not (el = v)) elts)
    | _ ->
      let () = Format.printf "%a\n" Stage_common.PP.constant c in
      let () = List.iter ( fun e -> Format.printf "%s\n" (Ligo_interpreter.PP.pp_value e)) operands in
      simple_fail "Unsupported constant op"
  )

(* TODO

hash on bytes
C_BLAKE2b
C_SHA256
C_SHA512
hash on key
C_HASH_KEY

need exts
C_AMOUNT
C_BALANCE
C_CHAIN_ID
C_CONTRACT_ENTRYPOINT_OPT
C_CONTRACT_OPT
C_CONTRACT
C_CONTRACT_ENTRYPOINT
C_SELF_ADDRESS
C_SOURCE
C_SENDER
C_NOW
C_IMPLICIT_ACCOUNT

C_CALL
C_SET_DELEGATE

C_BYTES_PACK
C_BYTES_UNPACK
C_CHECK_SIGNATURE
C_ADDRESS


WONT DO:
C_STEPS_TO_QUOTA

*)

(*interpreter*)
and eval_literal : Ast_typed.literal -> value result = function
  | Literal_unit        -> ok @@ V_Ct (C_unit)
  | Literal_bool b      -> ok @@ V_Ct (C_bool b)
  | Literal_int i       -> ok @@ V_Ct (C_int i)
  | Literal_nat n       -> ok @@ V_Ct (C_nat n)
  | Literal_timestamp i -> ok @@ V_Ct (C_timestamp i)
  | Literal_string s    -> ok @@ V_Ct (C_string s)
  | Literal_bytes s     -> ok @@ V_Ct (C_bytes s)
  | Literal_mutez t     -> ok @@ V_Ct (C_mutez t)
  | Literal_address s   -> ok @@ V_Ct (C_address s)
  | Literal_signature s -> ok @@ V_Ct (C_signature s)
  | Literal_key s       -> ok @@ V_Ct (C_key s)
  | Literal_key_hash s  -> ok @@ V_Ct (C_key_hash s)
  | Literal_chain_id s  -> ok @@ V_Ct (C_key_hash s)
  | Literal_operation o -> ok @@ V_Ct (C_operation o)
  | Literal_void -> simple_fail "iguess ?"

and eval : Ast_typed.expression -> env -> value result
  = fun term env ->
    match term.expression_content with
    | E_application ({expr1 = f; expr2 = args}) -> (
      let%bind f' = eval f env in
      match f' with
      | V_Func_val (arg_names, body, f_env) ->
        let%bind args' = eval args env in
        let f_env' = Env.extend f_env (arg_names, args') in
        eval body f_env'
      | _ -> simple_fail "trying to apply on something that is not a function"
    )
    | E_lambda { binder; result;} ->
      ok @@ V_Func_val (binder,result,env)
    | E_let_in { let_binder; rhs; let_result; _} ->
      let%bind rhs' = eval rhs env in
      eval let_result (Env.extend env (let_binder,rhs'))
    | E_map kvlist | E_big_map kvlist ->
      let%bind kvlist' = bind_map_list
        (fun kv -> bind_map_pair (fun (el:Ast_typed.expression) -> eval el env) kv)
        kvlist in
      ok @@ V_Map kvlist'
    | E_list expl ->
      let%bind expl' = bind_map_list
        (fun (exp:Ast_typed.expression) -> eval exp env)
        expl in
      ok @@ V_List expl'
    | E_set expl ->
      let%bind expl' = bind_map_list
        (fun (exp:Ast_typed.expression) -> eval exp env)
        (List.sort_uniq compare expl)
      in
      ok @@ V_Set expl'
    | E_literal l ->
      eval_literal l
    | E_variable var ->
      Env.lookup env var
    | E_record recmap ->
      let%bind lv' = bind_map_list
        (fun (label,(v:Ast_typed.expression)) ->
          let%bind v' = eval v env in
          ok (label,v'))
        (LMap.to_kv_list recmap) in
      ok @@ V_Record (LMap.of_list lv')
    | E_record_accessor { expr ; label} -> (
      let%bind record' = eval expr env in
      match record' with
      | V_Record recmap ->
        let%bind a = trace_option (simple_error "unknown record field") @@
          LMap.find_opt label recmap in
        ok a
      | _ -> simple_fail "trying to access a non-record"
    )
    | E_record_update {record ; path ; update} -> (
      let%bind record' = eval record env in
      match record' with
      | V_Record recmap ->
        if LMap.mem path recmap then
          let%bind field' = eval update env in
          ok @@ V_Record (LMap.add path field' recmap)
        else
          simple_fail "field l does not exist in record"
      | _ -> simple_fail "this expression isn't a record"
    )
    | E_constant {cons_name ; arguments} -> (
      let%bind operands' = bind_map_list
        (fun (ae:Ast_typed.expression) -> eval ae env)
        arguments in
      apply_operator cons_name operands'
    )
    | E_constructor { constructor = Constructor c ; element } ->
      let%bind v' = eval element env in
      ok @@ V_Construct (c,v')
    | E_matching { matchee ; cases} -> (
      let%bind e' = eval matchee env in
      match cases, e' with
      | Match_list cases , V_List [] ->
        eval cases.match_nil env
      | Match_list cases , V_List (head::tail) ->
        let (head_var,tail_var,body,_) = cases.match_cons in
        let env' = Env.extend (Env.extend env (head_var,head)) (tail_var, V_List tail) in
        eval body env'
      | Match_variant (case_list , _) , V_Construct (matched_c , proj) ->
        let ((_, var) , body) =
          List.find
            (fun case ->
              let (Constructor c , _) = fst case in
              String.equal matched_c c)
            case_list in
        let env' = Env.extend env (var, proj) in
        eval body env'
      | Match_bool cases , V_Ct (C_bool true) ->
        eval cases.match_true env
      | Match_bool cases , V_Ct (C_bool false) ->
        eval cases.match_false env
      | Match_option cases, V_Construct ("Some" , proj) ->
        let (var,body,_) = cases.match_some in
        let env' = Env.extend env (var,proj) in
        eval body env'
      | Match_option cases, V_Construct ("None" , V_Ct C_unit) ->
        eval cases.match_none env
      | _ -> simple_fail "not yet supported case"
        (* ((ctor,name),body) *)
    )
    | E_look_up _ | E_loop _ ->
      let serr = Format.asprintf "Unsupported construct :\n %a\n" Ast_typed.PP.expression term in
      simple_fail serr

let dummy : Ast_typed.program -> string result =
  fun prg ->
    let%bind (res,_) = bind_fold_list
      (fun (pp,top_env) el ->
        let (Ast_typed.Declaration_constant (exp_name, exp , _ , _)) = Location.unwrap el in
        let%bind v =
        (*TODO This TRY-CATCH is here until we properly implement effects*)
        try
          eval exp top_env
        with Temporary_hack s -> ok @@ V_Failure s
        (*TODO This TRY-CATCH is here until we properly implement effects*)
        in
        let pp' = pp^"\n val "^(Var.to_name exp_name)^" = "^(Ligo_interpreter.PP.pp_value v) in
        let top_env' = Env.extend top_env (exp_name, v) in
        ok @@ (pp',top_env')
      )
      ("",Env.empty_env) prg in
    ok @@ res
