open Trace
open Ligo_interpreter.Types
open Ligo_interpreter.Combinators
include Ast_typed.Types

module Env = Ligo_interpreter.Environment

(*TODO, maybe the interpreter should never fail ?*)
type interpreter_error = []


let apply_comparison : Ast_typed.constant' -> value list -> (value , interpreter_error) result =
  fun c operands -> match (c,operands) with
    | ( comp , [ V_Ct (C_int a'      ) ; V_Ct (C_int b'      ) ] )
    | ( comp , [ V_Ct (C_nat a'      ) ; V_Ct (C_nat b'      ) ] )
    | ( comp , [ V_Ct (C_mutez a'    ) ; V_Ct (C_mutez b'    ) ] )
    | ( comp , [ V_Ct (C_timestamp a') ; V_Ct (C_timestamp b') ] ) ->
      let f_op = match comp with
        | C_EQ -> Z.equal
        | C_NEQ -> fun a b -> not (Z.equal a b)
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
      failwith "unsupported comparison"

(* applying those operators does not involve extending the environment *)
let rec apply_operator : Ast_typed.constant' -> value list -> (value, interpreter_error) result =
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

    | ( C_SIZE   , [(V_Set l | V_List l)] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Map l            ] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Ct (C_string s ) ] ) -> return_ct @@ C_nat (Z.of_int @@ String.length s)
    | ( C_SIZE   , [ V_Ct (C_bytes b  ) ] ) -> return_ct @@ C_nat (Z.of_int @@ Bytes.length b)
    | ( C_NOT    , [ V_Ct (C_bool a'  ) ] ) -> return_ct @@ C_bool (not a')
    | ( C_INT    , [ V_Ct (C_nat a')    ] ) -> return_ct @@ C_int a'
    | ( C_ABS    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (Z.abs a')
    | ( C_NEG    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (Z.neg a')
    | ( C_SOME   , [ v                  ] ) -> return_some v
    | ( C_IS_NAT , [ V_Ct (C_int a')    ] ) ->
      if a' > Z.zero then return_some @@ V_Ct (C_nat a')
      else return_none ()
    | ( C_FOLD_CONTINUE  , [ v ] ) -> ok @@ v_pair (v_bool true  , v)
    | ( C_FOLD_STOP      , [ v ] ) -> ok @@ v_pair (v_bool false , v)
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
    | ( C_SUB    , [ V_Ct (C_int a' | C_nat a') ; V_Ct (C_int b' | C_nat b') ] ) -> return_ct @@ C_int (Z.sub a' b')
    | ( C_CONS   , [ v                  ; V_List vl          ] ) -> ok @@ V_List (v::vl)
    | ( C_ADD    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] ) -> return_ct @@ C_int   (Z.add a' b')
    | ( C_ADD    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_nat   (Z.add a' b')
    | ( C_ADD    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] ) -> return_ct @@ C_int   (Z.add a' b')
    | ( C_ADD    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_int   (Z.add a' b')
    | ( C_MUL    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] ) -> return_ct @@ C_int   (Z.mul a' b')
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_nat   (Z.mul a' b')
    | ( C_MUL    , [ V_Ct (C_nat a'  )  ; V_Ct (C_mutez b')  ] ) -> return_ct @@ C_mutez (Z.mul a' b')
    | ( C_MUL    , [ V_Ct (C_mutez a')  ; V_Ct (C_mutez b')  ] ) -> return_ct @@ C_mutez (Z.mul a' b')
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] ) -> return_ct @@ C_int   (Z.div a' b')
    | ( C_DIV    , [ V_Ct (C_nat a'  )  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_nat   (Z.div a' b')
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_nat b'  )  ] ) -> return_ct @@ C_mutez (Z.div a' b')
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_mutez b')  ] ) -> return_ct @@ C_nat   (Z.div a' b')
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_int b')    ] ) -> return_ct @@ C_nat   (Z.rem a' b')
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_nat b')    ] ) -> return_ct @@ C_nat   (Z.rem a' b')
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_int b')    ] ) -> return_ct @@ C_nat   (Z.rem a' b')
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_nat b')    ] ) -> return_ct @@ C_nat   (Z.rem a' b')
    | ( C_CONCAT , [ V_Ct (C_string a') ; V_Ct (C_string b') ] ) -> return_ct @@ C_string (a' ^ b')
    | ( C_CONCAT , [ V_Ct (C_bytes a' ) ; V_Ct (C_bytes b' ) ] ) -> return_ct @@ C_bytes  (Bytes.cat a' b')
    | ( C_OR     , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' || b')
    | ( C_AND    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' && b')
    | ( C_XOR    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   ( (a' || b') && (not (a' && b')) ) 
    | ( C_LIST_EMPTY, []) -> ok @@ V_List ([])
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
      ok @@ V_Ct (C_string (String.sub s (Z.to_int st) (Z.to_int ed)))
    | ( C_LIST_FOLD , [ V_Func_val (arg_name, body, env) ; V_List elts ; init ] ) ->
      bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env (arg_name,  fold_args) in
          eval body env'
        )
        init elts
    | ( C_MAP_EMPTY , []) -> ok @@ V_Map ([])
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
      | _ -> failwith "update without an option"
    )
    | ( C_SET_EMPTY, []) -> ok @@ V_Set ([])
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
      let () = Format.printf "%a\n" Ast_typed.PP.constant c in
      let () = List.iter ( fun e -> Format.printf "%s\n" (Ligo_interpreter.PP.pp_value e)) operands in
      failwith "Unsupported constant op"
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
and eval_literal : Ast_typed.literal -> (value , _) result = function
  | Literal_unit        -> ok @@ V_Ct (C_unit)
  | Literal_int i       -> ok @@ V_Ct (C_int i)
  | Literal_nat n       -> ok @@ V_Ct (C_nat n)
  | Literal_timestamp i -> ok @@ V_Ct (C_timestamp i)
  | Literal_string s    -> ok @@ V_Ct (C_string (Ligo_string.extract s))
  | Literal_bytes s     -> ok @@ V_Ct (C_bytes s)
  | Literal_mutez t     -> ok @@ V_Ct (C_mutez t)
  | Literal_address s   -> ok @@ V_Ct (C_address s)
  | Literal_signature s -> ok @@ V_Ct (C_signature s)
  | Literal_key s       -> ok @@ V_Ct (C_key s)
  | Literal_key_hash s  -> ok @@ V_Ct (C_key_hash s)
  | Literal_chain_id s  -> ok @@ V_Ct (C_key_hash s)
  | Literal_operation o -> ok @@ V_Ct (C_operation o)
  | Literal_void -> failwith "iguess ?"

and eval : Ast_typed.expression -> env -> (value , _) result
  = fun term env ->
    match term.expression_content with
    | E_application ({lamb = f; args}) -> (
      let%bind f' = eval f env in
      let%bind args' = eval args env in
      match f' with
      | V_Func_val (arg_names, body, f_env) ->
        let f_env' = Env.extend f_env (arg_names, args') in
        eval body f_env'
      | V_Func_rec (fun_name, arg_names, body, f_env) ->
        let f_env' = Env.extend f_env (arg_names, args') in
        let f_env'' = Env.extend f_env' (fun_name, f') in
        eval body f_env''
      | _ -> failwith "trying to apply on something that is not a function"
    )
    | E_lambda {binder; result;} ->
      ok @@ V_Func_val (binder,result,env)
    | E_let_in {let_binder ; rhs; let_result} -> (
      let%bind rhs' = eval rhs env in
      eval let_result (Env.extend env (let_binder,rhs'))
    )
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
    | E_record_accessor { record ; path} -> (
      let%bind record' = eval record env in
      match record' with
      | V_Record recmap ->
        let a = LMap.find path recmap in
        ok a
      | _ -> failwith "trying to access a non-record"
    )
    | E_record_update {record ; path ; update} -> (
      let%bind record' = eval record env in
      match record' with
      | V_Record recmap ->
        if LMap.mem path recmap then
          let%bind field' = eval update env in
          ok @@ V_Record (LMap.add path field' recmap)
        else
          failwith "field l does not exist in record"
      | _ -> failwith "this expression isn't a record"
    )
    | E_constant {cons_name ; arguments} -> (
      let%bind operands' = bind_map_list
        (fun (ae:Ast_typed.expression) -> eval ae env)
        arguments in
      apply_operator cons_name operands'
    )
    | E_constructor { constructor = Constructor c ; element } when (String.equal c "true" || String.equal c "false")
     && element.expression_content = Ast_typed.e_unit () -> ok @@ V_Ct (C_bool (bool_of_string c))
    | E_constructor { constructor = Constructor c ; element } ->
      let%bind v' = eval element env in
      ok @@ V_Construct (c,v')
    | E_matching { matchee ; cases} -> (
      let%bind e' = eval matchee env in
      match cases, e' with
      | Match_list cases , V_List [] ->
        eval cases.match_nil env
      | Match_list cases , V_List (head::tail) ->
        let {hd;tl;body;tv=_} = cases.match_cons in
        let env' = Env.extend (Env.extend env (hd,head)) (tl, V_List tail) in
        eval body env'
      | Match_variant {cases=[{constructor=Constructor t;body=match_true};{constructor=Constructor f; body=match_false}];_}, V_Ct (C_bool b)
        when String.equal t "true" && String.equal f "false" ->
        if b then eval match_true env
        else eval match_false env
      | Match_variant {cases ; tv=_} , V_Construct (matched_c , proj) ->
        let {constructor=_ ; pattern ; body} =
          List.find
            (fun {constructor = (Constructor c) ; pattern=_ ; body=_} ->
              String.equal matched_c c)
            cases in
        let env' = Env.extend env (pattern, proj) in
        eval body env'
      | Match_option cases, V_Construct ("Some" , proj) ->
        let {opt;body;tv=_} = cases.match_some in
        let env' = Env.extend env (opt,proj) in
        eval body env'
      | Match_option cases, V_Construct ("None" , V_Ct C_unit) ->
        eval cases.match_none env
      | _ -> failwith "not yet supported case"
        (* ((ctor,name),body) *)
    )
    | E_recursive {fun_name; fun_type=_; lambda} ->
      ok @@ V_Func_rec (fun_name, lambda.binder, lambda.result, env)
    | E_raw_code _ -> failwith "Can't evaluate a raw code insertion"

let eval : Ast_typed.program -> (string , _) result =
  fun prg ->
  let aux  (pp,top_env) el =
    match Location.unwrap el with
    | Ast_typed.Declaration_constant {binder; expr ; inline=_ ; _} ->
       let%bind v =
         (*TODO This TRY-CATCH is here until we properly implement effects*)
         try
           eval expr top_env
         with Temporary_hack s ->
           ok (V_Failure s)
              (*TODO This TRY-CATCH is here until we properly implement effects*)
       in
    let pp' = pp^"\n val "^(Var.to_name binder)^" = "^(Ligo_interpreter.PP.pp_value v) in
    let top_env' = Env.extend top_env (binder, v) in
    ok @@ (pp',top_env')
    | Ast_typed.Declaration_type _ ->
       ok (pp , top_env)
  in
  let%bind (res,_) = bind_fold_list aux
      ("",Env.empty_env) prg in
    ok @@ res
