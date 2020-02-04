open Trace
open Ligo_interpreter.Types
include Stage_common.Types

module Env = Ligo_interpreter.Environment

let apply_comparison : Ast_typed.constant -> value list -> value result =
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
      ok @@ V_Ct (C_bool (f_op a' b'))

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
      ok @@ V_Ct (C_bool (f_op a' b'))

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
      ok @@ V_Ct (C_bool (f_op a' b'))
    | _ -> simple_fail "unsupported comparison"

(* applying those operators does not involve extending the environment *)
let rec apply_operator : Ast_typed.constant -> value list -> value result =
  fun c operands ->
  let return_ct v = ok @@ V_Ct v in
  let return_none () = ok @@ V_Construct ("None" , V_Ct C_unit) in
  let return_some v  = ok @@ V_Construct ("Some" , v) in
  ( match (c,operands) with
    (* nullary *)
    | ( C_NONE , [] ) -> return_none ()
    | ( C_UNIT , [] ) -> ok @@ V_Ct C_unit
    | ( C_NIL  , [] ) -> ok @@ V_List []
    (* unary *)
    | ( C_FAILWITH , [ V_Ct (C_string a') ] ) ->
      (*TODO This raise is here until we properly implement effects*)
      raise (Temprorary_hack a')
      (*TODO This raise is here until we properly implement effects*)
    | ( C_NOT    , [ V_Ct (C_bool a'  ) ] ) -> return_ct @@ C_bool (not a')
    | ( C_INT    , [ V_Ct (C_nat a')    ] ) -> return_ct @@ C_int a'
    | ( C_ABS    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (abs a')
    | ( C_NEG    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (-a')
    | ( C_SOME   , [ v                  ] ) -> return_some v
    | ( C_IS_NAT , [ V_Ct (C_int a')    ] ) ->
      if a' > 0 then return_some @@ V_Ct (C_nat a')
      else return_none ()
    (* binary *)
    | ( (C_EQ | C_NEQ | C_LT | C_LE | C_GT | C_GE) , _ ) -> apply_comparison c operands
    | ( C_SUB    , [ V_Ct (C_int a' | C_nat a') ; V_Ct (C_int b' | C_nat b') ] ) -> return_ct @@ C_int (a' - b')
    | ( C_CONS   , [ v ; V_List vl ] ) -> ok @@ V_List (v::vl)
    | ( C_ADD    , [ V_Ct (C_int a'  ) ; V_Ct (C_int b'  ) ] ) -> return_ct @@ C_int   (a' + b')
    | ( C_ADD    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) -> return_ct @@ C_nat   (a' + b')
    | ( C_MUL    , [ V_Ct (C_int a'  ) ; V_Ct (C_int b'  ) ] ) -> return_ct @@ C_int   (a' * b')
    | ( C_MUL    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) -> return_ct @@ C_nat   (a' * b')
    | ( C_MUL    , [ V_Ct (C_nat a'  ) ; V_Ct (C_mutez b') ] ) -> return_ct @@ C_mutez (a' * b')
    | ( C_MUL    , [ V_Ct (C_mutez a') ; V_Ct (C_mutez b') ] ) -> return_ct @@ C_mutez (a' * b')
    | ( C_DIV    , [ V_Ct (C_int a'  ) ; V_Ct (C_int b'  ) ] ) -> return_ct @@ C_int   (a' / b')
    | ( C_DIV    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) -> return_ct @@ C_nat   (a' / b')
    | ( C_DIV    , [ V_Ct (C_mutez a') ; V_Ct (C_nat b'  ) ] ) -> return_ct @@ C_mutez (a' / b')
    | ( C_DIV    , [ V_Ct (C_mutez a') ; V_Ct (C_mutez b') ] ) -> return_ct @@ C_nat   (a' / b')
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
    | ( C_LIST_ITER , [ V_Func_val (arg_name, body, env) ; V_List (elts) ] ) ->
      bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env (arg_name,elt) in
          eval body env'
        )
        (V_Ct C_unit) elts
    (* tertiary *)
    | ( C_LIST_FOLD , [ V_Func_val (arg_name, body, env) ; V_List (elts) ; init ] ) ->
      bind_fold_list
        (fun prev elt ->
          let fold_args = V_Record (LMap.of_list [(Label "0",prev) ; (Label "1",elt)]) in
          let env' = Env.extend env (arg_name,  fold_args) in
          eval body env'
        )
        init elts
    | _ ->
      let () = Format.printf "%a\n" Stage_common.PP.constant c in
      let () = List.iter ( fun e -> Format.printf "%s\n" (Ligo_interpreter.PP.pp_value e)) operands in
      simple_fail "Unsupported constant op"
  )

(*
| C_NOW
| C_ASSERTION
| C_ASSERT_INFERRED
| C_UPDATE
| C_ITER
| C_FOLD_WHILE
| C_CONTINUE
| C_STOP
| C_FOLD
| C_SUB
| C_MOD
| C_SIZE
| C_SLICE
| C_BYTES_PACK
| C_BYTES_UNPACK
| C_PAIR
X| C_CAR
X| C_CDR
X| C_LEFT
X| C_RIGHT
| C_SET_EMPTY
| C_SET_LITERAL
| C_SET_ADD
| C_SET_REMOVE
| C_SET_ITER
| C_SET_FOLD
| C_SET_MEM
| C_MAP
| C_MAP_EMPTY
| C_MAP_LITERAL
| C_MAP_GET
| C_MAP_GET_FORCE
| C_MAP_ADD
| C_MAP_REMOVE
| C_MAP_UPDATE
| C_MAP_ITER
| C_MAP_MAP
| C_MAP_FOLD
| C_MAP_MEM
| C_MAP_FIND
| C_MAP_FIND_OPT
| C_BIG_MAP
| C_BIG_MAP_EMPTY
| C_BIG_MAP_LITERAL
x| C_LIST_CONS -> To remove ? seems unused
| C_SHA256
| C_SHA512
| C_BLAKE2b
| C_HASH
| C_HASH_KEY
| C_CHECK_SIGNATURE
| C_CHAIN_ID
| C_CALL
| C_CONTRACT
| C_CONTRACT_ENTRYPOINT
| C_AMOUNT
| C_BALANCE
| C_SOURCE
| C_SENDER
| C_ADDRESS
| C_SELF_ADDRESS
| C_IMPLICIT_ACCOUNT
| C_SET_DELEGATE
| C_STEPS_TO_QUOTA
*)

(*interpreter*)
and eval_literal : Ast_typed.literal -> value result = function
  | Literal_unit     -> ok @@ V_Ct (C_unit)
  | Literal_int i    -> ok @@ V_Ct (C_int i)
  | Literal_nat n    -> ok @@ V_Ct (C_nat n)
  | Literal_string s -> ok @@ V_Ct (C_string s)
  | Literal_bytes s  -> ok @@ V_Ct (C_bytes s)
  | Literal_bool b   -> ok @@ V_Ct (C_bool b)
  | Literal_mutez t  -> ok @@ V_Ct (C_mutez t)
  | _ -> simple_fail "Unsupported literal"

and eval : Ast_typed.expression -> env -> value result
  = fun term env ->
    match term with
    | E_application ({expression = f; _}, args) -> (
      let%bind f' = match f with
        | E_variable f -> Env.lookup env f
        | _ -> eval f env in
      match f' with
      | V_Func_val (arg_names, body, f_env) ->
        let%bind args' = eval args.expression env in
        let f_env' = Env.extend f_env (arg_names, args') in
        eval body f_env'
      | _ -> simple_fail "trying to apply on something that is not a function"
    )
    | E_lambda { binder; body;} ->
      ok @@ V_Func_val (binder,body.expression,env)
    | E_let_in { binder; rhs; result; _} ->
      let%bind rhs' = eval rhs.expression env in
      eval result.expression (Env.extend env (binder,rhs'))
    | E_map kvlist | E_big_map kvlist ->
      let%bind kvlist' = bind_map_list
        (fun kv -> bind_map_pair (fun (el:Ast_typed.annotated_expression) -> eval el.expression env) kv)
        kvlist in
      ok @@ V_Map kvlist'
    | E_list expl ->
      let%bind expl' = bind_map_list
        (fun (exp:Ast_typed.annotated_expression) -> eval exp.expression env)
        expl in
      ok @@ V_List expl'
    | E_literal l ->
      eval_literal l
    | E_variable var ->
      Env.lookup env var
    | E_record recmap ->
      let%bind lv' = bind_map_list
        (fun (label,(v:Ast_typed.annotated_expression)) ->
          let%bind v' = eval v.expression env in
          ok (label,v'))
        (LMap.to_kv_list recmap) in
      ok @@ V_Record (LMap.of_list lv')
    | E_record_accessor (record,label) -> (
      let%bind record' = eval record.expression env in
      match record' with
      | V_Record recmap ->
        let%bind a = trace_option (simple_error "unknown record field") @@
          LMap.find_opt label recmap in
        ok a
      | _ -> simple_fail "trying to access a non-record"
    )
    | E_record_update (record, (l,field)) -> (
      let%bind record' = eval record.expression env in
      match record' with
      | V_Record recmap ->
        if LMap.mem l recmap then
          let%bind field' = eval field.expression env in
          ok @@ V_Record (LMap.add l field' recmap)
        else
          simple_fail "field l does not exist in record"
      | _ -> simple_fail "this expression isn't a record"
    )
    | E_constant (op, operands) -> (
      let%bind operands' = bind_map_list
        (fun (ae:Ast_typed.annotated_expression) -> eval ae.expression env)
        operands in
      apply_operator op operands'
    )
    | E_constructor (Constructor c, v) ->
      let%bind v' = eval v.expression env in
      ok @@ V_Construct (c,v')
    | E_matching (e , cases) -> (
      let%bind e' = eval e.expression env in
      match cases, e' with
      | Match_list cases , V_List [] ->
        eval cases.match_nil.expression env
      | Match_list cases , V_List (head::tail) ->
        let (head_var,tail_var,body,_) = cases.match_cons in
        let env' = Env.extend (Env.extend env (head_var,head)) (tail_var, V_List tail) in
        eval body.expression env'
      | Match_variant (case_list , _) , V_Construct (matched_c , proj) ->
        let ((_, var) , body) =
          List.find
            (fun case ->
              let (Constructor c , _) = fst case in
              String.equal matched_c c)
            case_list in
        let env' = Env.extend env (var, proj) in
        eval body.expression env'
      | Match_bool cases , V_Ct (C_bool true) ->
        eval cases.match_true.expression env
      | Match_bool cases , V_Ct (C_bool false) ->
        eval cases.match_false.expression env
      | Match_option cases, V_Construct ("Some" , proj) ->
        let (var,body,_) = cases.match_some in
        let env' = Env.extend env (var,proj) in
        eval body.expression env'
      | Match_option cases, V_Construct ("None" , V_Ct C_unit) ->
        eval cases.match_none.expression env
      | _ -> simple_fail "not yet supported case"
        (* ((ctor,name),body) *)
    )
    (**********************************************
    This is not necessary after Ast simplification
    ***********************************************)
    | E_tuple el ->
      let%bind lv = bind_mapi_list
        (fun i (el:Ast_typed.annotated_expression) ->
          let%bind el' = eval el.expression env in
          ok (Label (string_of_int i), el'))
        el in
      ok @@ V_Record (LMap.of_list lv)
    | E_tuple_accessor (tuple,i) -> (
      let%bind record' = eval tuple.expression env in
      match record' with
      | V_Record recmap ->
        let label = Label (string_of_int i) in
        let%bind a = trace_option (simple_error "out of tuple range") @@
          LMap.find_opt label recmap in
        ok a
      | _ -> simple_fail "trying to access a non-record"
    )
    (**********************************************
    This is not necessary after Ast simplification
    ***********************************************)
    | E_look_up _ | E_loop _ | E_set _ | E_sequence _ | E_assign  _->
      let serr = Format.asprintf "Unsupported construct :\n %a\n" Ast_typed.PP.expression term in
      simple_fail serr

let dummy : Ast_typed.program -> string result =
  fun prg ->
    let%bind (res,_) = bind_fold_list
      (fun (pp,top_env) el ->
        let (Ast_typed.Declaration_constant (named_exp, _, _)) = Location.unwrap el in
        let%bind v =
        (*TODO This TRY-CATCH is here until we properly implement effects*)
        try
          eval named_exp.annotated_expression.expression top_env
        with Temprorary_hack s -> ok @@ V_Failure s
        (*TODO This TRY-CATCH is here until we properly implement effects*)
        in
        let pp' = pp^"\n val "^(Var.to_name named_exp.name)^" = "^(Ligo_interpreter.PP.pp_value v) in
        let top_env' = Env.extend top_env (named_exp.name, v) in
        ok @@ (pp',top_env')
      )
      ("",Env.empty_env) prg in
    ok @@ res
