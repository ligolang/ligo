open Trace
open Test_helpers

let type_file = Ligo.Compile.Of_source.type_file (Syntax_name "pascaligo")

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/multisig.ligo" in
        s := Some program ;
        ok program
      )

let compile_main () = 
  let%bind program,_ = get_program () in
  let%bind () =
    Ligo.Run.Of_simplified.compile_program
    program "main" in
  ok ()

open Ast_simplified

let init_storage threshold counter pkeys =
  let keys = List.map
    (fun el ->
      let (_,pk_str,_) = str_keys el in
      e_key @@ pk_str) 
    pkeys in
  ez_e_record [
    ("id" , e_string "MULTISIG" ) ;
    ("counter" , e_nat counter ) ;
    ("threshold" , e_nat threshold) ;
    ("auth" , e_typed_list keys t_key ) ;
  ]

let empty_op_list = 
  (e_typed_list [] t_operation)
let empty_message = e_lambda "arguments"
  (Some t_unit) (Some (t_list t_operation))
  empty_op_list
let chain_id_zero = e_chain_id @@ Tezos_crypto.Base58.simple_encode
  Tezos_base__TzPervasives.Chain_id.b58check_encoding
  Tezos_base__TzPervasives.Chain_id.zero

(* sign the message 'msg' with 'keys', if 'is_valid'=false the providid signature will be incorrect *)
let params counter msg keys is_validl  = 
  let%bind program,_ = get_program () in
  let aux = fun acc (key,is_valid) ->
    let (_,_pk,sk) = key in
    let (pkh,_,_) = str_keys key in
    let payload = e_tuple
      [ msg ; 
        e_nat counter ; 
        e_string (if is_valid then "MULTISIG" else "XX") ; 
        chain_id_zero ] in
    let%bind signature = sign_message program payload sk in
    ok @@ (e_pair (e_key_hash pkh) (e_signature signature))::acc in
  let%bind signed_msgs = Trace.bind_fold_list aux [] (List.rev @@ List.combine keys is_validl) in
  ok @@ e_constructor
    "CheckMessage"
    (ez_e_record [
      ("counter" , e_nat counter ) ;
      ("message" , msg) ;
      ("signatures" , e_typed_list signed_msgs (t_pair (t_key_hash,t_signature)) ) ;
    ])

(* Provide one valid signature when the threshold is two of two keys *)
let not_enough_1_of_2 () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Not enough signatures passed the check" in
  let keys = gen_keys () in
  let%bind test_params = params 0 empty_message [keys] [true] in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 2 0 [keys;gen_keys()])) exp_failwith in
  ok ()

let unmatching_counter () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Counters does not match" in
  let keys = gen_keys () in
  let%bind test_params = params 1 empty_message [keys] [true] in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 1 0 [keys])) exp_failwith in
  ok ()

(* Provide one invalid signature (correct key but incorrect signature)
   when the threshold is one of one key *)
let invalid_1_of_1 () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Invalid signature" in
  let keys = [gen_keys ()] in
  let%bind test_params = params 0 empty_message keys [false] in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 1 0 keys)) exp_failwith in
  ok ()

(* Provide one valid signature when the threshold is one of one key *)
let valid_1_of_1 () =
  let%bind program,_ = get_program () in
  let keys = gen_keys () in
  let%bind () = expect_eq_n_trace_aux [0;1;2] program "main"
      (fun n ->
        let%bind params = params n empty_message [keys] [true] in
        ok @@ e_pair params (init_storage 1 n [keys])
      )
      (fun n ->
        ok @@ e_pair empty_op_list (init_storage 1 (n+1) [keys])
      ) in
  ok ()

(* Provive two valid signatures when the threshold is two of three keys *)
let valid_2_of_3 () =
  let%bind program,_ = get_program () in
  let param_keys = [gen_keys (); gen_keys ()] in
  let st_keys = param_keys @ [gen_keys ()] in
  let%bind () = expect_eq_n_trace_aux [0;1;2] program "main"
      (fun n ->
        let%bind params = params n empty_message param_keys [true;true] in
        ok @@ e_pair params (init_storage 2 n st_keys)
      )
      (fun n ->
        ok @@ e_pair empty_op_list (init_storage 2 (n+1) st_keys)
      ) in
  ok ()

(* Provide one invalid signature and two valid signatures when the threshold is two of three keys *)
let invalid_3_of_3 () =
  let%bind program,_ = get_program () in
  let valid_keys = [gen_keys() ; gen_keys()] in
  let invalid_key = gen_keys () in
  let param_keys = valid_keys @ [invalid_key] in
  let st_keys = valid_keys @ [gen_keys ()] in
  let%bind test_params = params 0 empty_message param_keys [false;true;true] in
  let exp_failwith = "Invalid signature" in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 2 0 st_keys)) exp_failwith in
  ok ()

(* Provide two valid signatures when the threshold is three of three keys *)
let not_enough_2_of_3 () =
  let%bind program,_ = get_program () in
  let valid_keys = [gen_keys() ; gen_keys()] in
  let st_keys = gen_keys () :: valid_keys  in
  let%bind test_params = params 0 empty_message (valid_keys) [true;true] in
  let exp_failwith = "Not enough signatures passed the check" in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 3 0 st_keys)) exp_failwith in
  ok ()

let main = test_suite "Multisig" [
    test "compile"              compile_main       ;
    test "unmatching_counter"   unmatching_counter ;
    test "valid_1_of_1"         valid_1_of_1       ;
    test "invalid_1_of_1"       invalid_1_of_1     ;
    test "not_enough_signature" not_enough_1_of_2  ;
    test "valid_2_of_3"         valid_2_of_3       ;
    test "invalid_3_of_3"       invalid_3_of_3     ;
    test "not_enough_2_of_3"    not_enough_2_of_3  ;
  ]
