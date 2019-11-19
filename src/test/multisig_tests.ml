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

let gen_keys = fun () ->
  let open Tezos_crypto in
  let (_,raw_pk,raw_sk) = Ed25519.generate_key () in
  (raw_pk,raw_sk)

let str_keys (raw_pk, raw_sk) =
  let open Tezos_crypto in
  let (pk_str:string) = Base58.simple_encode (Ed25519.Public_key.b58check_encoding) raw_pk in
  let (sk_str:string) = Base58.simple_encode (Ed25519.Secret_key.b58check_encoding) raw_sk in
  (pk_str,sk_str)

let sign_message (msg : expression) raw_sk : string result =
  let open Tezos_crypto in
  let (sk : Signature.secret_key) = Signature.Ed25519 raw_sk in
  let%bind program,_ = get_program () in
  let%bind (msg : Tezos_utils.Michelson.michelson) =
    let env = Ast_typed.program_environment program in
    Ligo.Run.Of_simplified.compile_expression ~env ~state:(Typer.Solver.initial_state) msg
  in
  let%bind msg' = Ligo.Run.Of_michelson.pack_message_lambda msg in
  let (signed_data:Signature.t) = Signature.sign sk msg' in
  let signature_str = Signature.to_b58check signed_data in
  ok signature_str

let init_storage threshold counter pkeys =
  let keys = List.map (fun el -> e_key @@ fst @@ str_keys el) pkeys in
  ez_e_record [
    ("counter" , e_nat counter ) ;
    ("threshold" , e_nat threshold) ;
    ("auth" , e_typed_list keys t_key ) ;
  ]

let empty_op_list = 
  (e_typed_list [] t_operation)
let empty_message = e_lambda "arguments"
  (Some t_unit) (Some (t_list t_operation))
  empty_op_list

(* sign the same message 'msg' with the secret keys of 'keys' *)
let params counter msg keys = 
  let aux = fun acc sk ->
    let%bind signature = sign_message msg (snd sk) in
    ok @@ (e_signature signature)::acc in
  let%bind signed_msgs = Trace.bind_fold_list aux [] keys in
  ok @@ e_constructor
    "CheckMessage"
    (ez_e_record [
      ("counter" , e_nat counter ) ;
      ("message" , msg) ;
      ("signatures" , e_typed_list signed_msgs t_signature ) ;
    ])


(* Provide one valid signature when the threshold is two of two keys *)
let not_enough_1_of_2 () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Not enough signatures passed the check" in
  let keys = gen_keys () in
  let%bind test_params = params 0 empty_message [keys] in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 2 0 [keys;gen_keys()])) exp_failwith in
  ok ()

let unmatching_counter () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Counters does not match" in
  let keys = gen_keys () in
  let%bind test_params = params 1 empty_message [keys] in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 1 0 [keys])) exp_failwith in
  ok ()

(* Provide one invalid signature when the threshold is one of one key *)
let invalid_1_of_1 () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Invalid signature" in
  let keys = gen_keys () in
  let invalid_keys = gen_keys () in
  let%bind test_params = params 0 empty_message [keys] in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 1 0 [invalid_keys])) exp_failwith in
  ok ()

(* Provide one valid signature when the threshold is one of one key *)
let valid_1_of_1 () =
  let%bind program,_ = get_program () in
  let keys = gen_keys () in
  let%bind () = expect_eq_n_trace_aux [0;1;2] program "main"
      (fun n ->
        let%bind params = params n empty_message [keys] in
        ok @@ e_pair params (init_storage 1 n [keys])
      )
      (fun n ->
        ok @@ e_pair empty_op_list (init_storage 1 (n+1) [keys])
      ) in
  ok ()

(* Provive two valid signatures when the threshold is two of three keys *)
let valid_2_of_3 () =
  let%bind program,_ = get_program () in
  let keys = [gen_keys (); gen_keys ()] in
  let st_keys = gen_keys() :: keys in
  let%bind () = expect_eq_n_trace_aux [0;1;2] program "main"
      (fun n ->
        let%bind params = params n empty_message keys in
        ok @@ e_pair params (init_storage 2 n st_keys)
      )
      (fun n ->
        ok @@ e_pair empty_op_list (init_storage 2 (n+1) st_keys)
      ) in
  ok ()

(* Provide one invalid signature and two valid signatures when the threshold is two of three keys *)
let invalid_3_of_2 () =
  let%bind program,_ = get_program () in
  let valid_keys = [gen_keys() ; gen_keys()] in
  let invalid_key = gen_keys () in
  let st_keys = gen_keys () :: valid_keys  in
  let%bind test_params = params 0 empty_message (invalid_key::valid_keys) in
  let exp_failwith = "Invalid signature" in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 2 0 st_keys)) exp_failwith in
  ok ()

(* Provide two valid signatures when the threshold is three of three keys *)
let not_enough_2_of_3 () =
  let%bind program,_ = get_program () in
  let valid_keys = [gen_keys() ; gen_keys()] in
  let st_keys = gen_keys () :: valid_keys  in
  let%bind test_params = params 0 empty_message (valid_keys) in
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
    test "invalid_3_of_3"       invalid_3_of_2     ;
    test "not_enough_2_of_3"    not_enough_2_of_3  ;
  ]
