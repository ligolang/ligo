open Test_helpers

let mfile = "./contracts/basic_multisig/multisig.mligo"
let compile_main ~raise f () = Test_helpers.compile_main ~raise f ()

let init_storage threshold counter pkeys =
  let open Ast_unified in
  let keys =
    List.map
      ~f:(fun el ->
        let _, pk_str, _ = str_keys el in
        e_key ~loc @@ pk_str)
      pkeys
  in
  e_record_ez
    ~loc
    [ "id", e_string ~loc "MULTISIG"
    ; "counter", e_nat ~loc counter
    ; "threshold", e_nat ~loc threshold
    ; "auth", e_list ~loc keys
    ]


let _, first_contract =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt, kt


let op_list ~raise =
  let open Memory_proto_alpha.Protocol.Alpha_context in
  let open Proto_alpha_utils in
  let source =
    Destination.Contract
      (Trace.trace_alpha_tzresult ~raise (fun _ -> Main_errors.test_internal __LOC__)
      @@ Contract.of_b58check "KT1DUMMYDUMMYDUMMYDUMMYDUMMYDUMu2oHG")
  in
  let destination =
    Trace.trace_tzresult ~raise (fun _ -> Main_errors.test_internal __LOC__)
    @@ Signature.Public_key_hash.of_b58check "tz1PpDGHRXFQq3sYDuH8EpLWzPm5PFpe1sLE"
  in
  let operation
      : _ Memory_proto_alpha.Protocol.Script_typed_ir.internal_operation_contents
    =
    Transaction_to_implicit { destination; amount = Tez.zero }
  in
  let internal_operation
      : Memory_proto_alpha.Protocol.Script_typed_ir.packed_internal_operation
    =
    Memory_proto_alpha.Protocol.Script_typed_ir.(
      Internal_operation { source; operation; nonce = 0 })
  in
  let opbytes =
    let contents =
      Memory_proto_alpha.Protocol.Apply_internal_results.packed_internal_operation
        internal_operation
    in
    Data_encoding.Binary.to_bytes_exn
      Memory_proto_alpha.Protocol.Apply_internal_results.internal_operation_encoding
      contents
  in
  Ast_unified.(e_list ~loc [ e_literal ~loc (Literal_operation opbytes) ])


let empty_payload = Ast_unified.e_unit ~loc

let chain_id_zero =
  Ast_unified.e_bytes_raw
    ~loc
    (Tezos_crypto.Hashed.Chain_id.to_bytes Tezos_base__TzPervasives.Chain_id.zero)


(* sign the message 'msg' with 'keys', if 'is_valid'=false the providid signature will be incorrect *)
let params ~raise counter payload keys is_validl f =
  let open Ast_unified in
  let prog = get_program ~raise f () in
  let aux acc (key, is_valid) =
    let _, _pk, sk = key in
    let pkh, _, _ = str_keys key in
    let msg =
      e_tuple ~loc
      @@ List.Ne.of_list
           [ payload
           ; e_nat ~loc counter
           ; e_string ~loc (if is_valid then "MULTISIG" else "XX")
           ; chain_id_zero
           ]
    in
    let signature = sign_message ~raise prog msg sk in
    e_pair ~loc (e_key_hash ~loc pkh) (e_signature ~loc signature) :: acc
  in
  let signed_msgs = List.fold ~f:aux ~init:[] (List.rev @@ List.zip_exn keys is_validl) in
  e_record_ez
    ~loc
    [ "counter", e_nat ~loc counter
    ; "payload", payload
    ; "signatures", e_list ~loc signed_msgs
    ]


(* Provide one valid signature when the threshold is two of two keys *)
let not_enough_1_of_2 ~raise f () =
  let program = get_program ~raise f () in
  let exp_failwith = "Not enough signatures passed the check" in
  let keys = gen_keys () in
  let test_params = params ~raise 0 empty_payload [ keys ] [ true ] f in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(
      make_options ~env:(test_environment ()) ~sender:first_contract ())
  in
  let () =
    expect_string_failwith_twice
      ~raise
      program
      ~options
      "main"
      test_params
      (init_storage 2 0 [ keys; gen_keys () ])
      exp_failwith
  in
  ()


let unmatching_counter ~raise f () =
  let program = get_program ~raise f () in
  let exp_failwith = "Counters does not match" in
  let keys = gen_keys () in
  let test_params = params ~raise 1 empty_payload [ keys ] [ true ] f in
  let () =
    expect_string_failwith_twice
      ~raise
      program
      "main"
      test_params
      (init_storage 1 0 [ keys ])
      exp_failwith
  in
  ()


(* Provide one invalid signature (correct key but incorrect signature)
   when the threshold is one of one key *)
let invalid_1_of_1 ~raise f () =
  let program = get_program ~raise f () in
  let exp_failwith = "Invalid signature" in
  let keys = [ gen_keys () ] in
  let test_params = params ~raise 0 empty_payload keys [ false ] f in
  let () =
    expect_string_failwith_twice
      ~raise
      program
      "main"
      test_params
      (init_storage 1 0 keys)
      exp_failwith
  in
  ()


(* Provide one valid signature when the threshold is one of one key *)
let valid_1_of_1 ~raise f () =
  let open Ast_unified in
  let program = get_program ~raise f () in
  let op_list = op_list ~raise in
  let keys = gen_keys () in
  let () =
    expect_eq_n_trace_aux_twice
      ~raise
      [ 0; 1; 2 ]
      program
      "main"
      (fun n ->
        let params = params ~raise n empty_payload [ keys ] [ true ] f in
        params, init_storage 1 n [ keys ])
      (fun n -> e_pair ~loc op_list (init_storage 1 (n + 1) [ keys ]))
  in
  ()


(* Provive two valid signatures when the threshold is two of three keys *)
let valid_2_of_3 ~raise f () =
  let open Ast_unified in
  let program = get_program ~raise f () in
  let op_list = op_list ~raise in
  let param_keys = [ gen_keys (); gen_keys () ] in
  let st_keys = param_keys @ [ gen_keys () ] in
  let () =
    expect_eq_n_trace_aux_twice
      ~raise
      [ 0; 1; 2 ]
      program
      "main"
      (fun n ->
        let params = params ~raise n empty_payload param_keys [ true; true ] f in
        params, init_storage 2 n st_keys)
      (fun n -> e_pair ~loc op_list (init_storage 2 (n + 1) st_keys))
  in
  ()


(* Provide one invalid signature and two valid signatures when the threshold is two of three keys *)
let invalid_3_of_3 ~raise f () =
  let program = get_program ~raise f () in
  let valid_keys = [ gen_keys (); gen_keys () ] in
  let invalid_key = gen_keys () in
  let param_keys = valid_keys @ [ invalid_key ] in
  let st_keys = valid_keys @ [ gen_keys () ] in
  let test_params = params ~raise 0 empty_payload param_keys [ false; true; true ] f in
  let exp_failwith = "Invalid signature" in
  let () =
    expect_string_failwith_twice
      ~raise
      program
      "main"
      test_params
      (init_storage 2 0 st_keys)
      exp_failwith
  in
  ()


(* Provide two valid signatures when the threshold is three of three keys *)
let not_enough_2_of_3 ~raise f () =
  let program = get_program ~raise f () in
  let valid_keys = [ gen_keys (); gen_keys () ] in
  let st_keys = gen_keys () :: valid_keys in
  let test_params = params ~raise 0 empty_payload valid_keys [ true; true ] f in
  let exp_failwith = "Not enough signatures passed the check" in
  let () =
    expect_string_failwith_twice
      ~raise
      program
      "main"
      test_params
      (init_storage 3 0 st_keys)
      exp_failwith
  in
  ()


let main =
  test_suite
    "Basic Multisig"
    [ test_w "compile (mligo)" (compile_main mfile)
    ; test_w "unmatching_counter (mligo)" (unmatching_counter mfile)
    ; test_w "valid_1_of_1 (mligo)" (valid_1_of_1 mfile)
    ; test_w "invalid_1_of_1 (mligo)" (invalid_1_of_1 mfile)
    ; test_w "not_enough_signature (mligo)" (not_enough_1_of_2 mfile)
    ; test_w "valid_2_of_3 (mligo)" (valid_2_of_3 mfile)
    ; test_w "invalid_3_of_3 (mligo)" (invalid_3_of_3 mfile)
    ; test_w "not_enough_2_of_3 (mligo)" (not_enough_2_of_3 mfile)
    ]
