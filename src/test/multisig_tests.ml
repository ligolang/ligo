module Var = Simple_utils.Var
open Test_helpers

let mfile = "./contracts/multisig.mligo"
let compile_main ~raise f () = Test_helpers.compile_main ~raise f ()

open Ligo_prim
open Ast_unified

let init_storage threshold counter pkeys =
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


let empty_op_list = e_list ~loc []

let empty_message =
  e_lambda_ez
    ~loc
    (Value_var.of_input_var ~loc "arguments")
    ~ascr:(tv_unit ~loc ())
    (Some (t_list ~loc (tv_operation ~loc ())))
    empty_op_list


let chain_id_zero =
  e_chain_id ~loc
  @@ Tezos_crypto.Base58.simple_encode
       Tezos_base__TzPervasives.Chain_id.b58check_encoding
       Tezos_base__TzPervasives.Chain_id.zero


(* sign the message 'msg' with 'keys', if 'is_valid'=false the providid signature will be incorrect *)
let params ~raise counter msg keys is_validl f =
  Lwt_main.run
  @@
  let open Lwt.Let_syntax in
  let program = get_program ~raise f () in
  let aux acc (key, is_valid) =
    let _, _pk, sk = key in
    let pkh, _, _ = str_keys key in
    let payload =
      e_tuple
        ~loc
        [ msg
        ; e_nat ~loc counter
        ; e_string ~loc (if is_valid then "MULTISIG" else "XX")
        ; chain_id_zero
        ]
    in
    let%map signature = sign_message ~raise program payload sk in
    e_pair ~loc (e_key_hash ~loc pkh) (e_signature ~loc signature) :: acc
  in
  let%map signed_msgs =
    Lwt_list.fold_left_s aux [] (List.rev @@ List.zip_exn keys is_validl)
  in
  e_applied_constructor
    ~loc
    { constructor = Label.of_string "CheckMessage"
    ; element =
        e_record_ez
          ~loc
          [ "counter", e_nat ~loc counter
          ; "message", msg
          ; "signatures", e_list ~loc signed_msgs
          ]
    }


(* Provide one valid signature when the threshold is two of two keys *)
let not_enough_1_of_2 ~raise f () =
  let program = get_program ~raise f () in
  let exp_failwith = "Not enough signatures passed the check" in
  let keys = gen_keys () in
  let test_params = params ~raise 0 empty_message [ keys ] [ true ] f in
  let () =
    expect_string_failwith_twice
      ~raise
      program
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
  let test_params = params ~raise 1 empty_message [ keys ] [ true ] f in
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
  let test_params = params ~raise 0 empty_message keys [ false ] f in
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
  let program = get_program ~raise f () in
  let keys = gen_keys () in
  let () =
    expect_eq_n_trace_aux_twice
      ~raise
      [ 0; 1; 2 ]
      program
      "main"
      (fun n ->
        let params = params ~raise n empty_message [ keys ] [ true ] f in
        params, init_storage 1 n [ keys ])
      (fun n -> e_pair ~loc empty_op_list (init_storage 1 (n + 1) [ keys ]))
  in
  ()


(* Provide two valid signatures when the threshold is two of three keys *)
let valid_2_of_3 ~raise f () =
  let program = get_program ~raise f () in
  let param_keys = [ gen_keys (); gen_keys () ] in
  let st_keys = param_keys @ [ gen_keys () ] in
  let () =
    expect_eq_n_trace_aux_twice
      ~raise
      [ 0; 1; 2 ]
      program
      "main"
      (fun n ->
        let params = params ~raise n empty_message param_keys [ true; true ] f in
        params, init_storage 2 n st_keys)
      (fun n -> e_pair ~loc empty_op_list (init_storage 2 (n + 1) st_keys))
  in
  ()


(* Provide one invalid signature and two valid signatures when the threshold is two of three keys *)
let invalid_3_of_3 ~raise f () =
  let program = get_program ~raise f () in
  let valid_keys = [ gen_keys (); gen_keys () ] in
  let invalid_key = gen_keys () in
  let param_keys = valid_keys @ [ invalid_key ] in
  let st_keys = valid_keys @ [ gen_keys () ] in
  let test_params = params ~raise 0 empty_message param_keys [ false; true; true ] f in
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
  let test_params = params ~raise 0 empty_message valid_keys [ true; true ] f in
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
    "Multisig"
    [ test_w "compile (mligo)" (compile_main mfile)
    ; test_w "unmatching_counter (mligo)" (unmatching_counter mfile)
    ; test_w "valid_1_of_1 (mligo)" (valid_1_of_1 mfile)
    ; test_w "invalid_1_of_1 (mligo)" (invalid_1_of_1 mfile)
    ; test_w "not_enough_signature (mligo)" (not_enough_1_of_2 mfile)
    ; test_w "valid_2_of_3 (mligo)" (valid_2_of_3 mfile)
    ; test_w "invalid_3_of_3 (mligo)" (invalid_3_of_3 mfile)
    ; test_w "not_enough_2_of_3 (mligo)" (not_enough_2_of_3 mfile)
    ]
