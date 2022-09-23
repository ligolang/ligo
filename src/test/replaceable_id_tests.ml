module Var = Simple_utils.Var
open Test_helpers

let get_program = get_program "./contracts/replaceable_id.ligo"

let compile_main ~raise () =
  Test_helpers.compile_main ~raise "./contracts/replaceable_id.ligo" ()

open Ligo_prim
open Ast_imperative

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda_ez (Value_var.of_input_var "arguments")
  ~ascr:(t_unit ()) (Some (t_list (t_operation ())))
  empty_op_list

let storage id = e_address @@ addr id
let entry_change_addr id = e_constructor "Change_address"
  @@ e_address @@ addr @@ id
let entry_pass_message = e_constructor "Pass_message"
  @@ empty_message

let change_addr_success ~raise () =
  let program = get_program ~raise () in
  let init_storage = storage 1 in
  let param = entry_change_addr 2 in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ~sender ()) in
  expect_eq ~raise ~options program "main"
    (e_pair param init_storage) (e_pair empty_op_list (storage 2))

let change_addr_fail ~raise () =
  let program = get_program ~raise () in
  let init_storage = storage 1 in
  let param = entry_change_addr 2 in
  let options =
    let sender = contract 3 in
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ~sender ()) in
  let exp_failwith = "Unauthorized sender" in
  expect_string_failwith ~raise ~options program "main"
    (e_pair param init_storage) exp_failwith

let pass_message_success ~raise () =
  let program = get_program ~raise () in
  let init_storage = storage 1 in
  let param = entry_pass_message in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ~sender ()) in
  expect_eq ~raise ~options program "main"
    (e_pair param init_storage) (e_pair empty_op_list init_storage)

let pass_message_fail ~raise () =
  let program = get_program ~raise () in
  let init_storage = storage 1 in
  let param = entry_pass_message in
  let options =
    let sender = contract 2 in
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ~sender ()) in
  let exp_failwith = "Unauthorized sender" in
  expect_string_failwith ~raise ~options program "main"
    (e_pair param init_storage) exp_failwith

let main = test_suite "Replaceable ID" [
    test_w "compile"              (compile_main        ) ;
    test_w "change_addr_success"  (change_addr_success ) ;
    test_w "change_addr_fail"     (change_addr_fail    ) ;
    test_w "pass_message_success" (pass_message_success) ;
    test_w "pass_message_fail"    (pass_message_fail   ) ;
  ]
