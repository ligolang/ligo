open Trace
open Test_helpers

let get_program = get_program "./contracts/replaceable_id.ligo" (Contract "main")
      
let compile_main () =
  let* typed_prg,_     = get_program () in
  let* mini_c_prg      = Ligo_compile.Of_typed.compile typed_prg in
  let* michelson_prg   = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~options mini_c_prg "main" in
  let* _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract michelson_prg in
  ok ()

open Ast_imperative

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda_ez (Location.wrap @@ Var.of_name "arguments")
  ~ascr:(t_unit ()) (Some (t_list (t_operation ())))
  empty_op_list

let storage id = e_address @@ addr id
let entry_change_addr id = e_constructor "Change_address"
  @@ e_address @@ addr @@ id
let entry_pass_message = e_constructor "Pass_message"
  @@ empty_message

let change_addr_success () =
  let* (program, env) = get_program () in
  let init_storage = storage 1 in
  let param = entry_change_addr 2 in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  expect_eq ~options (program,env) "main"
    (e_pair param init_storage) (e_pair empty_op_list (storage 2))

let change_addr_fail () =
  let* (program,env) = get_program () in
  let init_storage = storage 1 in
  let param = entry_change_addr 2 in
  let options =
    let sender = contract 3 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  let exp_failwith = "Unauthorized sender" in
  expect_string_failwith ~options (program,env) "main"
    (e_pair param init_storage) exp_failwith

let pass_message_success () =
  let* (program,env) = get_program () in
  let init_storage = storage 1 in
  let param = entry_pass_message in
  let options =
    let sender = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  expect_eq ~options (program,env) "main"
    (e_pair param init_storage) (e_pair empty_op_list init_storage)

let pass_message_fail () =
  let* (program,env) = get_program () in
  let init_storage = storage 1 in
  let param = entry_pass_message in
  let options =
    let sender = contract 2 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~sender () in
  let exp_failwith = "Unauthorized sender" in
  expect_string_failwith ~options (program,env) "main"
    (e_pair param init_storage) exp_failwith

let main = test_suite "Replaceable ID" [
    test "compile"              compile_main         ;
    test "change_addr_success"  change_addr_success  ;
    test "change_addr_fail"     change_addr_fail     ;
    test "pass_message_success" pass_message_success ;
    test "pass_message_fail"    pass_message_fail    ;
  ]
