module Var = Simple_utils.Var
open Test_helpers
open Ligo_prim
open Ast_imperative

let file = "./contracts/pledge.ligo"
let mfile = "./contracts/pledge.mligo"
let refile = "./contracts/pledge.religo"
let compile_main ~raise f () = Test_helpers.compile_main ~raise f ()

let oracle_addr, oracle_contract =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt, kt


let stranger_addr, stranger_contract =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 1 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt, kt


let empty_op_list = e_typed_list ~loc [] (t_operation ~loc ())

let empty_message =
  e_lambda_ez
    ~loc
    (Value_var.of_input_var ~loc "arguments")
    ~ascr:(t_unit ~loc ())
    (Some (t_list ~loc (t_operation ~loc ())))
    empty_op_list


let pledge ~raise f () =
  let program = get_program ~raise f () in
  let storage = e_address ~loc oracle_addr in
  let parameter = e_unit ~loc () in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(
      make_options
        ~env:(test_environment ())
        ~sender:oracle_contract
        ~amount:Memory_proto_alpha.Protocol.Alpha_context.Tez.one
        ())
  in
  expect_eq
    ~raise
    ~options
    program
    "donate"
    (e_pair ~loc parameter storage)
    (e_pair ~loc (e_list ~loc []) storage)


let distribute ~raise f () =
  let program = get_program ~raise f () in
  let storage = e_address ~loc oracle_addr in
  let parameter = empty_message in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(
      make_options ~env:(test_environment ()) ~sender:oracle_contract ())
  in
  expect_eq
    ~raise
    ~options
    program
    "distribute"
    (e_pair ~loc parameter storage)
    (e_pair ~loc (e_list ~loc []) storage)


let distribute_unauthorized ~raise f () =
  let program = get_program ~raise f () in
  let storage = e_address ~loc oracle_addr in
  let parameter = empty_message in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(
      make_options ~env:(test_environment ()) ~sender:stranger_contract ())
  in
  expect_string_failwith
    ~raise
    ~options
    program
    "distribute"
    (e_pair ~loc parameter storage)
    "You're not the oracle for this distribution."


let main =
  test_suite
    "Pledge & Distribute"
    [ test_w "donate" (pledge file)
    ; test_w "distribute" (distribute file)
    ; test_w "distribute (unauthorized)" (distribute_unauthorized file)
    ; test_w "donate" (pledge mfile)
    ; test_w "distribute" (distribute mfile)
    ; test_w "distribute (unauthorized)" (distribute_unauthorized mfile)
    ; test_w "donate" (pledge refile)
    ; test_w "distribute" (distribute refile)
    ; test_w "distribute (unauthorized)" (distribute_unauthorized refile)
    ]
