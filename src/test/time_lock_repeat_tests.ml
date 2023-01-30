module Var = Simple_utils.Var
open Simple_utils.Trace
open Test_helpers
open Ligo_prim
open Ast_imperative
open Main_errors
module Alpha_context = Memory_proto_alpha.Protocol.Alpha_context

let get_program = get_program "./contracts/timelock_repeat.mligo"

let compile_main ~raise () =
  Test_helpers.compile_main ~raise "./contracts/timelock_repeat.mligo" ()


let empty_op_list = e_typed_list ~loc [] (t_operation ~loc ())

let empty_message =
  e_lambda_ez
    ~loc
    (Value_var.of_input_var ~loc "arguments")
    ~ascr:(t_unit ~loc ())
    (Some (t_list ~loc (t_operation ~loc ())))
    empty_op_list


let call msg = e_constructor ~loc "Call" msg

let mk_time ~raise st =
  match Memory_proto_alpha.Protocol.Script_timestamp.of_string st with
  | Some s -> s
  | None -> raise.error @@ test_internal "bad timestamp notation"


let to_sec t = Memory_proto_alpha.Protocol.Script_timestamp.to_zint t

let storage st interval execute =
  e_record_ez
    ~loc
    [ "next_use", e_timestamp_z ~loc (to_sec st)
    ; "interval", e_int ~loc interval
    ; "execute", execute
    ]


let early_call ~raise () =
  let program = get_program ~raise () in
  let now = mk_time ~raise "2000-01-01T00:10:10Z" in
  let lock_time = mk_time ~raise "2000-01-01T10:10:10Z" in
  let init_storage = storage lock_time 86400 empty_message in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ~now ())
  in
  let exp_failwith = "You have to wait before you can execute this contract again." in
  expect_string_failwith_twice
    ~raise
    ~options
    program
    "main"
    (e_unit ~loc ())
    init_storage
    exp_failwith


let fake_decompiled_empty_message =
  e_string ~loc "[lambda of type: (lambda unit (list operation)) ]"


(* Test that when we use the contract the next use time advances by correct interval *)
let interval_advance ~raise () =
  let program = get_program ~raise () in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ())
  in
  let now = options.now in
  let lock_time =
    Memory_proto_alpha.Protocol.(
      Script_timestamp.add_delta now (Script_int.of_int (-36_000)))
  in
  let init_storage = storage lock_time 86400 empty_message in
  let new_timestamp =
    Memory_proto_alpha.Protocol.(
      Script_timestamp.add_delta now (Script_int.of_int 86_400))
  in
  let new_storage_fake = storage new_timestamp 86400 fake_decompiled_empty_message in
  expect_eq_twice
    ~raise
    ~options
    program
    "main"
    (e_unit ~loc ())
    init_storage
    (e_pair ~loc empty_op_list new_storage_fake)


let main =
  test_suite
    "Time Lock Repeating"
    [ test_w "compile" compile_main
    ; test_w "early call" early_call
    ; test_w "interval advance" interval_advance
    ]
