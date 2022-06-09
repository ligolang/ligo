module Var = Simple_utils.Var
open Simple_utils.Trace
open Test_helpers
open Ast_imperative
open Main_errors

module Alpha_context = Memory_proto_alpha.Protocol.Alpha_context

let get_program = get_program "./contracts/timelock_repeat.mligo"

let compile_main ~raise ~add_warning () =
  Test_helpers.compile_main ~raise ~add_warning "./contracts/timelock_repeat.mligo" ()

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda_ez (ValueVar.of_input_var "arguments")
  ~ascr:(t_unit ()) (Some (t_list (t_operation ())))
  empty_op_list

let call msg = e_constructor "Call" msg
let mk_time ~raise st =
  match Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.of_string st with
  | Some s -> s
  | None -> raise.raise @@ test_internal "bad timestamp notation"
let to_sec t = Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.to_zint t
let storage st interval execute =
  e_record_ez [("next_use", e_timestamp_z (to_sec st)) ;
               ("interval", e_int interval) ;
               ("execute", execute)]

let early_call ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let now = mk_time ~raise "2000-01-01T00:10:10Z" in
  let lock_time = mk_time ~raise "2000-01-01T10:10:10Z" in
  let init_storage = storage lock_time 86400 empty_message in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ~now ()) in
  let exp_failwith = "You have to wait before you can execute this contract again." in
  expect_string_failwith ~raise ~add_warning ~options program "main"
    (e_pair (e_unit ())  init_storage) exp_failwith

let fake_decompiled_empty_message = e_string "[lambda of type: (lambda unit (list operation)) ]"

(* Test that when we use the contract the next use time advances by correct interval *)
let interval_advance ~raise ~add_warning () =
  let program = get_program ~raise ~add_warning () in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  let now = options.now in
  let lock_time = Alpha_context.Script_timestamp.add_delta now (Alpha_context.Script_int.of_int (-36_000)) in
  let init_storage = storage lock_time 86400 empty_message in
  let new_timestamp = Alpha_context.Script_timestamp.add_delta now (Alpha_context.Script_int.of_int 86_400) in
  let new_storage_fake = storage new_timestamp 86400 fake_decompiled_empty_message in
  expect_eq ~raise ~add_warning ~options program "main"
  (e_pair (e_unit ()) init_storage) (e_pair empty_op_list new_storage_fake)

let main = test_suite "Time Lock Repeating" [
    test_w "compile"          (compile_main    );
    test_w "early call"       (early_call      );
    test_w "interval advance" (interval_advance);
  ]
