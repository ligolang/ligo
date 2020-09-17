open Trace
open Main_errors
open Test_helpers

let type_file f = 
  let%bind typed,state = Ligo.Compile.Utils.type_file f "pascaligo" (Contract "main") in
  ok @@ (typed,state)

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/time-lock.ligo" in
        s := Some program ;
        ok program
      )

let compile_main () = 
  let%bind typed_prg,_     =  type_file "./contracts/time-lock.ligo" in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

open Ast_imperative

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda (Location.wrap @@ Var.of_name "arguments",t_unit ()) 
  @@ e_annotation empty_op_list (t_list (t_operation ()))


let call msg = e_constructor "Call" msg
let mk_time st =
  match Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.of_string st with
  | Some s -> ok s
  | None -> fail @@ test_internal "bad timestamp notation"
let to_sec t = Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.to_zint t
let storage st = e_timestamp_z (to_sec st)

let early_call () =
  let%bind (program , state) = get_program () in
  let%bind now = mk_time "2000-01-01T00:10:10Z" in
  let%bind lock_time = mk_time "2000-01-01T10:10:10Z" in
  let init_storage = storage lock_time in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options ~now () in
  let exp_failwith = "Contract is still time locked" in
  expect_string_failwith ~options (program, state) "main"
    (e_pair (call empty_message)  init_storage) exp_failwith

let call_on_time () =
  let%bind (program , state) = get_program () in
  let%bind now = mk_time "2000-01-01T10:10:10Z" in
  let%bind lock_time = mk_time "2000-01-01T00:10:10Z" in
  let init_storage = storage lock_time in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options ~now () in
  expect_eq ~options (program, state) "main"
    (e_pair (call empty_message) init_storage) (e_pair empty_op_list init_storage)

let main = test_suite "Time lock" [
    test "compile" compile_main ;
    test "early call" early_call ;
    test "call on time" call_on_time ;
  ]
