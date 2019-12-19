open Trace
open Test_helpers

let type_file f = 
  let%bind simplified  = Ligo.Compile.Of_source.compile f (Syntax_name "pascaligo") in
  let%bind typed,state = Ligo.Compile.Of_simplified.compile simplified in
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
  let%bind simplified      = Ligo.Compile.Of_source.compile "./contracts/time-lock.ligo" (Syntax_name "pascaligo") in
  let%bind typed_prg,_ = Ligo.Compile.Of_simplified.compile simplified in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

open Ast_simplified
let empty_op_list = 
  (e_typed_list [] t_operation)
let empty_message = e_lambda (Var.of_name "arguments")
  (Some t_unit) (Some (t_list t_operation))
  empty_op_list

let call msg = e_constructor "Call" msg
let mk_time st = 
  match Memory_proto_alpha.Protocol.Alpha_context.Timestamp.of_notation st with
  | Some s -> ok s
  | None -> simple_fail "bad timestamp notation"
let to_sec t = Tezos_utils.Time.Protocol.to_seconds t
let storage st = e_timestamp (Int64.to_int @@ to_sec st)

let early_call () =
  let%bind program,_ = get_program () in
  let%bind now = mk_time "2000-01-01T00:10:10Z" in
  let%bind lock_time = mk_time "2000-01-01T10:10:10Z" in
  let init_storage = storage lock_time in
  let options =
    let tezos_context = { Proto_alpha_utils.Memory_proto_alpha.dummy_environment.tezos_context with
      predecessor_timestamp = now ; } in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~tezos_context () in
  let exp_failwith = "Contract is still time locked" in
  expect_string_failwith ~options program "main"
    (e_pair (call empty_message)  init_storage) exp_failwith

let call_on_time () =
  let%bind program,_ = get_program () in
  let%bind now = mk_time "2000-01-01T10:10:10Z" in
  let%bind lock_time = mk_time "2000-01-01T00:10:10Z" in
  let init_storage = storage lock_time in
  let options =
    let tezos_context = { Proto_alpha_utils.Memory_proto_alpha.dummy_environment.tezos_context with
      predecessor_timestamp = now ; } in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~tezos_context () in
  expect_eq ~options program "main"
    (e_pair (call empty_message) init_storage) (e_pair empty_op_list init_storage)

let main = test_suite "Time lock" [
    test "compile" compile_main ;
    test "early call" early_call ;
    test "call on time" call_on_time ;
  ]