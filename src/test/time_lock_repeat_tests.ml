open Trace
open Test_helpers
open Ast_simplified

let type_file f =
  let%bind simplified  = Ligo.Compile.Of_source.compile f (Syntax_name "cameligo") in
  let%bind typed,state = Ligo.Compile.Of_simplified.compile simplified in
  ok @@ (typed,state)

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/timelock_repeat.mligo" in
        s := Some program ;
        ok program
      )

let compile_main () = 
  let%bind simplified      = Ligo.Compile.Of_source.compile "./contracts/timelock_repeat.mligo" (Syntax_name "cameligo") in
  let%bind typed_prg,_ = Ligo.Compile.Of_simplified.compile simplified in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

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
let storage st interval execute =
  e_ez_record [("next_use", e_timestamp (Int64.to_int @@ to_sec st)) ;
               ("interval", e_int interval) ;
               ("execute", execute)]

let early_call () =
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:10:10Z" in
  let%bind lock_time = mk_time "2000-01-01T10:10:10Z" in
  let init_storage = storage lock_time 86400 empty_message in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options ~predecessor_timestamp () in
  let exp_failwith = "You have to wait before you can execute this contract again." in
  expect_string_failwith ~options program "main"
    (e_pair (e_unit ())  init_storage) exp_failwith

(* Test that when we use the contract the next use time advances by correct interval *)
let interval_advance () =
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-01T10:10:10Z" in
  let%bind lock_time = mk_time "2000-01-01T00:10:10Z" in
  let init_storage = storage lock_time 86400 empty_message in
  (* It takes a second for Current.now to be called, awful hack *)
  let%bind new_timestamp = mk_time "2000-01-02T10:10:11Z" in
  let new_storage = storage new_timestamp 86400 empty_message in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options ~predecessor_timestamp () in
  expect_eq ~options program "main"
  (e_pair (e_unit ()) init_storage) (e_pair empty_op_list new_storage)

let main = test_suite "Time Lock Repeating" [
    test "compile" compile_main ;
    test "early call" early_call ;
    test "interval advance" interval_advance ;
  ]
