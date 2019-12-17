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
        let%bind program = type_file "./contracts/replaceable_id.ligo" in
        s := Some program ;
        ok program
      )

let compile_main () = 
  let%bind simplified      = Ligo.Compile.Of_source.compile "./contracts/replaceable_id.ligo" (Syntax_name "pascaligo") in
  let%bind typed_prg,_ = Ligo.Compile.Of_simplified.compile simplified in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_mini_c.build_contract michelson_prg in
  ok ()
open Ast_simplified

let empty_op_list = 
  (e_typed_list [] t_operation)
let empty_message = e_lambda (Var.of_name "arguments")
  (Some t_unit) (Some (t_list t_operation))
  empty_op_list

let storage id = e_address @@ addr id 
let entry_change_addr id = e_constructor "Change_address"
  @@ e_address @@ addr @@ id
let entry_pass_message = e_constructor "Pass_message"
  @@ empty_message

let change_addr_success () =
  let%bind program,_ = get_program () in
  let init_storage = storage 1 in
  let param = entry_change_addr 2 in
  let options =
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  expect_eq ~options program "main"
    (e_pair param init_storage) (e_pair empty_op_list (storage 2))

let change_addr_fail () =
  let%bind program,_ = get_program () in
  let init_storage = storage 1 in
  let param = entry_change_addr 2 in
  let options =
    let source = contract 3 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  let exp_failwith = "Unauthorized sender" in
  expect_string_failwith ~options program "main"
    (e_pair param init_storage) exp_failwith

let pass_message_success () =
  let%bind program,_ = get_program () in
  let init_storage = storage 1 in
  let param = entry_pass_message in
  let options =
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  expect_eq ~options program "main"
    (e_pair param init_storage) (e_pair empty_op_list init_storage)

let pass_message_fail () =
  let%bind program,_ = get_program () in
  let init_storage = storage 1 in
  let param = entry_pass_message in
  let options =
    let source = contract 2 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  let exp_failwith = "Unauthorized sender" in
  expect_string_failwith ~options program "main"
    (e_pair param init_storage) exp_failwith

let main = test_suite "Replaceable ID" [
    test "compile"              compile_main         ;
    test "change_addr_success"  change_addr_success  ;
    test "change_addr_fail"     change_addr_fail     ;
    test "pass_message_success" pass_message_success ;
    test "pass_message_fail"    pass_message_fail    ;
  ]
