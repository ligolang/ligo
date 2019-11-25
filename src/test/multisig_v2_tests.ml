open Trace
open Test_helpers

let type_file = Ligo.Compile.Of_source.type_file (Syntax_name "pascaligo")

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/multisig-v2.ligo" in
        s := Some program ;
        ok program
      )

let compile_main () = 
  let%bind program,_ = get_program () in
  let%bind () =
    Ligo.Run.Of_simplified.compile_program
    program "main" in
  ok ()

open Ast_simplified

let contract id = 
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities id in
  id.implicit_contract
let addr id =
  let open Proto_alpha_utils.Memory_proto_alpha in
  Protocol.Alpha_context.Contract.to_b58check @@ contract id

let empty_op_list = 
  (e_typed_list [] t_operation)
let empty_message = e_lambda "arguments"
  (Some t_unit) (Some (t_list t_operation))
  empty_op_list

let param = e_constructor "Send" empty_message

let storage threshold id_list store_list = e_ez_record [
  ("threshold", e_nat threshold) ;
  ("auth"     , e_typed_set
    (List.fold_left (fun acc el -> (e_address @@ addr el)::acc) [] id_list) 
    t_address) ;
  ("message_store" , e_typed_big_map store_list t_bytes (t_set t_address))
]

(* sender not stored in the authorized set *)
let wrong_addr () =
  let%bind program,_ = get_program () in
  let init_storage = storage 1 [1;2] [] in
  let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
  let source = contract 3 in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~source () in
  let%bind () =
    let exp_failwith = "Unauthorized address" in
    expect_string_failwith ~options program "main"
    (e_pair param init_storage) exp_failwith in
  ok ()

(* sender message is already stored in the message store *)
let already_accounted () =
  let%bind program,_ = get_program () in
  let%bind packed_payload = pack_payload program empty_message in
  let%bind bytes = e_bytes_ofbytes packed_payload in
  let init_storage = storage 2 [1;2]
    [(bytes, e_set [e_address@@ addr 1])] in
  let options =
    let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~source () in
  expect_eq ~options program "main"
    (e_pair param init_storage) (e_pair empty_op_list init_storage)

(* sender message isn't stored in the message store *)
let never_accounted () =
  let%bind program,_ = get_program () in
  let%bind packed_payload = pack_payload program empty_message in
  let%bind bytes = e_bytes_ofbytes packed_payload in
  let init_storage = storage 2 [1;2]
    [] in
  let final_storage = storage 2 [1;2]
    [(bytes, e_set [e_address@@ addr 1])] in
  let options =
    let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~source () in
  expect_eq ~options program "main"
    (e_pair param init_storage) (e_pair empty_op_list final_storage)

(* successful storing in the message store *)
let succeeded_storing () =
  let%bind program,_ = get_program () in
  let%bind packed_payload = pack_payload program empty_message in
  let%bind bytes = e_bytes_ofbytes packed_payload in
  let options =
    let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~source () in
  let%bind () = expect_eq_n_trace_aux ~options [1;2] program "main"
      (fun th ->
        let init_storage = storage th [1;2;3]
        [(bytes, e_typed_set [] t_address)] in
        ok @@ e_pair param init_storage
      )
      (fun th ->
        let final_msg_store, ret = match th with
        | 1 -> [] , empty_op_list
        | 2 -> [(bytes, e_set [e_address@@ addr 1])] , empty_op_list
        | _ -> failwith "impossible" in
        let final_storage = storage th [1;2;3] final_msg_store in
        ok @@ e_pair ret final_storage
      ) in
  ok ()

let main = test_suite "Multisig v2" [
    test "compile"           compile_main      ;
    test "wrong_addr"        wrong_addr        ;
    test "already_accounted" already_accounted ;
    test "never_accounted"   never_accounted   ;
    test "succeeded_storing" succeeded_storing ;
  ]
