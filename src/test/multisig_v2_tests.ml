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

let empty_op_list = 
  (e_typed_list [] t_operation)
let empty_message = e_lambda "arguments"
  (Some t_unit) (Some (t_list t_operation))
  empty_op_list
let empty_message2 = e_lambda "arguments"
  (Some t_unit) (Some (t_list t_operation))
 ( e_let_in ("foo",Some t_unit) (e_unit ()) empty_op_list)

let send_param msg = e_constructor "Send" msg
let withdraw_param = e_constructor "Withdraw" empty_message

type st_type = {
  threshold:int ;
  max_proposal:int ; 
  max_msg_size:int ;
  id_counter_list: (int * int) list ;
  msg_store_list: (expression * expression) list ;
}
let storage {threshold ; max_proposal ; max_msg_size ; id_counter_list ; msg_store_list} =
  let auth_set,counter_store = List.fold_left
    (fun (auth_set,counter_st) (id,ctr) ->
      let addr_exp = e_address @@ addr id in
      addr_exp::auth_set , (addr_exp, e_nat ctr)::counter_st)
    ([],[])
    id_counter_list in
  e_ez_record [
    ("threshold"       , e_nat threshold                                          ) ;
    ("max_proposal"    , e_nat max_proposal                                       ) ;
    ("max_message_size", e_nat max_msg_size                                       ) ;
    ("auth"            , e_typed_set auth_set t_address                           ) ;
    ("message_store"   , e_typed_map msg_store_list t_bytes (t_set t_address) ) ;
    ("counter_store"   , e_typed_map counter_store t_address t_nat                ) ;
  ]

(* sender not stored in the authorized set *)
let wrong_addr () =
  let%bind program,_ = get_program () in
  let init_storage = storage {
    threshold = 1 ; max_proposal = 1 ; max_msg_size = 1 ;
    id_counter_list = [1,0 ; 2,0] ;
    msg_store_list = []
  } in
  let source = contract 3 in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  let%bind () =
    let exp_failwith = "Unauthorized address" in
    expect_string_failwith ~options program "main"
    (e_pair (send_param empty_message) init_storage) exp_failwith in
  ok ()

(* send a message which exceed the size limit *)
let message_size_exceeded () =
  let%bind program,_ = get_program () in
  let init_storage = storage {
    threshold = 1 ; max_proposal = 1 ; max_msg_size = 1 ;
    id_counter_list = [1,0] ;
    msg_store_list = []
  } in
  let source = contract 1 in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  let%bind () =
    let exp_failwith = "Message size exceed maximum limit" in
    expect_string_failwith ~options program "main"
    (e_pair (send_param empty_message)  init_storage) exp_failwith in
  ok ()

(* sender has already has reached maximum number of proposal *)
let maximum_number_of_proposal () =
  let%bind program,_ = get_program () in
  let%bind packed_payload1 = pack_payload program (send_param empty_message) in
  let%bind bytes1 = e_bytes_ofbytes packed_payload1 in
  let init_storage = storage {
    threshold = 1 ; max_proposal = 1 ; max_msg_size = 15 ;
    id_counter_list = [1,1] ;
    msg_store_list = [(bytes1, e_set [e_address@@ addr 1])]
  } in
  let source = contract 1 in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  let%bind () =
    let exp_failwith = "Maximum number of proposal reached" in
    expect_string_failwith ~options program "main"
      (e_pair (send_param empty_message2) init_storage) exp_failwith in
  ok ()

(* sender message is already stored in the message store *)
let send_already_accounted () =
  let%bind program,_ = get_program () in
  let%bind packed_payload = pack_payload program empty_message in
  let%bind bytes = e_bytes_ofbytes packed_payload in
  let init_storage = storage {
    threshold = 2 ;  max_proposal = 1 ;  max_msg_size = 15 ;
    id_counter_list = [1,1 ; 2,0] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 1])]
  } in
  let options =
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  expect_eq ~options program "main"
    (e_pair (send_param empty_message) init_storage) (e_pair empty_op_list init_storage)

(* sender message isn't stored in the message store *)
let send_never_accounted () =
  let%bind program,_ = get_program () in
  let%bind packed_payload = pack_payload program empty_message in
  let%bind bytes = e_bytes_ofbytes packed_payload in
  let init_storage' = {
    threshold = 2 ; max_proposal = 1 ;  max_msg_size = 15 ;
    id_counter_list = [1,0 ; 2,0] ;
    msg_store_list = [] 
  } in
  let init_storage = storage init_storage' in
  let final_storage = storage { init_storage' with
    id_counter_list = [1,1 ; 2,0] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 1])] ;
  } in
  let options =
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  expect_eq ~options program "main"
    (e_pair (send_param empty_message) init_storage) (e_pair empty_op_list final_storage)

(* sender withdraw message is already binded to one address in the message store *)
let withdraw_already_accounted_one () =
  let%bind program,_ = get_program () in
  let%bind packed_payload = pack_payload program empty_message in
  let%bind bytes = e_bytes_ofbytes packed_payload in
  let param = withdraw_param in
  let init_storage' = {
    threshold = 2 ; max_proposal = 1 ;  max_msg_size = 1 ;
    id_counter_list = [1,1 ; 2,0] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 1])] ;
  } in
  let init_storage = storage init_storage' in
  let final_storage = storage { init_storage' with
    id_counter_list = [1,0 ; 2,0] ;
    msg_store_list = [] } in
  let options =
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  expect_eq ~options program "main"
    (e_pair param init_storage) (e_pair empty_op_list final_storage)

(* sender withdraw message is already binded to two addresses in the message store *)
let withdraw_already_accounted_two () =
  let%bind program,_ = get_program () in
  let%bind packed_payload = pack_payload program empty_message in
  let%bind bytes = e_bytes_ofbytes packed_payload in
  let param = withdraw_param in
  let init_storage' = {
    threshold = 2 ; max_proposal = 2 ;  max_msg_size = 1 ;
    id_counter_list = [1,1 ; 2,1] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 1; e_address@@ addr 2])] ;
  } in
  let init_storage = storage init_storage' in
  let final_storage = storage { init_storage' with
    id_counter_list = [1,0 ; 2,1] ;
    msg_store_list = [(bytes, e_set [e_address@@ addr 2])] } in
  let options =
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  expect_eq ~options program "main"
    (e_pair param init_storage) (e_pair empty_op_list final_storage)

(* sender withdraw message was never accounted *)
let withdraw_never_accounted () =
  let%bind program,_ = get_program () in
  let param = withdraw_param in
  let init_storage = storage {
    threshold = 2 ; max_proposal = 1 ;  max_msg_size = 1 ;
    id_counter_list = [1,0 ; 2,0] ;
    msg_store_list = [] ;
  } in
  let options =
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  expect_eq ~options program "main"
    (e_pair param init_storage) (e_pair empty_op_list init_storage)

(* successful storing in the message store *)
let succeeded_storing () =
  let%bind program,_ = get_program () in
  let%bind packed_payload = pack_payload program empty_message in
  let%bind bytes = e_bytes_ofbytes packed_payload in
  let init_storage th = {
    threshold = th ; max_proposal = 1 ;  max_msg_size = 15 ;
    id_counter_list = [1,0 ; 2,0 ; 3,0] ;
    msg_store_list = [(bytes, e_typed_set [] t_address)] ;
  } in
  let options =
    let source = contract 1 in
    Proto_alpha_utils.Memory_proto_alpha.make_options ~source () in
  let%bind () = expect_eq_n_trace_aux ~options [1;2] program "main"
      (fun th ->
        let init_storage = storage (init_storage th) in
        ok @@ e_pair (send_param empty_message) init_storage
      )
      (fun th ->
        let final_id_counter, final_msg_store, ret = match th with
          | 1 -> [1,0 ; 2,0 ; 3,0] , []                                    , empty_op_list
          | 2 -> [1,1 ; 2,0 ; 3,0] , [(bytes, e_set [e_address@@ addr 1])] , empty_op_list
          | _ -> assert false in
        let final_storage = storage { (init_storage th) with
          msg_store_list = final_msg_store ;
          id_counter_list = final_id_counter } in
        ok @@ e_pair ret final_storage
      ) in
  ok ()

let main = test_suite "Multisig v2" [
    test "compile"                        compile_main                   ;
    test "wrong_addr"                     wrong_addr                     ;
    test "message_size_exceeded"          message_size_exceeded          ;
    test "maximum_number_of_proposal"     maximum_number_of_proposal     ;
    test "send_already_accounted"         send_already_accounted         ;
    test "send_never_accounted"           send_never_accounted           ;
    test "succeeded_storing"              succeeded_storing              ;
    test "withdraw_already_accounted_one" withdraw_already_accounted_one ;
    test "withdraw_already_accounted_two" withdraw_already_accounted_two ;
  ]
