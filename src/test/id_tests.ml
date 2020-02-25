open Trace
open Test_helpers
open Ast_simplified


let mtype_file f =
  let%bind simplified  = Ligo.Compile.Of_source.compile f (Syntax_name "cameligo") in
  let%bind typed,state = Ligo.Compile.Of_simplified.compile simplified in
  ok (typed,state)

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = mtype_file "./contracts/id.mligo" in
        s := Some program ;
        ok program
      )

let compile_main () =
  let%bind simplified      = Ligo.Compile.Of_source.compile "./contracts/id.mligo" (Syntax_name "cameligo") in
  let%bind typed_prg,_ = Ligo.Compile.Of_simplified.compile simplified in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

let (first_owner , first_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let buy_id () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1)]) ;
                         e_int 1;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let param = e_pair owner_website (e_some (e_address new_addr)) in
  let new_storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                         (e_int 1, id_details_2)]) ;
                              e_int 2;
                              e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let%bind () = expect_eq ~options program "buy" 
      (e_pair param storage) 
      (e_pair (e_list []) new_storage)
  in ok ()

let buy_id_sender_addr () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1)]) ;
                         e_int 1;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let param = e_pair owner_website (e_typed_none t_address) in
  let new_storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                         (e_int 1, id_details_2)]) ;
                              e_int 2;
                              e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let%bind () = expect_eq ~options program "buy" 
      (e_pair param storage) 
      (e_pair (e_list []) new_storage)
  in ok ()

(* Test that contract fails if we attempt to buy an ID for the wrong amount *)
let buy_id_wrong_amount () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1)]) ;
                         e_int 1;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.fifty_cents) ()
  in
  let param = e_pair owner_website (e_some (e_address new_addr)) in
  let%bind () = expect_string_failwith ~options program "buy" 
      (e_pair param storage) 
      "Incorrect amount paid."
  in ok ()

let update_details_owner () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero) 
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", new_website)]
  in
  let id_details_2_diff = e_record_ez [("owner", e_address new_addr) ;
                                       ("controller", e_address new_addr) ;
                                       ("profile", new_website)] in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let new_storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                         (e_int 1, id_details_2_diff)]) ;
                             e_int 2;
                             e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let details = e_bytes_string "ligolang.org" in
  let param = e_tuple [e_int 1 ; 
                       e_some details ;
                       e_some (e_address new_addr)] in
  let%bind () = expect_eq ~options program "update_details"
      (e_pair param storage)
      (e_pair (e_list []) new_storage)
  in ok ()

let update_details_controller () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let id_details_2_diff = e_record_ez [("owner", e_address owner_addr) ;
                                       ("controller", e_address owner_addr) ;
                                       ("profile", new_website)] in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let new_storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                         (e_int 1, id_details_2_diff)]) ;
                             e_int 2;
                             e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let details = e_bytes_string "ligolang.org" in
  let param = e_tuple [e_int 1 ; 
                       e_some details ;
                       e_some (e_address owner_addr)] in
  let%bind () = expect_eq ~options program "update_details"
      (e_pair param storage)
      (e_pair (e_list []) new_storage)
  in ok ()

(* Test that contract fails when we attempt to update details of nonexistent ID *)
let update_details_nonexistent () = 
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let details = e_bytes_string "ligolang.org" in
  let param = e_tuple [e_int 2 ; 
                       e_some details ;
                       e_some (e_address owner_addr)] in
  let%bind () = expect_string_failwith ~options program "update_details"
      (e_pair param storage)
      "This ID does not exist."
  in ok ()

(* Test that contract fails when we attempt to update details from wrong addr *)
let update_details_wrong_addr () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let details = e_bytes_string "ligolang.org" in
  let param = e_tuple [e_int 0 ; 
                       e_some details ;
                       e_some (e_address owner_addr)] in
  let%bind () = expect_string_failwith ~options program "update_details"
      (e_pair param storage)
      "You are not the owner or controller of this ID."
  in ok ()

(* Test that giving none on both profile and controller address is a no-op *)
let update_details_unchanged () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let param = e_tuple [e_int 1 ; 
                       e_typed_none t_bytes ;
                       e_typed_none t_address] in
  let%bind () = expect_eq ~options program "update_details"
      (e_pair param storage)
      (e_pair (e_list []) storage)
  in ok ()

let update_owner () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let id_details_2_diff = e_record_ez [("owner", e_address owner_addr) ;
                                       ("controller", e_address new_addr) ;
                                       ("profile", new_website)] in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let new_storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                         (e_int 1, id_details_2_diff)]) ;
                             e_int 2;
                             e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let param = e_pair (e_int 1) (e_address owner_addr) in
  let%bind () = expect_eq ~options program "update_owner"
      (e_pair param storage)
      (e_pair (e_list []) new_storage)
  in ok ()

(* Test that contract fails when we attempt to update owner of nonexistent ID *)
let update_owner_nonexistent () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let param = e_pair (e_int 2) (e_address new_addr) in
  let%bind () = expect_string_failwith ~options program "update_owner"
      (e_pair param storage)
      "This ID does not exist."
  in ok ()

(* Test that contract fails when we attempt to update owner from non-owner addr *)
let update_owner_wrong_addr () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero)
      ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let param = e_pair (e_int 0) (e_address new_addr) in
  let%bind () = expect_string_failwith ~options program "update_owner"
      (e_pair param storage)
      "You are not the owner of this ID."
  in ok ()

let skip () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let new_storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                         (e_int 1, id_details_2)]) ;
                             e_int 3;
                             e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let%bind () = expect_eq ~options program "skip"
      (e_pair (e_unit ()) storage)
      (e_pair (e_list []) new_storage)
  in ok ()

(* Test that contract fails if we try to skip without paying the right amount *)
let skip_wrong_amount () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let owner_website = e_bytes_string "ligolang.org" in
  let id_details_1 = e_record_ez [("owner", e_address owner_addr) ;
                                  ("controller", e_address owner_addr) ;
                                  ("profile", owner_website)]
  in
  let new_addr = first_owner in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:first_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.fifty_cents) ()
  in
  let new_website = e_bytes_string "ligolang.org" in
  let id_details_2 = e_record_ez [("owner", e_address new_addr) ;
                                  ("controller", e_address new_addr) ;
                                  ("profile", new_website)]
  in
  let storage = e_tuple [(e_big_map [(e_int 0, id_details_1) ;
                                     (e_int 1, id_details_2)]) ;
                         e_int 2;
                         e_tuple [e_mutez 1000000 ; e_mutez 1000000]]
  in
  let%bind () = expect_string_failwith ~options program "skip"
      (e_pair (e_unit ()) storage)
      "Incorrect amount paid."
  in ok ()

let main = test_suite "ID Layer" [
    test "buy"    buy_id ;
    test "buy (sender addr)" buy_id_sender_addr ;
    test "buy (wrong amount)" buy_id_wrong_amount ;
    test "update_details (owner)" update_details_owner ;
    test "update_details (controller)" update_details_controller ;
    test "update_details_nonexistent" update_details_nonexistent ;
    test "update_details_wrong_addr" update_details_wrong_addr ;
    test "update_details_unchanged" update_details_unchanged ;
    test "update_owner" update_owner ;
    test "update_owner_nonexistent" update_owner_nonexistent ;
    test "update_owner_wrong_addr" update_owner_wrong_addr ;
    test "skip" skip ;
    test "skip (wrong amount)" skip_wrong_amount ;
]
