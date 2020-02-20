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
        let%bind program = type_file "./contracts/hashlock.mligo" in
        s := Some program ;
        ok program
      )

let compile_main () =
  let%bind simplified      = Ligo.Compile.Of_source.compile "./contracts/hashlock.mligo" (Syntax_name "cameligo") in
  let%bind typed_prg,_ = Ligo.Compile.Of_simplified.compile simplified in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

let call msg = e_constructor "Call" msg
let mk_time st =
  match Memory_proto_alpha.Protocol.Alpha_context.Timestamp.of_notation st with
  | Some s -> ok s
  | None -> simple_fail "bad timestamp notation"
let to_sec t = Tezos_utils.Time.Protocol.to_seconds t
let storage hashed used commits =
  e_record_ez [("hashed", hashed);
               ("unused", e_bool used);
               ("commits", commits)]

let (first_committer , first_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let empty_op_list =
  (e_typed_list [] t_operation)
let empty_message = e_lambda (Var.of_name "arguments")
  (Some t_unit) (Some (t_list t_operation))
  empty_op_list


let commit () =
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:10:10Z" in
  let%bind lock_time = mk_time "2000-01-02T00:10:11Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let%bind packed_sender = pack_payload program (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [test_hash_raw;
                                                              packed_sender]))
 
  in
  let pre_commits = e_typed_big_map [] t_address (t_record_ez [("date", t_timestamp);
                                                              ("salted_hash", t_bytes)])
  in
  let init_storage = storage test_hash true pre_commits in
  let commit =
    e_record_ez [("date", e_timestamp
                    (Int64.to_int (to_sec lock_time)));
                 ("salted_hash", salted_hash)]
  in
  let post_commits = e_big_map [((e_address first_committer), commit)]
  in
  let post_storage = storage test_hash true post_commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_eq ~options program "commit"
    (e_pair salted_hash init_storage) (e_pair empty_op_list post_storage)

(* Test that the contract fails if we haven't committed before revealing the answer *)
let reveal_no_commit () =
  let%bind program,_ = get_program () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello world");
                            ("message", empty_message)]
  in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let pre_commits = e_typed_big_map [] t_address (t_record_ez [("date", t_timestamp);
                                                              ("salted_hash", t_bytes)])
  in
  let init_storage = storage test_hash true pre_commits in
  expect_string_failwith program "reveal"
    (e_pair reveal init_storage)
    "You haven't made a commitment to hash against yet."

(* Test that the contract fails if our commit isn't 24 hours old yet *)
let reveal_young_commit () =
  let%bind program,_ = get_program () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello world");
                            ("message", empty_message)]
  in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:10:10Z" in
  let%bind lock_time = mk_time "2000-01-02T00:10:11Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let%bind packed_sender = pack_payload program (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [test_hash_raw;
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp
                    (Int64.to_int (to_sec lock_time)));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash true commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "reveal"
    (e_pair reveal init_storage)
    "It hasn't been 24 hours since your commit yet."

(* Test that the contract fails if our reveal doesn't meet our commitment *)
let reveal_breaks_commit () =
  let%bind program,_ = get_program () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello world");
                            ("message", empty_message)]
  in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let%bind packed_sender = pack_payload program (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [Bytes.of_string "hello";
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp
                    (Int64.to_int (to_sec predecessor_timestamp)));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash true commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "reveal"
    (e_pair reveal init_storage)
    "This reveal doesn't match your commitment."

(* Test that the contract fails if we reveal the wrong bytes for the stored hash *)
let reveal_wrong_commit () =
  let%bind program,_ = get_program () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello");
                            ("message", empty_message)]
  in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let%bind packed_sender = pack_payload program (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [Bytes.of_string "hello";
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp
                    (Int64.to_int (to_sec predecessor_timestamp)));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash true commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "reveal"
    (e_pair reveal init_storage)
    "Your commitment did not match the storage hash."

(* Test that the contract fails if we try to reuse it after unused flag changed *)
let reveal_no_reuse () = 
  let%bind program,_ = get_program () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello");
                            ("message", empty_message)]
  in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let%bind packed_sender = pack_payload program (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [Bytes.of_string "hello";
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp
                    (Int64.to_int (to_sec predecessor_timestamp)));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash false commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_string_failwith ~options program "reveal"
    (e_pair reveal init_storage)
    "This contract has already been used."

(* Test that the contract executes successfully with valid commit-reveal *)
let reveal () =
  let%bind program,_ = get_program () in
  let empty_message = empty_message in
  let reveal = e_record_ez [("hashable", e_bytes_string "hello world");
                            ("message", empty_message)]
  in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:10:10Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let%bind packed_sender = pack_payload program (e_address first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [Bytes.of_string "hello world";
                                                              packed_sender])) in
  let commit =
    e_record_ez [("date", e_timestamp
                    (Int64.to_int (to_sec predecessor_timestamp)));
                 ("salted_hash", salted_hash)]
  in
  let commits = e_big_map [((e_address first_committer), commit)]
  in
  let init_storage = storage test_hash true commits in
  let post_storage = storage test_hash false commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~sender:first_contract
      ()
  in
  expect_eq ~options program "reveal"
    (e_pair reveal init_storage) (e_pair empty_op_list post_storage)

let main = test_suite "Hashlock" [
    test "compile" compile_main ;
    test "commit" commit ;
    test "reveal (fail if no commitment)" reveal_no_commit ;
    test "reveal (fail if commit too young)" reveal_young_commit ;
    test "reveal (fail if breaks commitment)" reveal_breaks_commit ;
    test "reveal (fail if wrong bytes for hash)" reveal_wrong_commit ;
    test "reveal (fail if attempt to reuse)" reveal_no_reuse ;
    test "reveal" reveal ;
]
