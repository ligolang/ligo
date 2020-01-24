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
let storage hashed used commits =
  e_ez_record [("hashed", hashed);
               ("unused", e_bool used);
               ("commits", commits)]

let (first_committer , first_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let commit () =
  let%bind program,_ = get_program () in
  let%bind predecessor_timestamp = mk_time "2000-01-01T00:10:10Z" in
  let%bind lock_time = mk_time "2000-01-02T00:10:11Z" in
  let test_hash_raw = sha_256_hash (Bytes.of_string "hello world") in
  let test_hash = e_bytes_raw test_hash_raw in
  let%bind packed_sender = pack_payload program (e_bytes_string first_committer) in
  let salted_hash = e_bytes_raw (sha_256_hash
                                   (Bytes.concat Bytes.empty [test_hash_raw;
                                                              packed_sender]))
  in
  let pre_commits = e_typed_big_map [] t_address (t_record_ez [("date", t_timestamp);
                                                              ("salted_hash", t_bytes)])
  in
  let init_storage = storage test_hash true pre_commits in
  let commit =
    e_ez_record [("date", e_timestamp
                    (Int64.to_int (to_sec lock_time)));
                 ("salted_hash", salted_hash)]
  in
  let post_commits = e_big_map [((e_address first_committer), commit)]
  in
  let post_storage = storage salted_hash true post_commits in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options
      ~predecessor_timestamp
      ~payer:first_contract
      ()
  in
  expect_eq ~options program "commit"
    (e_pair (e_unit ()) init_storage) (e_pair empty_op_list post_storage)

let main = test_suite "Hashlock" [
    test "compile" compile_main ;
    test "commit" commit ;
]
