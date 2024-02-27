open Cli_expect

let test basename = "./" ^ basename
let pwd = Caml.Sys.getcwd ()
let () = Caml.Sys.chdir "../../test/contracts/interpreter_tests/"

(* tests for timelock primitives *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_timelock.mligo" ];
  [%expect
    {|
    File "./test_timelock.mligo", line 19, characters 13-27:
     18 |   let init_storage : bytes = 0x41414141 in
     19 |   let orig = Test.originate (contract_of C) init_storage 0tez in
                       ^^^^^^^^^^^^^^
     20 |   let payload = 0x4141
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timelock.mligo", line 23, characters 38-54:
     22 |   let test_open (cc : chest_key * chest) (expected : bytes) : unit =
     23 |     let x : C parameter_of contract = Test.to_contract orig.addr in
                                                ^^^^^^^^^^^^^^^^
     24 |     let _ = Test.transfer_to_contract_exn x (Check cc) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timelock.mligo", line 24, characters 12-41:
     23 |     let x : C parameter_of contract = Test.to_contract orig.addr in
     24 |     let _ = Test.transfer_to_contract_exn x (Check cc) 0tez in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     25 |     let s = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timelock.mligo", line 25, characters 12-28:
     24 |     let _ = Test.transfer_to_contract_exn x (Check cc) 0tez in
     25 |     let s = Test.get_storage orig.addr in
                      ^^^^^^^^^^^^^^^^
     26 |     let _ = Test.log (s, expected) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timelock.mligo", line 26, characters 12-20:
     25 |     let s = Test.get_storage orig.addr in
     26 |     let _ = Test.log (s, expected) in
                      ^^^^^^^^
     27 |     assert (s = expected)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timelock.mligo", line 30, characters 27-44:
     29 |   let test1 = (* chest key/payload and time matches -> OK *)
     30 |     let chest, chest_key = Test.create_chest payload 10n in
                                     ^^^^^^^^^^^^^^^^^
     31 |     test_open (chest_key, chest) payload
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Timelock.create` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timelock.mligo", line 34, characters 19-36:
     33 |   let test2 = (* chest key/payload do not match *)
     34 |     let chest, _ = Test.create_chest payload 10n in
                             ^^^^^^^^^^^^^^^^^
     35 |     let _, chest_key = Test.create_chest 0x2020 10n in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Timelock.create` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timelock.mligo", line 35, characters 23-40:
     34 |     let chest, _ = Test.create_chest payload 10n in
     35 |     let _, chest_key = Test.create_chest 0x2020 10n in
                                 ^^^^^^^^^^^^^^^^^
     36 |     test_open (chest_key,chest) 0xff
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Timelock.create` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timelock.mligo", line 39, characters 19-36:
     38 |   let test3 = (* chest time does not match *)
     39 |     let chest, _ = Test.create_chest payload 2n in
                             ^^^^^^^^^^^^^^^^^
     40 |     let chest_key = Test.create_chest_key chest 10n in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Timelock.create` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timelock.mligo", line 40, characters 20-41:
     39 |     let chest, _ = Test.create_chest payload 2n in
     40 |     let chest_key = Test.create_chest_key chest 10n in
                              ^^^^^^^^^^^^^^^^^^^^^
     41 |     test_open (chest_key, chest) 0x
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Timelock.create_key` from `Test.Next` is encouraged for a smoother migration.

    (0x4141 , 0x4141)
    (0xff , 0xff)
    (0x , 0x)
    Everything at the top-level was executed.
    - test exited with value [() ; () ; ()]. |}]

(* tests replacing Hashlock tests *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_hashlock.mligo" ];
  [%expect
    {|
    File "./test_hashlock.mligo", line 4, characters 11-30:
      3 | let test_commit =
      4 |   let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^^^^
      5 |   let first_committer = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 5, characters 24-50:
      4 |   let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
      5 |   let first_committer = Test.nth_bootstrap_account 0 in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
      6 |   let hashable = [%bytes "hello world"] in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 11, characters 48-72:
     10 |   let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = pre_commits } in
     11 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^
     12 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 12, characters 14-30:
     11 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
     12 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     13 |   let parameter = Commit salted_hash in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 14, characters 11-26:
     13 |   let parameter = Commit salted_hash in
     14 |   let () = Test.set_source first_committer in
                     ^^^^^^^^^^^^^^^
     15 |   let lock_time = Tezos.get_now () + 15 + 86_400 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 16, characters 10-39:
     15 |   let lock_time = Tezos.get_now () + 15 + 86_400 in
     16 |   let _ = Test.transfer_to_contract_exn contr parameter 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     17 |   let new_storage = Test.get_storage typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 17, characters 20-36:
     16 |   let _ = Test.transfer_to_contract_exn contr parameter 0tez in
     17 |   let new_storage = Test.get_storage typed_addr in
                              ^^^^^^^^^^^^^^^^
     18 |   let commit = { date = lock_time ; salted_hash = salted_hash } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 21, characters 2-13:
     20 |   let post_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = post_commits } in
     21 |   Test.assert (new_storage = post_storage)
            ^^^^^^^^^^^
     22 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 24, characters 11-30:
     23 | let test_reveal_no_commit =
     24 |   let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^^^^
     25 |   let first_committer = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 25, characters 24-50:
     24 |   let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
     25 |   let first_committer = Test.nth_bootstrap_account 0 in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
     26 |   let empty_message = fun (_ : unit) -> ([] : operation list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 31, characters 48-72:
     30 |   let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = pre_commits } in
     31 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^
     32 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 32, characters 14-30:
     31 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
     32 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     33 |   let parameter = Reveal reveal in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 34, characters 11-26:
     33 |   let parameter = Reveal reveal in
     34 |   let () = Test.set_source first_committer in
                     ^^^^^^^^^^^^^^^
     35 |   match Test.transfer_to_contract contr parameter 0tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 35, characters 8-33:
     34 |   let () = Test.set_source first_committer in
     35 |   match Test.transfer_to_contract contr parameter 0tez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
     36 |   | Success _ -> failwith "Transaction should fail"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 37, characters 30-41:
     36 |   | Success _ -> failwith "Transaction should fail"
     37 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "You have not made a commitment to hash against yet."))
                                        ^^^^^^^^^^^
     38 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 37, characters 43-63:
     36 |   | Success _ -> failwith "Transaction should fail"
     37 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "You have not made a commitment to hash against yet."))
                                                     ^^^^^^^^^^^^^^^^^^^^
     38 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 37, characters 67-76:
     36 |   | Success _ -> failwith "Transaction should fail"
     37 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "You have not made a commitment to hash against yet."))
                                                                             ^^^^^^^^^
     38 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 41, characters 11-30:
     40 | let test_reveal_young_commit =
     41 |   let () = Test.reset_state_at (3_600 : timestamp) 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^^^^
     42 |   let first_committer = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 42, characters 24-50:
     41 |   let () = Test.reset_state_at (3_600 : timestamp) 10n ([] : tez list) in
     42 |   let first_committer = Test.nth_bootstrap_account 0 in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
     43 |   let packed_sender = Bytes.pack first_committer in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 50, characters 48-72:
     49 |   let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
     50 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^
     51 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 51, characters 14-30:
     50 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
     51 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     52 |   let empty_message = fun (_ : unit) -> ([] : operation list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 55, characters 11-26:
     54 |   let parameter = Reveal reveal in
     55 |   let () = Test.set_source first_committer in
                     ^^^^^^^^^^^^^^^
     56 |   match Test.transfer_to_contract contr parameter 0tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 56, characters 8-33:
     55 |   let () = Test.set_source first_committer in
     56 |   match Test.transfer_to_contract contr parameter 0tez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
     57 |   | Success _ -> failwith "Transaction should fail"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 58, characters 30-41:
     57 |   | Success _ -> failwith "Transaction should fail"
     58 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "It has not been 24 hours since your commit yet."))
                                        ^^^^^^^^^^^
     59 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 58, characters 43-63:
     57 |   | Success _ -> failwith "Transaction should fail"
     58 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "It has not been 24 hours since your commit yet."))
                                                     ^^^^^^^^^^^^^^^^^^^^
     59 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 58, characters 67-76:
     57 |   | Success _ -> failwith "Transaction should fail"
     58 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "It has not been 24 hours since your commit yet."))
                                                                             ^^^^^^^^^
     59 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 62, characters 11-30:
     61 | let test_reveal_breaks_commit =
     62 |   let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^^^^
     63 |   let first_committer = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 63, characters 24-50:
     62 |   let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
     63 |   let first_committer = Test.nth_bootstrap_account 0 in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
     64 |   let packed_sender = Bytes.pack first_committer in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 72, characters 48-72:
     71 |   let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
     72 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^
     73 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 73, characters 14-30:
     72 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
     73 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     74 |   let empty_message = fun (_ : unit) -> ([] : operation list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 77, characters 11-26:
     76 |   let parameter = Reveal reveal in
     77 |   let () = Test.set_source first_committer in
                     ^^^^^^^^^^^^^^^
     78 |   match Test.transfer_to_contract contr parameter 0tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 78, characters 8-33:
     77 |   let () = Test.set_source first_committer in
     78 |   match Test.transfer_to_contract contr parameter 0tez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
     79 |   | Success _ -> failwith "Transaction should fail"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 80, characters 30-41:
     79 |   | Success _ -> failwith "Transaction should fail"
     80 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "This reveal does not match your commitment."))
                                        ^^^^^^^^^^^
     81 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 80, characters 43-63:
     79 |   | Success _ -> failwith "Transaction should fail"
     80 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "This reveal does not match your commitment."))
                                                     ^^^^^^^^^^^^^^^^^^^^
     81 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 80, characters 67-76:
     79 |   | Success _ -> failwith "Transaction should fail"
     80 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "This reveal does not match your commitment."))
                                                                             ^^^^^^^^^
     81 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 84, characters 11-30:
     83 | let test_reveal_wrong_commit =
     84 |   let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^^^^
     85 |   let first_committer = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 85, characters 24-50:
     84 |   let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
     85 |   let first_committer = Test.nth_bootstrap_account 0 in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
     86 |   let packed_sender = Bytes.pack first_committer in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 94, characters 48-72:
     93 |   let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
     94 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^
     95 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 95, characters 14-30:
     94 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
     95 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     96 |   let empty_message = fun (_ : unit) -> ([] : operation list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 99, characters 11-26:
     98 |   let parameter = Reveal reveal in
     99 |   let () = Test.set_source first_committer in
                     ^^^^^^^^^^^^^^^
    100 |   match Test.transfer_to_contract contr parameter 0tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 100, characters 8-33:
     99 |   let () = Test.set_source first_committer in
    100 |   match Test.transfer_to_contract contr parameter 0tez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
    101 |   | Success _ -> failwith "Transaction should fail"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 102, characters 30-41:
    101 |   | Success _ -> failwith "Transaction should fail"
    102 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "Your commitment did not match the storage hash."))
                                        ^^^^^^^^^^^
    103 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 102, characters 43-63:
    101 |   | Success _ -> failwith "Transaction should fail"
    102 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "Your commitment did not match the storage hash."))
                                                     ^^^^^^^^^^^^^^^^^^^^
    103 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 102, characters 67-76:
    101 |   | Success _ -> failwith "Transaction should fail"
    102 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "Your commitment did not match the storage hash."))
                                                                             ^^^^^^^^^
    103 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 106, characters 11-30:
    105 | let test_reveal_no_reuse =
    106 |   let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^^^^
    107 |   let first_committer = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 107, characters 24-50:
    106 |   let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
    107 |   let first_committer = Test.nth_bootstrap_account 0 in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
    108 |   let packed_sender = Bytes.pack first_committer in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 116, characters 48-72:
    115 |   let init_storage = { hashed = Crypto.sha256 hashable ; unused = false ; commits = commits } in
    116 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^
    117 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 117, characters 14-30:
    116 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
    117 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
    118 |   let empty_message = fun (_ : unit) -> ([] : operation list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 121, characters 11-26:
    120 |   let parameter = Reveal reveal in
    121 |   let () = Test.set_source first_committer in
                     ^^^^^^^^^^^^^^^
    122 |   match Test.transfer_to_contract contr parameter 0tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 122, characters 8-33:
    121 |   let () = Test.set_source first_committer in
    122 |   match Test.transfer_to_contract contr parameter 0tez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
    123 |   | Success _ -> failwith "Transaction should fail"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 124, characters 30-41:
    123 |   | Success _ -> failwith "Transaction should fail"
    124 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "This contract has already been used."))
                                        ^^^^^^^^^^^
    125 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 124, characters 43-63:
    123 |   | Success _ -> failwith "Transaction should fail"
    124 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "This contract has already been used."))
                                                     ^^^^^^^^^^^^^^^^^^^^
    125 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 124, characters 67-76:
    123 |   | Success _ -> failwith "Transaction should fail"
    124 |   | Fail (Rejected (a, _)) -> Test.assert (Test.michelson_equal a (Test.eval "This contract has already been used."))
                                                                             ^^^^^^^^^
    125 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 128, characters 11-30:
    127 | let test_reveal =
    128 |   let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^^^^
    129 |   let first_committer = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 129, characters 24-50:
    128 |   let () = Test.reset_state_at (86_400 : timestamp) 10n ([] : tez list) in
    129 |   let first_committer = Test.nth_bootstrap_account 0 in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
    130 |   let packed_sender = Bytes.pack first_committer in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 137, characters 48-72:
    136 |   let init_storage = { hashed = Crypto.sha256 hashable ; unused = true ; commits = commits } in
    137 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^
    138 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 138, characters 14-30:
    137 |   let {addr = typed_addr; code = _; size = _} = Test.originate_from_file "../hashlock.mligo" init_storage 0tez in
    138 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
    139 |   let empty_message = fun (_ : unit) -> ([] : operation list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 142, characters 11-26:
    141 |   let parameter = Reveal reveal in
    142 |   let () = Test.set_source first_committer in
                     ^^^^^^^^^^^^^^^
    143 |   let _ = Test.transfer_to_contract_exn contr parameter 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 143, characters 10-39:
    142 |   let () = Test.set_source first_committer in
    143 |   let _ = Test.transfer_to_contract_exn contr parameter 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    144 |   let new_storage = Test.get_storage typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 144, characters 20-36:
    143 |   let _ = Test.transfer_to_contract_exn contr parameter 0tez in
    144 |   let new_storage = Test.get_storage typed_addr in
                              ^^^^^^^^^^^^^^^^
    145 |   let commit = { date = lock_time ; salted_hash = salted_hash } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_hashlock.mligo", line 148, characters 2-13:
    147 |   let post_storage = { hashed = Crypto.sha256 hashable ; unused = false ; commits = post_commits } in
    148 |   Test.assert (new_storage = post_storage)
            ^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_commit exited with value ().
    - test_reveal_no_commit exited with value ().
    - test_reveal_young_commit exited with value ().
    - test_reveal_breaks_commit exited with value ().
    - test_reveal_wrong_commit exited with value ().
    - test_reveal_no_reuse exited with value ().
    - test_reveal exited with value (). |}]

(* test comparison on sum/record types *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_compare.mligo" ];
  [%expect
    {|
    File "./test_compare.mligo", line 7, characters 11-22:
      6 | let test_cmp =
      7 |   let () = Test.assert (A "hello" > B 42) in
                     ^^^^^^^^^^^
      8 |   let () = Test.assert (C "x" < D 0) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare.mligo", line 8, characters 11-22:
      7 |   let () = Test.assert (A "hello" > B 42) in
      8 |   let () = Test.assert (C "x" < D 0) in
                     ^^^^^^^^^^^
      9 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare.mligo", line 12, characters 11-22:
     11 | let test_cmp_list =
     12 |   let () = Test.assert ([A "hello" ; A "bye"] > [A "hello" ; B 42]) in
                     ^^^^^^^^^^^
     13 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_cmp exited with value ().
    - test_cmp_list exited with value ().
    - test_cmp_record exited with value (). |}]

(* test loops on maps *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_loop_map.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - testmap exited with value [0 -> 1 ; 1 -> 2 ; 2 -> 4].
    - test_entries exited with value [(2 , 4) ; (1 , 2) ; (0 , 1)].
    - test_unzipped_entries exited with value ([2 ; 1 ; 0] , [4 ; 2 ; 1]). |}]

(* events payload being records and not decompiled to pairs in the interpreter *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_events_pair_vs_record.mligo" ];
  [%expect
    {|
    File "./test_events_pair_vs_record.mligo", line 14, characters 13-27:
     13 | let test_foo =
     14 |   let orig = Test.originate (contract_of C) () 0tez in
                       ^^^^^^^^^^^^^^
     15 |   let _ = Test.transfer_exn orig.addr (Main {num1 = 1n ; num2 = 2n}) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_events_pair_vs_record.mligo", line 15, characters 10-27:
     14 |   let orig = Test.originate (contract_of C) () 0tez in
     15 |   let _ = Test.transfer_exn orig.addr (Main {num1 = 1n ; num2 = 2n}) 0tez in
                    ^^^^^^^^^^^^^^^^^
     16 |   let events = (Test.get_last_events_from orig.addr "foo" : C.mystruct list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_events_pair_vs_record.mligo", line 16, characters 16-41:
     15 |   let _ = Test.transfer_exn orig.addr (Main {num1 = 1n ; num2 = 2n}) 0tez in
     16 |   let events = (Test.get_last_events_from orig.addr "foo" : C.mystruct list) in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^
     17 |   match events with [{num1;num2}] -> num1 + num2 | _ -> Test.failwith "not good"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.last_events` from `Test.Next` is encouraged for a smoother migration.

    File "./test_events_pair_vs_record.mligo", line 17, characters 56-69:
     16 |   let events = (Test.get_last_events_from orig.addr "foo" : C.mystruct list) in
     17 |   match events with [{num1;num2}] -> num1 + num2 | _ -> Test.failwith "not good"
                                                                  ^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.failwith` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_foo exited with value 3n. |}]

(* decompilation of timestamp *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_timestamp_contract.mligo" ];
  [%expect
    {|
    File "./test_timestamp_contract.mligo", line 8, characters 11-27:
      7 | let boot () =
      8 |   let () = Test.reset_state 2n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
      9 |   let sender_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timestamp_contract.mligo", line 9, characters 16-42:
      8 |   let () = Test.reset_state 2n ([] : tez list) in
      9 |   let sender_ = Test.nth_bootstrap_account 1 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |   let () = Test.set_source sender_ in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timestamp_contract.mligo", line 10, characters 11-26:
      9 |   let sender_ = Test.nth_bootstrap_account 1 in
     10 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
     11 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timestamp_contract.mligo", line 14, characters 13-27:
     13 |
     14 |   let orig = Test.originate (contract_of C) init_storage 0mutez in
                       ^^^^^^^^^^^^^^
     15 |   let contr = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timestamp_contract.mligo", line 15, characters 14-30:
     14 |   let orig = Test.originate (contract_of C) init_storage 0mutez in
     15 |   let contr = Test.to_contract orig.addr in
                        ^^^^^^^^^^^^^^^^
     16 |   let addr = Tezos.address contr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timestamp_contract.mligo", line 21, characters 10-35:
     20 |   let c = boot() in
     21 |   let r = Test.transfer_to_contract c.contr (Main ("2022-01-01t10:10:10Z" : timestamp)) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
     22 |   Test.log r
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timestamp_contract.mligo", line 22, characters 2-10:
     21 |   let r = Test.transfer_to_contract c.contr (Main ("2022-01-01t10:10:10Z" : timestamp)) 0tez in
     22 |   Test.log r
            ^^^^^^^^
     23 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    Success (1279n)
    Everything at the top-level was executed.
    - test_timestamp exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "interpret_test.mligo" ];
  [%expect
    {|
    File "./interpret_test.mligo", line 401, characters 19-28:
    400 |   let () = assert (Crypto.sha256 (Bytes.pack 5n) = hash5n) in
    401 |   let () = assert (Test.eval (Crypto.sha256 (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.sha256 (Bytes.pack n)) 5n) in
                             ^^^^^^^^^
    402 |   assert (Crypto.sha256 (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 401, characters 63-71:
    400 |   let () = assert (Crypto.sha256 (Bytes.pack 5n) = hash5n) in
    401 |   let () = assert (Test.eval (Crypto.sha256 (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.sha256 (Bytes.pack n)) 5n) in
                                                                         ^^^^^^^^
    402 |   assert (Crypto.sha256 (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 410, characters 19-28:
    409 |   let () = assert (Crypto.sha512 (Bytes.pack 5n) = hash5n) in
    410 |   let () = assert (Test.eval (Crypto.sha512 (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.sha512 (Bytes.pack n)) 5n) in
                             ^^^^^^^^^
    411 |   assert (Crypto.sha512 (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 410, characters 63-71:
    409 |   let () = assert (Crypto.sha512 (Bytes.pack 5n) = hash5n) in
    410 |   let () = assert (Test.eval (Crypto.sha512 (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.sha512 (Bytes.pack n)) 5n) in
                                                                         ^^^^^^^^
    411 |   assert (Crypto.sha512 (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 417, characters 19-28:
    416 |   let () = assert (Crypto.blake2b (Bytes.pack 5n) = hash5n) in
    417 |   let () = assert (Test.eval (Crypto.blake2b (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.blake2b (Bytes.pack n)) 5n) in
                             ^^^^^^^^^
    418 |   assert (Crypto.blake2b (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 417, characters 64-72:
    416 |   let () = assert (Crypto.blake2b (Bytes.pack 5n) = hash5n) in
    417 |   let () = assert (Test.eval (Crypto.blake2b (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.blake2b (Bytes.pack n)) 5n) in
                                                                          ^^^^^^^^
    418 |   assert (Crypto.blake2b (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 424, characters 19-28:
    423 |   let () = assert (Crypto.keccak (Bytes.pack 5n) = hash5n) in
    424 |   let () = assert (Test.eval (Crypto.keccak (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.keccak (Bytes.pack n)) 5n) in
                             ^^^^^^^^^
    425 |   assert (Crypto.keccak (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 424, characters 63-71:
    423 |   let () = assert (Crypto.keccak (Bytes.pack 5n) = hash5n) in
    424 |   let () = assert (Test.eval (Crypto.keccak (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.keccak (Bytes.pack n)) 5n) in
                                                                         ^^^^^^^^
    425 |   assert (Crypto.keccak (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 431, characters 19-28:
    430 |   let () = assert (Crypto.sha3 (Bytes.pack 5n) = hash5n) in
    431 |   let () = assert (Test.eval (Crypto.sha3 (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.sha3 (Bytes.pack n)) 5n) in
                             ^^^^^^^^^
    432 |   assert (Crypto.sha3 (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 431, characters 61-69:
    430 |   let () = assert (Crypto.sha3 (Bytes.pack 5n) = hash5n) in
    431 |   let () = assert (Test.eval (Crypto.sha3 (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.sha3 (Bytes.pack n)) 5n) in
                                                                       ^^^^^^^^
    432 |   assert (Crypto.sha3 (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 437, characters 19-28:
    436 |   let key_hash = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : key_hash) in
    437 |   let () = assert (Test.eval (Crypto.hash_key key) = Test.run (fun (k : key) -> Crypto.hash_key k) key) in
                             ^^^^^^^^^
    438 |   assert (Crypto.hash_key key = key_hash)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 437, characters 53-61:
    436 |   let key_hash = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : key_hash) in
    437 |   let () = assert (Test.eval (Crypto.hash_key key) = Test.run (fun (k : key) -> Crypto.hash_key k) key) in
                                                               ^^^^^^^^
    438 |   assert (Crypto.hash_key key = key_hash)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 451, characters 17-25:
    450 |   let alpha_int = int alpha in
    451 |   let mich_int = Test.run (fun (_ : unit) -> int (0xe406000000000000000000000000000000000000000000000000000000000000 : bls12_381_fr)) () in
                           ^^^^^^^^
    452 |   assert (Test.eval alpha_int = mich_int)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 452, characters 10-19:
    451 |   let mich_int = Test.run (fun (_ : unit) -> int (0xe406000000000000000000000000000000000000000000000000000000000000 : bls12_381_fr)) () in
    452 |   assert (Test.eval alpha_int = mich_int)
                    ^^^^^^^^^
    453 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 456, characters 10-19:
    455 |   let f ((x, y) : nat * int) : int = x * not y in
    456 |   assert (Test.eval (f (313n , 2938818607801353443)) = Test.run f (313n , 2938818607801353443))
                    ^^^^^^^^^
    457 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 456, characters 55-63:
    455 |   let f ((x, y) : nat * int) : int = x * not y in
    456 |   assert (Test.eval (f (313n , 2938818607801353443)) = Test.run f (313n , 2938818607801353443))
                                                                 ^^^^^^^^
    457 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 459, characters 17-26:
    458 | let test_chain_id =
    459 |   let chain_id = Test.eval ("NetXH12Aer3be93" : chain_id) in
                           ^^^^^^^^^
    460 |   assert (chain_id = Test.eval (Tezos.get_chain_id ()))
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 460, characters 21-30:
    459 |   let chain_id = Test.eval ("NetXH12Aer3be93" : chain_id) in
    460 |   assert (chain_id = Test.eval (Tezos.get_chain_id ()))
                               ^^^^^^^^^
    461 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 469, characters 19-27:
    468 |   let () = assert (Bytes.concats [] = Bytes.sub 0n 0n (0x00 : bytes)) in
    469 |   let () = assert (Test.run (fun () -> String.concats ss) () = Test.eval (String.concats ss)) in
                             ^^^^^^^^
    470 |   let () = assert (Test.run (fun () -> Bytes.concats bs) () = Test.eval (Bytes.concats bs)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 469, characters 63-72:
    468 |   let () = assert (Bytes.concats [] = Bytes.sub 0n 0n (0x00 : bytes)) in
    469 |   let () = assert (Test.run (fun () -> String.concats ss) () = Test.eval (String.concats ss)) in
                                                                         ^^^^^^^^^
    470 |   let () = assert (Test.run (fun () -> Bytes.concats bs) () = Test.eval (Bytes.concats bs)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 470, characters 19-27:
    469 |   let () = assert (Test.run (fun () -> String.concats ss) () = Test.eval (String.concats ss)) in
    470 |   let () = assert (Test.run (fun () -> Bytes.concats bs) () = Test.eval (Bytes.concats bs)) in
                             ^^^^^^^^
    471 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test.mligo", line 470, characters 62-71:
    469 |   let () = assert (Test.run (fun () -> String.concats ss) () = Test.eval (String.concats ss)) in
    470 |   let () = assert (Test.run (fun () -> Bytes.concats bs) () = Test.eval (Bytes.concats bs)) in
                                                                        ^^^^^^^^^
    471 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_lambda_call exited with value ().
    - test_higher_order1 exited with value ().
    - test_higher_order2 exited with value ().
    - test_higher_order3 exited with value ().
    - test_higher_order4 exited with value ().
    - test_concats exited with value ().
    - test_record_concat exited with value ().
    - test_record_patch exited with value ().
    - test_record_lambda exited with value ().
    - test_variant_match exited with value ().
    - test_bool_match exited with value ().
    - test_list_match exited with value ().
    - test_tuple_proj exited with value ().
    - test_list_const exited with value ().
    - test_options_match_some exited with value ().
    - test_options_match_none exited with value ().
    - test_is_nat_yes exited with value ().
    - test_is_nat_no exited with value ().
    - test_abs_int exited with value ().
    - test_nat_int exited with value ().
    - test_map_list exited with value ().
    - test_fold_list exited with value ().
    - test_comparison_int exited with value ().
    - test_comparison_string exited with value ().
    - test_divs_int exited with value ().
    - test_divs_nat exited with value ().
    - test_var_neg exited with value ().
    - test_sizes exited with value ().
    - test_modi exited with value ().
    - test_assertion_pass exited with value ().
    - test_map_finds exited with value ().
    - test_map_fold exited with value ().
    - test_map_map exited with value ().
    - test_map_mem exited with value ().
    - test_map_remove exited with value ().
    - test_map_update exited with value ().
    - test_set_add exited with value ().
    - test_set_mem exited with value ().
    - test_set_remove exited with value ().
    - test_recursion_let_rec_in exited with value ().
    - test_top_level_recursion exited with value ().
    - test_bitwise_ops exited with value ().
    - test_bitwise_module exited with value ().
    - test_bytes_bitwise_ops exited with value ().
    - test_bytes_bitwise_module exited with value ().
    - test_list_concat exited with value ().
    - test_list_head_opt exited with value ().
    - test_list_tail_opt exited with value ().
    - test_list_reverse exited with value ().
    - test_set_fold_desc exited with value ().
    - test_set_update exited with value ().
    - test_map_get_and_update exited with value ().
    - test_big_map_get_and_update exited with value ().
    - test_add_mutez exited with value ().
    - test_sub_mutez exited with value ().
    - test_div_mutez exited with value ().
    - test_sub_timestamp exited with value ().
    - test_list_fold_left_sum exited with value ().
    - test_bytes_sub exited with value ().
    - test_with_error exited with value ().
    - test_some exited with value ().
    - test_some_with_error exited with value ().
    - test_none exited with value ().
    - test_none_with_error exited with value ().
    - test_unopt exited with value ().
    - test_unopt_with_error exited with value ().
    - test_sha256 exited with value ().
    - test_sha512 exited with value ().
    - test_blake2b exited with value ().
    - test_keccak exited with value ().
    - test_sha3 exited with value ().
    - test_key_hash exited with value ().
    - test_check exited with value ().
    - test_int_bls exited with value ().
    - test_not exited with value ().
    - test_chain_id exited with value ().
    - test_concats exited with value ().
    - test_bytes_nat_int_conversion exited with value (). |}]

let%expect_test _ =
  (* This tests a possible regression on the way modules are evaluated. It is possible that the number of element in the environment explodes. *)
  run_ligo_good [ "run"; "test"; test "imported_modules/test.mligo"; "--format"; "dev" ];
  [%expect
    {|
    File "./imported_modules/test.mligo", line 4, characters 10-24:
      3 | let test1 =
      4 |   let _ = Test.originate (contract_of Main) "a" 1tez in
                    ^^^^^^^^^^^^^^
      5 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test1 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "views_test.mligo" ];
  [%expect
    {|
    File "./views_test.mligo", line 3, characters 15-41:
      2 | let test =
      3 |   let _baker = Test.nth_bootstrap_account 0 in
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |   let _src = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./views_test.mligo", line 4, characters 13-39:
      3 |   let _baker = Test.nth_bootstrap_account 0 in
      4 |   let _src = Test.nth_bootstrap_account 1 in
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |   let init_storage = 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./views_test.mligo", line 6, characters 46-60:
      5 |   let init_storage = 0 in
      6 |   let {addr = addr_v ; code = _ ; size = _} = Test.originate (contract_of CUT.Main_with_view) init_storage 0mutez in
                                                        ^^^^^^^^^^^^^^
      7 |   let {addr = addr_c ; code = _ ; size = _} = Test.originate (contract_of CUT.Caller) init_storage 0mutez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./views_test.mligo", line 7, characters 46-60:
      6 |   let {addr = addr_v ; code = _ ; size = _} = Test.originate (contract_of CUT.Main_with_view) init_storage 0mutez in
      7 |   let {addr = addr_c ; code = _ ; size = _} = Test.originate (contract_of CUT.Caller) init_storage 0mutez in
                                                        ^^^^^^^^^^^^^^
      8 |   let tx =
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./views_test.mligo", line 9, characters 4-17:
      8 |   let tx =
      9 |     Test.transfer
              ^^^^^^^^^^^^^
     10 |       addr_c
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./views_test.mligo", line 11, characters 13-28:
     10 |       addr_c
     11 |       (Main (Test.to_address addr_v))
                       ^^^^^^^^^^^^^^^
     12 |       1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

    File "./views_test.mligo", line 15, characters 14-30:
     14 |     Success _ ->
     15 |       let x = Test.get_storage addr_c in
                        ^^^^^^^^^^^^^^^^
     16 |       assert (x = 2)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "interpret_test_log.mligo" ];
  [%expect
    {|
    File "./interpret_test_log.mligo", line 9, characters 4-12:
      8 |   begin
      9 |     Test.log v1 ;
              ^^^^^^^^
     10 |     Test.log v2
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./interpret_test_log.mligo", line 10, characters 4-12:
      9 |     Test.log v1 ;
     10 |     Test.log v2
              ^^^^^^^^
     11 |   end
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    {a = 1 ; b = 2n ; c = "aaa"}
    One (())
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_fail.mligo" ];
  [%expect
    {|
    File "./test_fail.mligo", line 4, characters 13-27:
      3 | let test =
      4 |   let orig = Test.originate (contract_of C) () 0tez in
                       ^^^^^^^^^^^^^^
      5 |   let contr = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_fail.mligo", line 5, characters 14-30:
      4 |   let orig = Test.originate (contract_of C) () 0tez in
      5 |   let contr = Test.to_contract orig.addr in
                        ^^^^^^^^^^^^^^^^
      6 |   let addr = Tezos.address contr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_fail.mligo", line 7, characters 8-33:
      6 |   let addr = Tezos.address contr in
      7 |   match Test.transfer_to_contract contr (Main ()) 10tez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |   | Success _ -> (failwith "Should fail !" : michelson_program )
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value "my contract always fail". |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_fail_from_file.mligo" ];
  [%expect
    {|
    File "./test_fail_from_file.mligo", line 5, characters 15-23:
      4 | let test =
      5 |   let _vfail = Test.run (fun () -> fail_data) () in
                         ^^^^^^^^
      6 |   let orig = Test.originate_from_file under_test () 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./test_fail_from_file.mligo", line 6, characters 13-37:
      5 |   let _vfail = Test.run (fun () -> fail_data) () in
      6 |   let orig = Test.originate_from_file under_test () 0tez in
                       ^^^^^^^^^^^^^^^^^^^^^^^^
      7 |   match Test.transfer orig.addr () 10tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_fail_from_file.mligo", line 7, characters 8-21:
      6 |   let orig = Test.originate_from_file under_test () 0tez in
      7 |   match Test.transfer orig.addr () 10tez with
                  ^^^^^^^^^^^^^
      8 |   | Success _ -> (failwith "Should fail !" : michelson_program )
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_fail_from_file.mligo", line 12, characters 35-50:
     11 |     | Rejected (x, addr_fail) ->
     12 |       let () = assert (addr_fail = Test.to_address orig.addr) in
                                             ^^^^^^^^^^^^^^^
     13 |       x
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value "my contract always fail". |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "compile_expr.mligo" ];
  [%expect
    {|
  File "./compile_expr.mligo", line 11, characters 14-23:
   10 | let test1 =
   11 |   let d_one = Test.eval (1 + 3 + 2) in
                      ^^^^^^^^^
   12 |   let ret = Test.run (fun (x : (int * nat * string * bytes * unit)) -> f ({ one = x.0 ; two = x.1 ; three = x.2 ; four = x.3 ; five = x.4 } : some_r))
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 12, characters 12-20:
   11 |   let d_one = Test.eval (1 + 3 + 2) in
   12 |   let ret = Test.run (fun (x : (int * nat * string * bytes * unit)) -> f ({ one = x.0 ; two = x.1 ; three = x.2 ; four = x.3 ; five = x.4 } : some_r))
                    ^^^^^^^^
   13 |                    (1 + 3 + 2, 1n + 2n, "a" ^ "b", 0xFF00, ()) in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 14, characters 10-30:
   13 |                    (1 + 3 + 2, 1n + 2n, "a" ^ "b", 0xFF00, ()) in
   14 |   assert (Test.michelson_equal d_one ret)
                  ^^^^^^^^^^^^^^^^^^^^
   15 |
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 17, characters 11-20:
   16 | let test2 =
   17 |   let x1 = Test.eval (1,2) in
                   ^^^^^^^^^
   18 |   let x2 = Test.run (fun (x : int * int) -> x) (1, 2) in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 18, characters 11-19:
   17 |   let x1 = Test.eval (1,2) in
   18 |   let x2 = Test.run (fun (x : int * int) -> x) (1, 2) in
                   ^^^^^^^^
   19 |   let eq = Test.michelson_equal x1 x2 in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 19, characters 11-31:
   18 |   let x2 = Test.run (fun (x : int * int) -> x) (1, 2) in
   19 |   let eq = Test.michelson_equal x1 x2 in
                   ^^^^^^^^^^^^^^^^^^^^
   20 |   assert eq
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 23, characters 11-20:
   22 | let test3 =
   23 |   let x1 = Test.eval (Baz 1n : some_v) in
                   ^^^^^^^^^
   24 |   let x2 = Test.eval (Baz2 1n : some_v_2) in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 24, characters 11-20:
   23 |   let x1 = Test.eval (Baz 1n : some_v) in
   24 |   let x2 = Test.eval (Baz2 1n : some_v_2) in
                   ^^^^^^^^^
   25 |   assert (not (Test.michelson_equal x1 x2))
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 25, characters 15-35:
   24 |   let x2 = Test.eval (Baz2 1n : some_v_2) in
   25 |   assert (not (Test.michelson_equal x1 x2))
                       ^^^^^^^^^^^^^^^^^^^^
   26 |
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 28, characters 11-20:
   27 | let test4 =
   28 |   let x1 = Test.eval ({ one = 1 ; two = 2n ; three = "a" ; four = 0xFF00 ; five = () } : some_r) in
                   ^^^^^^^^^
   29 |   let x2 = Test.eval ({ one2 = 1 ; two2 = 2n ; three2 = "a" ; four2 = 0xFF00 ; five2 = () } : some_r_2) in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 29, characters 11-20:
   28 |   let x1 = Test.eval ({ one = 1 ; two = 2n ; three = "a" ; four = 0xFF00 ; five = () } : some_r) in
   29 |   let x2 = Test.eval ({ one2 = 1 ; two2 = 2n ; three2 = "a" ; four2 = 0xFF00 ; five2 = () } : some_r_2) in
                   ^^^^^^^^^
   30 |   assert (not (Test.michelson_equal x1 x2))
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr.mligo", line 30, characters 15-35:
   29 |   let x2 = Test.eval ({ one2 = 1 ; two2 = 2n ; three2 = "a" ; four2 = 0xFF00 ; five2 = () } : some_r_2) in
   30 |   assert (not (Test.michelson_equal x1 x2))
                       ^^^^^^^^^^^^^^^^^^^^
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "compile_expr_from_file.mligo" ];
  [%expect
    {|
  File "./compile_expr_from_file.mligo", line 3, characters 14-32:
    2 |   type r_comb = [@layout comb] { one : int ; two : nat ; three : string ; four : bytes ; five : unit } in
    3 |   let d_one = Test.compile_value (1 + 3 + 5) in
                      ^^^^^^^^^^^^^^^^^^
    4 |   let v = {one = 1 + 3 + 5 ; two = 1n +2n ; three = "a"^"b" ; four = 0xFF00 ; five = ()} in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 5, characters 12-20:
    4 |   let v = {one = 1 + 3 + 5 ; two = 1n +2n ; three = "a"^"b" ; four = 0xFF00 ; five = ()} in
    5 |   let ret = Test.run (fun (x:r_comb) -> x.one ) v in
                    ^^^^^^^^
    6 |   assert (Test.michelson_equal d_one ret)
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 6, characters 10-30:
    5 |   let ret = Test.run (fun (x:r_comb) -> x.one ) v in
    6 |   assert (Test.michelson_equal d_one ret)
                  ^^^^^^^^^^^^^^^^^^^^
    7 |
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 9, characters 11-29:
    8 | let test2 =
    9 |   let x1 = Test.compile_value (1,2) in
                   ^^^^^^^^^^^^^^^^^^
   10 |   let x2 = Test.run (fun () -> (1,2)) () in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 10, characters 11-19:
    9 |   let x1 = Test.compile_value (1,2) in
   10 |   let x2 = Test.run (fun () -> (1,2)) () in
                   ^^^^^^^^
   11 |   let eq = Test.michelson_equal x1 x2 in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 11, characters 11-31:
   10 |   let x2 = Test.run (fun () -> (1,2)) () in
   11 |   let eq = Test.michelson_equal x1 x2 in
                   ^^^^^^^^^^^^^^^^^^^^
   12 |   assert eq
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 17, characters 4-12:
   16 |     type r_comb = [@layout comb] { one : int ; two : nat ; three : string ; four : bytes ; five : unit } in
   17 |     Test.run (fun () -> ({one = 1 ; two = 2n ; three = "a" ; four = 0xFF00 ; five = ()}:r_comb)) ()
            ^^^^^^^^
   18 |   in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 21, characters 4-12:
   20 |     type r_tree = [@layout tree] { one : int ; two : nat ; three : string ; four : bytes ; five : unit } in
   21 |     Test.run (fun () -> ({one = 1 ; two = 2n ; three = "a" ; four = 0xFF00 ; five = ()}:r_tree)) ()
            ^^^^^^^^
   22 |   in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 23, characters 15-35:
   22 |   in
   23 |   assert (not (Test.michelson_equal x1 x2))
                       ^^^^^^^^^^^^^^^^^^^^
   24 |
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 28, characters 4-12:
   27 |     type v_comb = [@layout comb] | Foo of int | Bar of string | Bare of string | Baz of nat in
   28 |     Test.run (fun () -> (Baz 1n: v_comb)) ()
            ^^^^^^^^
   29 |   in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 32, characters 4-12:
   31 |     type v_tree = [@layout tree] | Foo of int | Bar of string | Bare of string | Baz of nat in
   32 |     Test.run (fun () -> (Baz 1n: v_tree)) ()
            ^^^^^^^^
   33 |   in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

  File "./compile_expr_from_file.mligo", line 34, characters 15-35:
   33 |   in
   34 |   assert (not (Test.michelson_equal x1 x2))
                       ^^^^^^^^^^^^^^^^^^^^
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_example.mligo" ];
  [%expect
    {|
    File "./test_example.mligo", line 4, characters 19-41:
      3 | let check_new_origination (src :address) : address =
      4 |   let last_origs = Test.last_originations () in
                             ^^^^^^^^^^^^^^^^^^^^^^
      5 |   match Map.find_opt src last_origs with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.last_originations` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 7, characters 15-26:
      6 |     | Some new_lst -> (
      7 |       let () = Test.assert (List.length new_lst = 1n) in
                         ^^^^^^^^^^^
      8 |       match new_lst with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 15, characters 15-41:
     14 | let test =
     15 |   let _baker = Test.nth_bootstrap_account 0 in
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
     16 |   let src = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 16, characters 12-38:
     15 |   let _baker = Test.nth_bootstrap_account 0 in
     16 |   let src = Test.nth_bootstrap_account 1 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
     17 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 18, characters 44-58:
     17 |
     18 |   let {addr = typed_addr; code = _; size} = Test.originate (contract_of C) None 0tez in
                                                      ^^^^^^^^^^^^^^
     19 |   let () = Test.assert ((None : C.storage) = (Test.get_storage typed_addr)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 19, characters 11-22:
     18 |   let {addr = typed_addr; code = _; size} = Test.originate (contract_of C) None 0tez in
     19 |   let () = Test.assert ((None : C.storage) = (Test.get_storage typed_addr)) in
                     ^^^^^^^^^^^
     20 |   let () = Test.assert (size < 300) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 19, characters 46-62:
     18 |   let {addr = typed_addr; code = _; size} = Test.originate (contract_of C) None 0tez in
     19 |   let () = Test.assert ((None : C.storage) = (Test.get_storage typed_addr)) in
                                                        ^^^^^^^^^^^^^^^^
     20 |   let () = Test.assert (size < 300) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 20, characters 11-22:
     19 |   let () = Test.assert ((None : C.storage) = (Test.get_storage typed_addr)) in
     20 |   let () = Test.assert (size < 300) in
                     ^^^^^^^^^^^
     21 |   let new_account1 = check_new_origination src in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 23, characters 10-27:
     22 |
     23 |   let _ = Test.transfer_exn typed_addr (Main Two) 10tez in
                    ^^^^^^^^^^^^^^^^^
     24 |   let new_account2 = check_new_origination new_account1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 25, characters 20-36:
     24 |   let new_account2 = check_new_origination new_account1 in
     25 |   let new_storage = Test.get_storage typed_addr in
                              ^^^^^^^^^^^^^^^^
     26 |   let expected_new_storage = Some new_account2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 27, characters 11-22:
     26 |   let expected_new_storage = Some new_account2 in
     27 |   let () = Test.assert (new_storage = expected_new_storage) in
                     ^^^^^^^^^^^
     28 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 29, characters 9-22:
     28 |
     29 |   match (Test.transfer typed_addr (Main One) 10tez : test_exec_result) with
                   ^^^^^^^^^^^^^
     30 |   | Success _ -> (failwith "contract did not fail" : michelson_program)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 36, characters 15-26:
     35 |       let (v,addr) = reject_data in
     36 |       let () = Test.assert (addr = new_account2) in
                         ^^^^^^^^^^^
     37 |       let () = Test.assert (addr = new_account2) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 37, characters 15-26:
     36 |       let () = Test.assert (addr = new_account2) in
     37 |       let () = Test.assert (addr = new_account2) in
                         ^^^^^^^^^^^
     38 |       let () = Test.assert (Test.michelson_equal v (Test.eval 111)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 38, characters 15-26:
     37 |       let () = Test.assert (addr = new_account2) in
     38 |       let () = Test.assert (Test.michelson_equal v (Test.eval 111)) in
                         ^^^^^^^^^^^
     39 |       v
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 38, characters 28-48:
     37 |       let () = Test.assert (addr = new_account2) in
     38 |       let () = Test.assert (Test.michelson_equal v (Test.eval 111)) in
                                      ^^^^^^^^^^^^^^^^^^^^
     39 |       v
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 38, characters 52-61:
     37 |       let () = Test.assert (addr = new_account2) in
     38 |       let () = Test.assert (Test.michelson_equal v (Test.eval 111)) in
                                                              ^^^^^^^^^
     39 |       v
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 51, characters 11-27:
     50 |   let overide_default_amounts = [ 8000tez ; 2mutez ] in // the [i]th element of the list overwrite default balance of the [i]th account
     51 |   let () = Test.reset_state number_of_account overide_default_amounts in
                     ^^^^^^^^^^^^^^^^
     52 |   // And by setting the source in between calls to `Test.transfer_to_contract` or `Test.originate`
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 53, characters 14-40:
     52 |   // And by setting the source in between calls to `Test.transfer_to_contract` or `Test.originate`
     53 |   let bsa0 = (Test.nth_bootstrap_account 0) in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     54 |   let bsa1 = (Test.nth_bootstrap_account 1) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 54, characters 14-40:
     53 |   let bsa0 = (Test.nth_bootstrap_account 0) in
     54 |   let bsa1 = (Test.nth_bootstrap_account 1) in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     55 |   let bsa2 = (Test.nth_bootstrap_account 2) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 55, characters 14-40:
     54 |   let bsa1 = (Test.nth_bootstrap_account 1) in
     55 |   let bsa2 = (Test.nth_bootstrap_account 2) in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     56 |   let bsa3 = (Test.nth_bootstrap_account 3) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 56, characters 14-40:
     55 |   let bsa2 = (Test.nth_bootstrap_account 2) in
     56 |   let bsa3 = (Test.nth_bootstrap_account 3) in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     57 |   let () = Test.set_source bsa3 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 57, characters 11-26:
     56 |   let bsa3 = (Test.nth_bootstrap_account 3) in
     57 |   let () = Test.set_source bsa3 in
                     ^^^^^^^^^^^^^^^
     58 |   let () = Test.set_baker bsa2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 58, characters 11-25:
     57 |   let () = Test.set_source bsa3 in
     58 |   let () = Test.set_baker bsa2 in
                     ^^^^^^^^^^^^^^
     59 |   // some balance tests:
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 61, characters 4-12:
     60 |   let tz = fun (n:nat) ->
     61 |     Test.run (fun (x : unit -> nat) -> x () * 1mutez) (fun (_ : unit) -> n)
              ^^^^^^^^
     62 |   in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 63, characters 11-22:
     62 |   in
     63 |   let () = Test.assert ((Test.get_balance_of_address bsa0) = 2000tez) in
                     ^^^^^^^^^^^
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 63, characters 25-52:
     62 |   in
     63 |   let () = Test.assert ((Test.get_balance_of_address bsa0) = 2000tez) in
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 64, characters 11-22:
     63 |   let () = Test.assert ((Test.get_balance_of_address bsa0) = 2000tez) in
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
                     ^^^^^^^^^^^
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 64, characters 25-52:
     63 |   let () = Test.assert ((Test.get_balance_of_address bsa0) = 2000tez) in
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 65, characters 11-22:
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
                     ^^^^^^^^^^^
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 65, characters 24-44:
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
                                  ^^^^^^^^^^^^^^^^^^^^
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 65, characters 46-55:
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
                                                        ^^^^^^^^^
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 65, characters 57-84:
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
                                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 66, characters 11-22:
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
                     ^^^^^^^^^^^
     67 |   let () = Test.assert ((Test.get_balance_of_address bsa3) = 3800000000000mutez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 66, characters 25-52:
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     67 |   let () = Test.assert ((Test.get_balance_of_address bsa3) = 3800000000000mutez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 67, characters 11-22:
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
     67 |   let () = Test.assert ((Test.get_balance_of_address bsa3) = 3800000000000mutez) in
                     ^^^^^^^^^^^
     68 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 67, characters 25-52:
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
     67 |   let () = Test.assert ((Test.get_balance_of_address bsa3) = 3800000000000mutez) in
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     68 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value 111.
    - test2 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_example.mligo" ];
  [%expect
    {|
    File "./test_example.mligo", line 4, characters 19-41:
      3 | let check_new_origination (src :address) : address =
      4 |   let last_origs = Test.last_originations () in
                             ^^^^^^^^^^^^^^^^^^^^^^
      5 |   match Map.find_opt src last_origs with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.last_originations` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 7, characters 15-26:
      6 |     | Some new_lst -> (
      7 |       let () = Test.assert (List.length new_lst = 1n) in
                         ^^^^^^^^^^^
      8 |       match new_lst with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 15, characters 15-41:
     14 | let test =
     15 |   let _baker = Test.nth_bootstrap_account 0 in
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
     16 |   let src = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 16, characters 12-38:
     15 |   let _baker = Test.nth_bootstrap_account 0 in
     16 |   let src = Test.nth_bootstrap_account 1 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
     17 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 18, characters 44-58:
     17 |
     18 |   let {addr = typed_addr; code = _; size} = Test.originate (contract_of C) None 0tez in
                                                      ^^^^^^^^^^^^^^
     19 |   let () = Test.assert ((None : C.storage) = (Test.get_storage typed_addr)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 19, characters 11-22:
     18 |   let {addr = typed_addr; code = _; size} = Test.originate (contract_of C) None 0tez in
     19 |   let () = Test.assert ((None : C.storage) = (Test.get_storage typed_addr)) in
                     ^^^^^^^^^^^
     20 |   let () = Test.assert (size < 300) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 19, characters 46-62:
     18 |   let {addr = typed_addr; code = _; size} = Test.originate (contract_of C) None 0tez in
     19 |   let () = Test.assert ((None : C.storage) = (Test.get_storage typed_addr)) in
                                                        ^^^^^^^^^^^^^^^^
     20 |   let () = Test.assert (size < 300) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 20, characters 11-22:
     19 |   let () = Test.assert ((None : C.storage) = (Test.get_storage typed_addr)) in
     20 |   let () = Test.assert (size < 300) in
                     ^^^^^^^^^^^
     21 |   let new_account1 = check_new_origination src in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 23, characters 10-27:
     22 |
     23 |   let _ = Test.transfer_exn typed_addr (Main Two) 10tez in
                    ^^^^^^^^^^^^^^^^^
     24 |   let new_account2 = check_new_origination new_account1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 25, characters 20-36:
     24 |   let new_account2 = check_new_origination new_account1 in
     25 |   let new_storage = Test.get_storage typed_addr in
                              ^^^^^^^^^^^^^^^^
     26 |   let expected_new_storage = Some new_account2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 27, characters 11-22:
     26 |   let expected_new_storage = Some new_account2 in
     27 |   let () = Test.assert (new_storage = expected_new_storage) in
                     ^^^^^^^^^^^
     28 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 29, characters 9-22:
     28 |
     29 |   match (Test.transfer typed_addr (Main One) 10tez : test_exec_result) with
                   ^^^^^^^^^^^^^
     30 |   | Success _ -> (failwith "contract did not fail" : michelson_program)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 36, characters 15-26:
     35 |       let (v,addr) = reject_data in
     36 |       let () = Test.assert (addr = new_account2) in
                         ^^^^^^^^^^^
     37 |       let () = Test.assert (addr = new_account2) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 37, characters 15-26:
     36 |       let () = Test.assert (addr = new_account2) in
     37 |       let () = Test.assert (addr = new_account2) in
                         ^^^^^^^^^^^
     38 |       let () = Test.assert (Test.michelson_equal v (Test.eval 111)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 38, characters 15-26:
     37 |       let () = Test.assert (addr = new_account2) in
     38 |       let () = Test.assert (Test.michelson_equal v (Test.eval 111)) in
                         ^^^^^^^^^^^
     39 |       v
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 38, characters 28-48:
     37 |       let () = Test.assert (addr = new_account2) in
     38 |       let () = Test.assert (Test.michelson_equal v (Test.eval 111)) in
                                      ^^^^^^^^^^^^^^^^^^^^
     39 |       v
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 38, characters 52-61:
     37 |       let () = Test.assert (addr = new_account2) in
     38 |       let () = Test.assert (Test.michelson_equal v (Test.eval 111)) in
                                                              ^^^^^^^^^
     39 |       v
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 51, characters 11-27:
     50 |   let overide_default_amounts = [ 8000tez ; 2mutez ] in // the [i]th element of the list overwrite default balance of the [i]th account
     51 |   let () = Test.reset_state number_of_account overide_default_amounts in
                     ^^^^^^^^^^^^^^^^
     52 |   // And by setting the source in between calls to `Test.transfer_to_contract` or `Test.originate`
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 53, characters 14-40:
     52 |   // And by setting the source in between calls to `Test.transfer_to_contract` or `Test.originate`
     53 |   let bsa0 = (Test.nth_bootstrap_account 0) in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     54 |   let bsa1 = (Test.nth_bootstrap_account 1) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 54, characters 14-40:
     53 |   let bsa0 = (Test.nth_bootstrap_account 0) in
     54 |   let bsa1 = (Test.nth_bootstrap_account 1) in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     55 |   let bsa2 = (Test.nth_bootstrap_account 2) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 55, characters 14-40:
     54 |   let bsa1 = (Test.nth_bootstrap_account 1) in
     55 |   let bsa2 = (Test.nth_bootstrap_account 2) in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     56 |   let bsa3 = (Test.nth_bootstrap_account 3) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 56, characters 14-40:
     55 |   let bsa2 = (Test.nth_bootstrap_account 2) in
     56 |   let bsa3 = (Test.nth_bootstrap_account 3) in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     57 |   let () = Test.set_source bsa3 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 57, characters 11-26:
     56 |   let bsa3 = (Test.nth_bootstrap_account 3) in
     57 |   let () = Test.set_source bsa3 in
                     ^^^^^^^^^^^^^^^
     58 |   let () = Test.set_baker bsa2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 58, characters 11-25:
     57 |   let () = Test.set_source bsa3 in
     58 |   let () = Test.set_baker bsa2 in
                     ^^^^^^^^^^^^^^
     59 |   // some balance tests:
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 61, characters 4-12:
     60 |   let tz = fun (n:nat) ->
     61 |     Test.run (fun (x : unit -> nat) -> x () * 1mutez) (fun (_ : unit) -> n)
              ^^^^^^^^
     62 |   in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 63, characters 11-22:
     62 |   in
     63 |   let () = Test.assert ((Test.get_balance_of_address bsa0) = 2000tez) in
                     ^^^^^^^^^^^
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 63, characters 25-52:
     62 |   in
     63 |   let () = Test.assert ((Test.get_balance_of_address bsa0) = 2000tez) in
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 64, characters 11-22:
     63 |   let () = Test.assert ((Test.get_balance_of_address bsa0) = 2000tez) in
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
                     ^^^^^^^^^^^
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 64, characters 25-52:
     63 |   let () = Test.assert ((Test.get_balance_of_address bsa0) = 2000tez) in
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 65, characters 11-22:
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
                     ^^^^^^^^^^^
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 65, characters 24-44:
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
                                  ^^^^^^^^^^^^^^^^^^^^
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 65, characters 46-55:
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
                                                        ^^^^^^^^^
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 65, characters 57-84:
     64 |   let () = Test.assert ((Test.get_balance_of_address bsa1) = 0mutez) in
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
                                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 66, characters 11-22:
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
                     ^^^^^^^^^^^
     67 |   let () = Test.assert ((Test.get_balance_of_address bsa3) = 3800000000000mutez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 66, characters 25-52:
     65 |   let () = Test.assert (Test.michelson_equal (Test.eval (Test.get_balance_of_address bsa1)) (tz 0n)) in
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     67 |   let () = Test.assert ((Test.get_balance_of_address bsa3) = 3800000000000mutez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 67, characters 11-22:
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
     67 |   let () = Test.assert ((Test.get_balance_of_address bsa3) = 3800000000000mutez) in
                     ^^^^^^^^^^^
     68 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_example.mligo", line 67, characters 25-52:
     66 |   let () = Test.assert ((Test.get_balance_of_address bsa2) = 3800000tez) in
     67 |   let () = Test.assert ((Test.get_balance_of_address bsa3) = 3800000000000mutez) in
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     68 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value 111.
    - test2 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "catch_balance_too_low.mligo" ];
  [%expect
    {|
    File "./catch_balance_too_low.mligo", line 4, characters 13-27:
      3 | let test =
      4 |   let orig = Test.originate (contract_of C) None 0tez in
                       ^^^^^^^^^^^^^^
      5 |   let contr = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./catch_balance_too_low.mligo", line 5, characters 14-30:
      4 |   let orig = Test.originate (contract_of C) None 0tez in
      5 |   let contr = Test.to_contract orig.addr in
                        ^^^^^^^^^^^^^^^^
      6 |   match Test.transfer_to_contract contr (Main Two) 1mutez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./catch_balance_too_low.mligo", line 6, characters 8-33:
      5 |   let contr = Test.to_contract orig.addr in
      6 |   match Test.transfer_to_contract contr (Main Two) 1mutez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |   (* TODO this is a bug :( *)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_subst_with_storage.mligo" ];
  [%expect
    {|
  File "./test_subst_with_storage.mligo", line 5, characters 13-27:
    4 |   let init_storage = {foo = 0 ; bar = "bar"} in
    5 |   let orig = Test.originate (contract_of C) init_storage 0tez in
                     ^^^^^^^^^^^^^^
    6 |   let store = Test.get_storage orig.addr in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

  File "./test_subst_with_storage.mligo", line 6, characters 14-30:
    5 |   let orig = Test.originate (contract_of C) init_storage 0tez in
    6 |   let store = Test.get_storage orig.addr in
                      ^^^^^^^^^^^^^^^^
    7 |   Test.eval store.foo
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

  File "./test_subst_with_storage.mligo", line 7, characters 2-11:
    6 |   let store = Test.get_storage orig.addr in
    7 |   Test.eval store.foo
          ^^^^^^^^^
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

  Everything at the top-level was executed.
  - test exited with value 0. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_subst_with_storage_from_file.mligo" ];
  [%expect
    {|
    File "./test_subst_with_storage_from_file.mligo", line 5, characters 55-79:
      4 | let test =
      5 |   let orig : (parameter, storage) origination_result = Test.originate_from_file cut {foo = 0 ; bar = "bar"} 0tez in
                                                                 ^^^^^^^^^^^^^^^^^^^^^^^^
      6 |   let store : storage = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_subst_with_storage_from_file.mligo", line 6, characters 24-40:
      5 |   let orig : (parameter, storage) origination_result = Test.originate_from_file cut {foo = 0 ; bar = "bar"} 0tez in
      6 |   let store : storage = Test.get_storage orig.addr in
                                  ^^^^^^^^^^^^^^^^
      7 |   let ovens_map = store.foo in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "nesting_modules.mligo" ];
  [%expect
    {|
    File "./nesting_modules.mligo", line 34, characters 13-27:
     33 | let test =
     34 |   let orig = Test.originate (contract_of C) 0 0tez in
                       ^^^^^^^^^^^^^^
     35 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./nesting_modules.mligo", line 35, characters 10-27:
     34 |   let orig = Test.originate (contract_of C) 0 0tez in
     35 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
     36 |   Test.log (Test.get_storage orig.addr)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./nesting_modules.mligo", line 36, characters 2-10:
     35 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
     36 |   Test.log (Test.get_storage orig.addr)
            ^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./nesting_modules.mligo", line 36, characters 12-28:
     35 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
     36 |   Test.log (Test.get_storage orig.addr)
                      ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    111
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "map_map.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value ["one" -> "foo" ; "two" -> "foo"]. |}]

(* DEPRECATED
let%expect_test _ =
run_ligo_good ["run";"test" ; test "bootstrapped_contracts.mligo" ] ;
  [%expect {|
  "Initial states:"
  (Pair "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy" 12)
  (Pair "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG" 9)
  "Final states:"
  (Pair "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy" 3)
  (Pair "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG" 0)
  Everything at the top-level was executed.
  - test_transfer exited with value ().
  |}]
*)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "override_function.mligo" ];
  [%expect
    {|
    File "./override_function.mligo", line 8, characters 10-18:
      7 | let test =
      8 |   let v = Test.run f 4 in
                    ^^^^^^^^
      9 |   Test.log v
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    File "./override_function.mligo", line 9, characters 2-10:
      8 |   let v = Test.run f 4 in
      9 |   Test.log v
            ^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    4
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_fresh.mligo" ];
  [%expect {| Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_rec_contract.mligo" ];
  [%expect
    {|
    File "./test_rec_contract.mligo", line 8, characters 43-57:
      7 | let test =
      8 |   let {addr = taddr; code = _; size = _} = Test.originate (contract_of C) () 0tez in
                                                     ^^^^^^^^^^^^^^
      9 |   let _contr = Test.to_contract taddr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_rec_contract.mligo", line 9, characters 15-31:
      8 |   let {addr = taddr; code = _; size = _} = Test.originate (contract_of C) () 0tez in
      9 |   let _contr = Test.to_contract taddr in
                         ^^^^^^^^^^^^^^^^
     10 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_importer.mligo" ];
  [%expect
    {|
    File "./test_importer.mligo", line 4, characters 13-27:
      3 | let test =
      4 |   let orig = Test.originate (contract_of External) External.D.default.initial 0tez in
                       ^^^^^^^^^^^^^^
      5 |   let contr = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_importer.mligo", line 5, characters 14-30:
      4 |   let orig = Test.originate (contract_of External) External.D.default.initial 0tez in
      5 |   let contr = Test.to_contract orig.addr in
                        ^^^^^^^^^^^^^^^^
      6 |   let () = assert (Test.get_storage orig.addr = External.D.default.initial) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_importer.mligo", line 6, characters 19-35:
      5 |   let contr = Test.to_contract orig.addr in
      6 |   let () = assert (Test.get_storage orig.addr = External.D.default.initial) in
                             ^^^^^^^^^^^^^^^^
      7 |   let _ = Test.transfer_to_contract_exn contr (Main (External.D.default.final)) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_importer.mligo", line 7, characters 10-39:
      6 |   let () = assert (Test.get_storage orig.addr = External.D.default.initial) in
      7 |   let _ = Test.transfer_to_contract_exn contr (Main (External.D.default.final)) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |   assert (Test.get_storage orig.addr = External.D.default.final)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_importer.mligo", line 8, characters 10-26:
      7 |   let _ = Test.transfer_to_contract_exn contr (Main (External.D.default.final)) 0tez in
      8 |   assert (Test.get_storage orig.addr = External.D.default.final)
                    ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_bigmap.mligo" ];
  [%expect
    {|
    File "./test_bigmap.mligo", line 11, characters 10-24:
     10 |   let init = Big_map.add 12 42n Big_map.empty in
     11 |   let _ = Test.originate (contract_of C) init 0tez in
                    ^^^^^^^^^^^^^^
     12 |   let init = Big_map.add 32 42n Big_map.empty in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 13, characters 13-27:
     12 |   let init = Big_map.add 32 42n Big_map.empty in
     13 |   let orig = Test.originate (contract_of C) init 0tez in
                       ^^^^^^^^^^^^^^
     14 |   let ctr = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 14, characters 12-28:
     13 |   let orig = Test.originate (contract_of C) init 0tez in
     14 |   let ctr = Test.to_contract orig.addr in
                      ^^^^^^^^^^^^^^^^
     15 |   let m_old = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 15, characters 14-30:
     14 |   let ctr = Test.to_contract orig.addr in
     15 |   let m_old = Test.get_storage orig.addr in
                        ^^^^^^^^^^^^^^^^
     16 |   let () = Test.log m_old in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 16, characters 11-19:
     15 |   let m_old = Test.get_storage orig.addr in
     16 |   let () = Test.log m_old in
                     ^^^^^^^^
     17 |   let () = Test.log (Big_map.find_opt 21 m_old) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 17, characters 11-19:
     16 |   let () = Test.log m_old in
     17 |   let () = Test.log (Big_map.find_opt 21 m_old) in
                     ^^^^^^^^
     18 |   let _ = Test.transfer_to_contract_exn ctr (Main (21, 42n)) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 18, characters 10-39:
     17 |   let () = Test.log (Big_map.find_opt 21 m_old) in
     18 |   let _ = Test.transfer_to_contract_exn ctr (Main (21, 42n)) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     19 |   let _ = Test.transfer_to_contract_exn ctr (Main (3, 42n)) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 19, characters 10-39:
     18 |   let _ = Test.transfer_to_contract_exn ctr (Main (21, 42n)) 0tez in
     19 |   let _ = Test.transfer_to_contract_exn ctr (Main (3, 42n)) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     20 |   let m_new = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 20, characters 14-30:
     19 |   let _ = Test.transfer_to_contract_exn ctr (Main (3, 42n)) 0tez in
     20 |   let m_new = Test.get_storage orig.addr in
                        ^^^^^^^^^^^^^^^^
     21 |   let () = Test.log m_old in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 21, characters 11-19:
     20 |   let m_new = Test.get_storage orig.addr in
     21 |   let () = Test.log m_old in
                     ^^^^^^^^
     22 |   let () = Test.log m_new in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 22, characters 11-19:
     21 |   let () = Test.log m_old in
     22 |   let () = Test.log m_new in
                     ^^^^^^^^
     23 |   let () = Test.log (Big_map.find_opt 21 m_old) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 23, characters 11-19:
     22 |   let () = Test.log m_new in
     23 |   let () = Test.log (Big_map.find_opt 21 m_old) in
                     ^^^^^^^^
     24 |   let () = Test.log (Big_map.find_opt 21 m_new) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap.mligo", line 24, characters 11-19:
     23 |   let () = Test.log (Big_map.find_opt 21 m_old) in
     24 |   let () = Test.log (Big_map.find_opt 21 m_new) in
                     ^^^^^^^^
     25 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    [32 -> 42n]
    None (())
    [32 -> 42n]
    [3 -> 42n ; 21 -> 42n ; 32 -> 42n]
    None (())
    Some (42n)
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_bigmap_compare.mligo" ];
  [%expect
    {|
    File "./test_bigmap_compare.mligo", line 8, characters 13-29:
      7 | let test =
      8 |     let () = Test.reset_state 10n ([] : tez list) in
                       ^^^^^^^^^^^^^^^^
      9 |     let a1 = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_compare.mligo", line 9, characters 13-39:
      8 |     let () = Test.reset_state 10n ([] : tez list) in
      9 |     let a1 = Test.nth_bootstrap_account 1 in
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |     let initial_storage = Big_map.literal [((a1, 0n), 42n)] in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_compare.mligo", line 11, characters 15-29:
     10 |     let initial_storage = Big_map.literal [((a1, 0n), 42n)] in
     11 |     let orig = Test.originate (contract_of C) initial_storage 0tez in
                         ^^^^^^^^^^^^^^
     12 |     let () = Test.set_source a1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_compare.mligo", line 12, characters 13-28:
     11 |     let orig = Test.originate (contract_of C) initial_storage 0tez in
     12 |     let () = Test.set_source a1 in
                       ^^^^^^^^^^^^^^^
     13 |     let _ = Test.transfer_exn orig.addr (Main (a1, 1234n)) 1mutez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_compare.mligo", line 13, characters 12-29:
     12 |     let () = Test.set_source a1 in
     13 |     let _ = Test.transfer_exn orig.addr (Main (a1, 1234n)) 1mutez in
                      ^^^^^^^^^^^^^^^^^
     14 |     let ns = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_compare.mligo", line 14, characters 13-29:
     13 |     let _ = Test.transfer_exn orig.addr (Main (a1, 1234n)) 1mutez in
     14 |     let ns = Test.get_storage orig.addr in
                       ^^^^^^^^^^^^^^^^
     15 |     let () = assert (ns = Big_map.literal [((a1, 0n), 42n); ((a1, 1n), 1234n)]) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_compare.mligo", line 17, characters 12-29:
     16 |     let () = assert (Big_map.find_opt (a1, 1n) ns = Some 1234n) in
     17 |     let _ = Test.transfer_exn orig.addr (Main (a1, 4321n)) 1mutez in
                      ^^^^^^^^^^^^^^^^^
     18 |     let () = assert (ns = Big_map.literal [((a1, 0n), 42n); ((a1, 1n), 1234n)]) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_compare.mligo", line 19, characters 13-29:
     18 |     let () = assert (ns = Big_map.literal [((a1, 0n), 42n); ((a1, 1n), 1234n)]) in
     19 |     let ns = Test.get_storage orig.addr in
                       ^^^^^^^^^^^^^^^^
     20 |     let () = assert (ns = Big_map.literal [((a1, 0n), 42n); ((a1, 1n), 4321n)]) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_bigmap_set.mligo" ];
  [%expect
    {|
    File "./test_bigmap_set.mligo", line 9, characters 43-57:
      8 | let test =
      9 |   let {addr = taddr; code = _; size = _} = Test.originate (contract_of C) Big_map.empty 0tez in
                                                     ^^^^^^^^^^^^^^
     10 |   let y : nat = 1n in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_set.mligo", line 11, characters 10-27:
     10 |   let y : nat = 1n in
     11 |   let _ = Test.transfer_exn taddr (Main (21, (fun (x : nat) -> x * 2n + y))) 0tez in
                    ^^^^^^^^^^^^^^^^^
     12 |   let _y : nat = 100n in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_set.mligo", line 14, characters 11-27:
     13 |   let init = Big_map.add 21 (fun (_ : nat) -> 0n) (Big_map.empty : (int, nat -> nat) big_map) in
     14 |   let () = Test.set_big_map 5 init in
                     ^^^^^^^^^^^^^^^^
     15 |   let m_new = Test.get_storage taddr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_big_map` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_set.mligo", line 15, characters 14-30:
     14 |   let () = Test.set_big_map 5 init in
     15 |   let m_new = Test.get_storage taddr in
                        ^^^^^^^^^^^^^^^^
     16 |   let v = Big_map.find_opt 21 m_new in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_set.mligo", line 19, characters 15-23:
     18 |   | Some f ->
     19 |       let () = Test.log (f 4n) in
                         ^^^^^^^^
     20 |       let () = Test.set_big_map 4 init in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_set.mligo", line 20, characters 15-31:
     19 |       let () = Test.log (f 4n) in
     20 |       let () = Test.set_big_map 4 init in
                         ^^^^^^^^^^^^^^^^
     21 |       let m_new = Test.get_storage taddr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_big_map` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_set.mligo", line 21, characters 18-34:
     20 |       let () = Test.set_big_map 4 init in
     21 |       let m_new = Test.get_storage taddr in
                            ^^^^^^^^^^^^^^^^
     22 |       let v = Big_map.find_opt 21 m_new in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_set.mligo", line 25, characters 11-19:
     24 |        | Some f ->
     25 |            Test.log (f 4n)
                     ^^^^^^^^
     26 |        | None ->
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_set.mligo", line 27, characters 11-19:
     26 |        | None ->
     27 |            Test.log "Error")
                     ^^^^^^^^
     28 |   | None ->
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_bigmap_set.mligo", line 29, characters 6-14:
     28 |   | None ->
     29 |       Test.log "Error"
                ^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    9n
    0n
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_module.mligo" ];
  [%expect
    {|
    File "./test_module.mligo", line 13, characters 13-27:
     12 | let test =
     13 |   let orig = Test.originate (contract_of C) 0 0tez in
                       ^^^^^^^^^^^^^^
     14 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_module.mligo", line 14, characters 10-27:
     13 |   let orig = Test.originate (contract_of C) 0 0tez in
     14 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
     15 |   Test.get_storage orig.addr
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_module.mligo", line 15, characters 2-18:
     14 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
     15 |   Test.get_storage orig.addr
            ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value 1. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "interpreter_nested_comparison_test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value ().
    - test_equal exited with value ().
    - test_not_equal exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_no_mutation.mligo" ];
  [%expect
    {|
    File "./test_no_mutation.mligo", line 28, characters 13-27:
     27 |   let initial_storage = 7 in
     28 |   let orig = Test.originate contract initial_storage 0tez in
                       ^^^^^^^^^^^^^^
     29 |   let _ = Test.transfer_exn orig.addr (Main (Increment (7)) : C parameter_of) 1mutez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_no_mutation.mligo", line 29, characters 10-27:
     28 |   let orig = Test.originate contract initial_storage 0tez in
     29 |   let _ = Test.transfer_exn orig.addr (Main (Increment (7)) : C parameter_of) 1mutez in
                    ^^^^^^^^^^^^^^^^^
     30 |   Test.assert (Test.get_storage orig.addr = initial_storage + 7)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_no_mutation.mligo", line 30, characters 2-13:
     29 |   let _ = Test.transfer_exn orig.addr (Main (Increment (7)) : C parameter_of) 1mutez in
     30 |   Test.assert (Test.get_storage orig.addr = initial_storage + 7)
            ^^^^^^^^^^^
     31 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_no_mutation.mligo", line 30, characters 15-31:
     29 |   let _ = Test.transfer_exn orig.addr (Main (Increment (7)) : C parameter_of) 1mutez in
     30 |   Test.assert (Test.get_storage orig.addr = initial_storage + 7)
                         ^^^^^^^^^^^^^^^^
     31 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_no_mutation.mligo", line 35, characters 8-26:
     34 | let test_mutation =
     35 |   match Test.mutation_test (contract_of C) originate_and_test with
                  ^^^^^^^^^^^^^^^^^^
     36 |     None -> ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.func` from `Test.Next` is encouraged for a smoother migration.

    File "./test_no_mutation.mligo", line 37, characters 35-43:
     36 |     None -> ()
     37 |   | Some (_, mutation) -> let () = Test.log(mutation) in
                                             ^^^^^^^^
     38 |                           failwith "Some mutation also passes the tests! ^^"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_no_mutation.mligo", line 41, characters 8-30:
     40 | let test_mutation_all =
     41 |   match Test.mutation_test_all (contract_of C) originate_and_test with
                  ^^^^^^^^^^^^^^^^^^^^^^
     42 |     [] -> ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.func` from `Test.Next` is encouraged for a smoother migration.

    File "./test_no_mutation.mligo", line 43, characters 35-43:
     42 |     [] -> ()
     43 |   | (_, mutation) :: _ -> let () = Test.log(mutation) in
                                             ^^^^^^^^
     44 |                           failwith "Some mutation also passes the tests! ^^"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value ().
    - test_mutation exited with value ().
    - test_mutation_all exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_mutate_from_file.mligo" ];
  [%expect
    {|
    File "./test_mutate_from_file.mligo", line 4, characters 12-29:
      3 |     (* Test 1 *)
      4 |     let _ = Test.transfer_exn a 0 0tez in
                      ^^^^^^^^^^^^^^^^^
      5 |     let () = assert (Test.get_storage a = 0) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_from_file.mligo", line 5, characters 21-37:
      4 |     let _ = Test.transfer_exn a 0 0tez in
      5 |     let () = assert (Test.get_storage a = 0) in
                               ^^^^^^^^^^^^^^^^
      6 |     (* Test 2 *)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_from_file.mligo", line 7, characters 12-29:
      6 |     (* Test 2 *)
      7 |     let _ = Test.transfer_exn a 1 0tez in
                      ^^^^^^^^^^^^^^^^^
      8 |     let () = assert (Test.get_storage a = 1) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_from_file.mligo", line 8, characters 21-37:
      7 |     let _ = Test.transfer_exn a 1 0tez in
      8 |     let () = assert (Test.get_storage a = 1) in
                               ^^^^^^^^^^^^^^^^
      9 |     ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_from_file.mligo", line 12, characters 2-41:
     11 |   let fn = "adder.mligo" in
     12 |   Test.originate_from_file_and_mutate_all fn 0 0tez tester
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.from_file` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value [(() , Mutation at: File "adder.mligo", line 2, characters 58-63:
      1 | [@entry]
      2 | let main (p : int) (k : int) : operation list * int = [], p + k
                                                                    ^^^^^

    Replacing by: p ^ k.
    )]. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_mutate_module.mligo" ];
  [%expect
    {|
    File "./test_mutate_module.mligo", line 4, characters 42-58:
      3 | let _tester (a : (Adder parameter_of, int) typed_address) (_ : (Adder parameter_of, int) michelson_contract) (_ : int) : unit =
      4 |   let c : (Adder parameter_of) contract = Test.to_contract a in
                                                    ^^^^^^^^^^^^^^^^
      5 |   (* Test 1 *)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.mligo", line 6, characters 10-39:
      5 |   (* Test 1 *)
      6 |   let _ = Test.transfer_to_contract_exn c (Add 0) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |   let () = assert (Test.get_storage a = 0) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.mligo", line 7, characters 19-35:
      6 |   let _ = Test.transfer_to_contract_exn c (Add 0) 0tez in
      7 |   let () = assert (Test.get_storage a = 0) in
                             ^^^^^^^^^^^^^^^^
      8 |   (* Test 2 *)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.mligo", line 9, characters 10-39:
      8 |   (* Test 2 *)
      9 |   let _ = Test.transfer_to_contract_exn c (Add 1) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |   let () = assert (Test.get_storage a = 1) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.mligo", line 10, characters 19-35:
      9 |   let _ = Test.transfer_to_contract_exn c (Add 1) 0tez in
     10 |   let () = assert (Test.get_storage a = 1) in
                             ^^^^^^^^^^^^^^^^
     11 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.mligo", line 14, characters 4-33:
     13 | let test =
     14 |     Test.originate_and_mutate_all (contract_of Adder) 0 0tez _tester
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.contract` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value [(() , Mutation at: File "contract_under_test/module_adder.mligo", line 1, characters 66-71:
      1 | [@entry] let add (p : int) (k : int) : operation list * int = [], p + k
                                                                            ^^^^^

    Replacing by: p ^ k.
    )]. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_mutate_module.jsligo" ];
  [%expect
    {|
    File "./test_mutate_module.jsligo", line 4, characters 41-57:
      3 | const _tester = (a : typed_address<parameter_of Adder, int>, _c : michelson_contract<parameter_of Adder, int>, _i : int) : unit => {
      4 |   let c : contract<parameter_of Adder> = Test.to_contract(a);
                                                   ^^^^^^^^^^^^^^^^
      5 |   /* Test 1 */
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.jsligo", line 6, characters 2-31:
      5 |   /* Test 1 */
      6 |   Test.transfer_to_contract_exn(c, Add(0), 0tez);
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |   Test.assert(Test.get_storage(a) == 0);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.jsligo", line 7, characters 2-13:
      6 |   Test.transfer_to_contract_exn(c, Add(0), 0tez);
      7 |   Test.assert(Test.get_storage(a) == 0);
            ^^^^^^^^^^^
      8 |   /* Test 2 */
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.jsligo", line 7, characters 14-30:
      6 |   Test.transfer_to_contract_exn(c, Add(0), 0tez);
      7 |   Test.assert(Test.get_storage(a) == 0);
                        ^^^^^^^^^^^^^^^^
      8 |   /* Test 2 */
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.jsligo", line 9, characters 2-31:
      8 |   /* Test 2 */
      9 |   Test.transfer_to_contract_exn(c, Add(1), 0tez);
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |   Test.assert(Test.get_storage(a) == 1);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.jsligo", line 10, characters 2-13:
      9 |   Test.transfer_to_contract_exn(c, Add(1), 0tez);
     10 |   Test.assert(Test.get_storage(a) == 1);
            ^^^^^^^^^^^
     11 | };
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.jsligo", line 10, characters 14-30:
      9 |   Test.transfer_to_contract_exn(c, Add(1), 0tez);
     10 |   Test.assert(Test.get_storage(a) == 1);
                        ^^^^^^^^^^^^^^^^
     11 | };
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.jsligo", line 14, characters 12-41:
     13 | const test = (() : unit => {
     14 |     let l = Test.originate_and_mutate_all(contract_of(Adder), 0, (0 as tez), _tester);
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     15 |     Test.log(l);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_mutate_module.jsligo", line 15, characters 4-12:
     14 |     let l = Test.originate_and_mutate_all(contract_of(Adder), 0, (0 as tez), _tester);
     15 |     Test.log(l);
              ^^^^^^^^
     16 | })();
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    [(() , Mutation at: File "contract_under_test/module_adder.mligo", line 1, characters 66-71:
      1 | [@entry] let add (p : int) (k : int) : operation list * int = [], p + k
                                                                            ^^^^^

    Replacing by: p ^ k.
    )]
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "iteration.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_set exited with value 3.
    - test_list exited with value 3. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "func_michelson.mligo" ];
  [%expect
    {|
    File "./func_michelson.mligo", line 11, characters 13-27:
     10 | let test =
     11 |   let orig = Test.originate (contract_of C) 1 0tez in
                       ^^^^^^^^^^^^^^
     12 |   let _ = Test.transfer_exn orig.addr (Main 41) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./func_michelson.mligo", line 12, characters 10-27:
     11 |   let orig = Test.originate (contract_of C) 1 0tez in
     12 |   let _ = Test.transfer_exn orig.addr (Main 41) 0tez in
                    ^^^^^^^^^^^^^^^^^
     13 |   Test.log (Test.get_storage orig.addr)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./func_michelson.mligo", line 13, characters 2-10:
     12 |   let _ = Test.transfer_exn orig.addr (Main 41) 0tez in
     13 |   Test.log (Test.get_storage orig.addr)
            ^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./func_michelson.mligo", line 13, characters 12-28:
     12 |   let _ = Test.transfer_exn orig.addr (Main 41) 0tez in
     13 |   Test.log (Test.get_storage orig.addr)
                      ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    42
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "func_michelson_loop.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_many_imports.mligo" ];
  [%expect
    {|
    File "./test_many_imports.mligo", line 4, characters 13-27:
      3 | let test =
      4 |   let orig = Test.originate (contract_of C) () 0tez in
                       ^^^^^^^^^^^^^^
      5 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_many_imports.mligo", line 5, characters 10-27:
      4 |   let orig = Test.originate (contract_of C) () 0tez in
      5 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
      6 |   assert (Test.get_storage orig.addr = ())
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_many_imports.mligo", line 6, characters 10-26:
      5 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
      6 |   assert (Test.get_storage orig.addr = ())
                    ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_part_1.jsligo"; "--no-warn" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test1 exited with value ().
    - test2 exited with value ().
    - test3 exited with value ().
    - test4 exited with value ().
    - test5 exited with value ().
    - test6 exited with value ().
    - test7 exited with value ().
    - test8 exited with value ().
    - test9 exited with value ().
    - test10 exited with value ().
    - test11 exited with value ().
    - test12 exited with value ().
    - test13 exited with value ().
    - test14 exited with value ().
    - test15 exited with value ().
    - test16 exited with value ().
    - test17 exited with value ().
    - test18 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_part_2.jsligo"; "--no-warn" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test1 exited with value ().
      - test2 exited with value ().
      - test3 exited with value ().
      - test4 exited with value ().
      - test5 exited with value ().
      - test6 exited with value ().
      - test7 exited with value ().
      - test8 exited with value ().
      - test9 exited with value ().
      - test10 exited with value ().
      - test11 exited with value ().
      - test12 exited with value ().
      - test13 exited with value ().
      - test14 exited with value ().
      - test15 exited with value ().
      - test16 exited with value ().
      - test17 exited with value ().
      - test18 exited with value ().
      - test19 exited with value ().
      - test20 exited with value ().
      - test21 exited with value ().
      - test22 exited with value ().
      - test23 exited with value ().
      - test24 exited with value ().
      - test25 exited with value ().
      - test26 exited with value ().
      - test27 exited with value ().
      - test28 exited with value ().
      - test29 exited with value ().
      - test30 exited with value ().
      - test31 exited with value ().
      - test32 exited with value ().
      - test33 exited with value ().
      - test34 exited with value ().
      - test35 exited with value ().
      - test36 exited with value ().
      - test37 exited with value ().
      - test38 exited with value ().
      - test39 exited with value ().
      - test40 exited with value ().
      - test41 exited with value ().
      - test42 exited with value ().
      - test43 exited with value ().
      - test44 exited with value ().
      - test45 exited with value ().
      - test46 exited with value ().
      - test47 exited with value ().
      - test48 exited with value ().
      - test49 exited with value ().
      - test50 exited with value ().
      - test51 exited with value ().
      - test52 exited with value ().
      - test53 exited with value ().
      - test54 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_part_3.jsligo"; "--no-warn" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test1 exited with value ().
      - test2 exited with value ().
      - test3 exited with value ().
      - test4 exited with value ().
      - test5 exited with value ().
      - test6 exited with value ().
      - test7 exited with value ().
      - test8 exited with value ().
      - test9 exited with value ().
      - test10 exited with value ().
      - test11 exited with value ().
      - test12 exited with value ().
      - test13 exited with value ().
      - test14 exited with value ().
      - test15 exited with value ().
      - test16 exited with value ().
      - test17 exited with value ().
      - test18 exited with value ().
      - test19 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_if_else.jsligo"; "--no-warn" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test_if_switch_break exited with value ().
      - test_if_switch_return exited with value ().
      - test_switch_if_break exited with value ().
      - test_switch_if_return exited with value ().
      - test_switch_switch_break exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "let_rec.mligo" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test exited with value true. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_negative_big_map_id.mligo" ];
  [%expect
    {|
      File "./test_negative_big_map_id.mligo", line 10, characters 16-30:
        9 | let test_main =
       10 |     let orig =  Test.originate (contract_of C) () 0tez in
                            ^^^^^^^^^^^^^^
       11 |     let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

      File "./test_negative_big_map_id.mligo", line 11, characters 12-29:
       10 |     let orig =  Test.originate (contract_of C) () 0tez in
       11 |     let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                        ^^^^^^^^^^^^^^^^^
       12 |     ()
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

      Everything at the top-level was executed.
      - test_main exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_FA12.mligo" ];
  [%expect
    {|
    File "./test_FA12.mligo", line 4, characters 11-27:
      3 | let test_transfer =
      4 |   let () = Test.reset_state 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
      5 |   let sender_ = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 5, characters 16-42:
      4 |   let () = Test.reset_state 10n ([] : tez list) in
      5 |   let sender_ = Test.nth_bootstrap_account 0 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
      6 |   let from_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 6, characters 14-40:
      5 |   let sender_ = Test.nth_bootstrap_account 0 in
      6 |   let from_ = Test.nth_bootstrap_account 1 in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 7, characters 12-38:
      6 |   let from_ = Test.nth_bootstrap_account 1 in
      7 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |   let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 11, characters 48-62:
     10 |                   total_supply = 300n } in
     11 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
                                                          ^^^^^^^^^^^^^^
     12 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 12, characters 14-30:
     11 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
     12 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     13 |   let parameter = Transfer { address_from = from_; address_to = to_; value = 10n } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 14, characters 11-26:
     13 |   let parameter = Transfer { address_from = from_; address_to = to_; value = 10n } in
     14 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
     15 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 15, characters 10-39:
     14 |   let () = Test.set_source sender_ in
     15 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     16 |   let new_storage = Test.get_storage typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 16, characters 20-36:
     15 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
     16 |   let new_storage = Test.get_storage typed_addr in
                              ^^^^^^^^^^^^^^^^
     17 |   assert ((Big_map.find_opt to_ new_storage.tokens = Some 110n) &&
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 24, characters 11-27:
     23 | let test_transfer_not_e_allowance =
     24 |   let () = Test.reset_state 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
     25 |   let sender_ = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 25, characters 16-42:
     24 |   let () = Test.reset_state 10n ([] : tez list) in
     25 |   let sender_ = Test.nth_bootstrap_account 0 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
     26 |   let from_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 26, characters 14-40:
     25 |   let sender_ = Test.nth_bootstrap_account 0 in
     26 |   let from_ = Test.nth_bootstrap_account 1 in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     27 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 27, characters 12-38:
     26 |   let from_ = Test.nth_bootstrap_account 1 in
     27 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
     28 |   let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 31, characters 48-62:
     30 |                   total_supply = 300n } in
     31 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
                                                          ^^^^^^^^^^^^^^
     32 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 32, characters 14-30:
     31 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
     32 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     33 |   let parameter = Transfer { address_from = from_; address_to = to_; value = 10n } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 34, characters 11-26:
     33 |   let parameter = Transfer { address_from = from_; address_to = to_; value = 10n } in
     34 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
     35 |   match Test.transfer_to_contract contr (Main parameter) 0tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 35, characters 8-33:
     34 |   let () = Test.set_source sender_ in
     35 |   match Test.transfer_to_contract contr (Main parameter) 0tez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
     36 |   | Success _ -> failwith "Transaction should fail"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 37, characters 38-58:
     36 |   | Success _ -> failwith "Transaction should fail"
     37 |   | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "NotEnoughAllowance"))
                                                ^^^^^^^^^^^^^^^^^^^^
     38 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 37, characters 62-71:
     36 |   | Success _ -> failwith "Transaction should fail"
     37 |   | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "NotEnoughAllowance"))
                                                                        ^^^^^^^^^
     38 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 41, characters 11-27:
     40 | let test_transfer_not_e_balance =
     41 |   let () = Test.reset_state 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
     42 |   let sender_ = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 42, characters 16-42:
     41 |   let () = Test.reset_state 10n ([] : tez list) in
     42 |   let sender_ = Test.nth_bootstrap_account 0 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
     43 |   let from_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 43, characters 14-40:
     42 |   let sender_ = Test.nth_bootstrap_account 0 in
     43 |   let from_ = Test.nth_bootstrap_account 1 in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     44 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 44, characters 12-38:
     43 |   let from_ = Test.nth_bootstrap_account 1 in
     44 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
     45 |   let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 0n); (to_, 100n)];
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 48, characters 48-62:
     47 |                   total_supply = 300n } in
     48 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
                                                          ^^^^^^^^^^^^^^
     49 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 49, characters 14-30:
     48 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
     49 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     50 |   let parameter = Transfer { address_from = from_; address_to = to_; value = 10n } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 51, characters 11-26:
     50 |   let parameter = Transfer { address_from = from_; address_to = to_; value = 10n } in
     51 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
     52 |   match Test.transfer_to_contract contr (Main parameter) 0tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 52, characters 8-33:
     51 |   let () = Test.set_source sender_ in
     52 |   match Test.transfer_to_contract contr (Main parameter) 0tez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
     53 |   | Success _ -> failwith "Transaction should fail"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 54, characters 38-58:
     53 |   | Success _ -> failwith "Transaction should fail"
     54 |   | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "NotEnoughBalance"))
                                                ^^^^^^^^^^^^^^^^^^^^
     55 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 54, characters 62-71:
     53 |   | Success _ -> failwith "Transaction should fail"
     54 |   | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "NotEnoughBalance"))
                                                                        ^^^^^^^^^
     55 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 58, characters 11-27:
     57 | let test_approve =
     58 |   let () = Test.reset_state 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
     59 |   let sender_ = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 59, characters 16-42:
     58 |   let () = Test.reset_state 10n ([] : tez list) in
     59 |   let sender_ = Test.nth_bootstrap_account 0 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
     60 |   let from_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 60, characters 14-40:
     59 |   let sender_ = Test.nth_bootstrap_account 0 in
     60 |   let from_ = Test.nth_bootstrap_account 1 in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     61 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 61, characters 12-38:
     60 |   let from_ = Test.nth_bootstrap_account 1 in
     61 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
     62 |   let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 65, characters 48-62:
     64 |                   total_supply = 300n } in
     65 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
                                                          ^^^^^^^^^^^^^^
     66 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 66, characters 14-30:
     65 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
     66 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     67 |   let parameter = Approve { spender = from_; value = 100n } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 68, characters 11-26:
     67 |   let parameter = Approve { spender = from_; value = 100n } in
     68 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
     69 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 69, characters 10-39:
     68 |   let () = Test.set_source sender_ in
     69 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     70 |   let new_storage = Test.get_storage typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 70, characters 20-36:
     69 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
     70 |   let new_storage = Test.get_storage typed_addr in
                              ^^^^^^^^^^^^^^^^
     71 |   assert ((Big_map.find_opt to_ new_storage.tokens = Some 100n) &&
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 78, characters 11-27:
     77 | let test_approve_unsafe =
     78 |   let () = Test.reset_state 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
     79 |   let sender_ = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 79, characters 16-42:
     78 |   let () = Test.reset_state 10n ([] : tez list) in
     79 |   let sender_ = Test.nth_bootstrap_account 0 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
     80 |   let from_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 80, characters 14-40:
     79 |   let sender_ = Test.nth_bootstrap_account 0 in
     80 |   let from_ = Test.nth_bootstrap_account 1 in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
     81 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 81, characters 12-38:
     80 |   let from_ = Test.nth_bootstrap_account 1 in
     81 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
     82 |   let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 85, characters 48-62:
     84 |                   total_supply = 300n } in
     85 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
                                                          ^^^^^^^^^^^^^^
     86 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 86, characters 14-30:
     85 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
     86 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
     87 |   let parameter = Approve { spender = from_; value = 100n } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 88, characters 11-26:
     87 |   let parameter = Approve { spender = from_; value = 100n } in
     88 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
     89 |   match Test.transfer_to_contract contr (Main parameter) 0tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 89, characters 8-33:
     88 |   let () = Test.set_source sender_ in
     89 |   match Test.transfer_to_contract contr (Main parameter) 0tez with
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
     90 |   | Success _ -> failwith "Transaction should fail"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 91, characters 38-58:
     90 |   | Success _ -> failwith "Transaction should fail"
     91 |   | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "UnsafeAllowanceChange"))
                                                ^^^^^^^^^^^^^^^^^^^^
     92 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 91, characters 62-71:
     90 |   | Success _ -> failwith "Transaction should fail"
     91 |   | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "UnsafeAllowanceChange"))
                                                                        ^^^^^^^^^
     92 |   | Fail _ -> failwith "Transaction should fail with rejection"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 101, characters 11-27:
    100 | let test_get_allowance =
    101 |   let () = Test.reset_state 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
    102 |   let sender_ = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 102, characters 16-42:
    101 |   let () = Test.reset_state 10n ([] : tez list) in
    102 |   let sender_ = Test.nth_bootstrap_account 0 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
    103 |   let from_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 103, characters 14-40:
    102 |   let sender_ = Test.nth_bootstrap_account 0 in
    103 |   let from_ = Test.nth_bootstrap_account 1 in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
    104 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 104, characters 12-38:
    103 |   let from_ = Test.nth_bootstrap_account 1 in
    104 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
    105 |   let {addr = dummy_typed_addr; code = _; size = _} = Test.originate dummy_contract 0n 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 105, characters 54-68:
    104 |   let to_ = Test.nth_bootstrap_account 2 in
    105 |   let {addr = dummy_typed_addr; code = _; size = _} = Test.originate dummy_contract 0n 0tez in
                                                                ^^^^^^^^^^^^^^
    106 |   let dummy_typed_contr = Test.to_contract dummy_typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 106, characters 26-42:
    105 |   let {addr = dummy_typed_addr; code = _; size = _} = Test.originate dummy_contract 0n 0tez in
    106 |   let dummy_typed_contr = Test.to_contract dummy_typed_addr in
                                    ^^^^^^^^^^^^^^^^
    107 |   let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 110, characters 48-62:
    109 |                   total_supply = 300n } in
    110 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
                                                          ^^^^^^^^^^^^^^
    111 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 111, characters 14-30:
    110 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
    111 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
    112 |   let parameter = GetAllowance { request = { owner = from_; spender = sender_} ; callback = dummy_typed_contr } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 113, characters 11-26:
    112 |   let parameter = GetAllowance { request = { owner = from_; spender = sender_} ; callback = dummy_typed_contr } in
    113 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
    114 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 114, characters 10-39:
    113 |   let () = Test.set_source sender_ in
    114 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    115 |   let new_storage = Test.get_storage typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 115, characters 20-36:
    114 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
    115 |   let new_storage = Test.get_storage typed_addr in
                              ^^^^^^^^^^^^^^^^
    116 |   let _ = assert ((Big_map.find_opt to_ new_storage.tokens = Some 100n) &&
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 121, characters 26-42:
    120 |                   (new_storage.total_supply = 300n)) in
    121 |   let dummy_new_storage = Test.get_storage dummy_typed_addr in
                                    ^^^^^^^^^^^^^^^^
    122 |   assert (dummy_new_storage = 100n)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 125, characters 11-27:
    124 | let test_get_balance =
    125 |   let () = Test.reset_state 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
    126 |   let sender_ = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 126, characters 16-42:
    125 |   let () = Test.reset_state 10n ([] : tez list) in
    126 |   let sender_ = Test.nth_bootstrap_account 0 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
    127 |   let from_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 127, characters 14-40:
    126 |   let sender_ = Test.nth_bootstrap_account 0 in
    127 |   let from_ = Test.nth_bootstrap_account 1 in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
    128 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 128, characters 12-38:
    127 |   let from_ = Test.nth_bootstrap_account 1 in
    128 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
    129 |   let {addr = dummy_typed_addr; code = _; size = _} = Test.originate dummy_contract 0n 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 129, characters 54-68:
    128 |   let to_ = Test.nth_bootstrap_account 2 in
    129 |   let {addr = dummy_typed_addr; code = _; size = _} = Test.originate dummy_contract 0n 0tez in
                                                                ^^^^^^^^^^^^^^
    130 |   let dummy_typed_contr = Test.to_contract dummy_typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 130, characters 26-42:
    129 |   let {addr = dummy_typed_addr; code = _; size = _} = Test.originate dummy_contract 0n 0tez in
    130 |   let dummy_typed_contr = Test.to_contract dummy_typed_addr in
                                    ^^^^^^^^^^^^^^^^
    131 |   let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 134, characters 48-62:
    133 |                   total_supply = 300n } in
    134 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
                                                          ^^^^^^^^^^^^^^
    135 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 135, characters 14-30:
    134 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
    135 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
    136 |   let parameter = GetBalance { owner = from_ ; callback = dummy_typed_contr } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 137, characters 11-26:
    136 |   let parameter = GetBalance { owner = from_ ; callback = dummy_typed_contr } in
    137 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
    138 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 138, characters 10-39:
    137 |   let () = Test.set_source sender_ in
    138 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    139 |   let new_storage = Test.get_storage typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 139, characters 20-36:
    138 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
    139 |   let new_storage = Test.get_storage typed_addr in
                              ^^^^^^^^^^^^^^^^
    140 |   let _ = assert ((Big_map.find_opt to_ new_storage.tokens = Some 100n) &&
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 145, characters 26-42:
    144 |                   (new_storage.total_supply = 300n)) in
    145 |   let dummy_new_storage = Test.get_storage dummy_typed_addr in
                                    ^^^^^^^^^^^^^^^^
    146 |   assert (dummy_new_storage = 100n)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 149, characters 11-27:
    148 | let test_get_total_supply =
    149 |   let () = Test.reset_state 10n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
    150 |   let sender_ = Test.nth_bootstrap_account 0 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 150, characters 16-42:
    149 |   let () = Test.reset_state 10n ([] : tez list) in
    150 |   let sender_ = Test.nth_bootstrap_account 0 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
    151 |   let from_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 151, characters 14-40:
    150 |   let sender_ = Test.nth_bootstrap_account 0 in
    151 |   let from_ = Test.nth_bootstrap_account 1 in
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
    152 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 152, characters 12-38:
    151 |   let from_ = Test.nth_bootstrap_account 1 in
    152 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
    153 |   let {addr = dummy_typed_addr; code = _; size = _} = Test.originate dummy_contract 0n 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 153, characters 54-68:
    152 |   let to_ = Test.nth_bootstrap_account 2 in
    153 |   let {addr = dummy_typed_addr; code = _; size = _} = Test.originate dummy_contract 0n 0tez in
                                                                ^^^^^^^^^^^^^^
    154 |   let dummy_typed_contr = Test.to_contract dummy_typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 154, characters 26-42:
    153 |   let {addr = dummy_typed_addr; code = _; size = _} = Test.originate dummy_contract 0n 0tez in
    154 |   let dummy_typed_contr = Test.to_contract dummy_typed_addr in
                                    ^^^^^^^^^^^^^^^^
    155 |   let storage = { tokens = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 158, characters 48-62:
    157 |                   total_supply = 300n } in
    158 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
                                                          ^^^^^^^^^^^^^^
    159 |   let contr = Test.to_contract typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 159, characters 14-30:
    158 |   let {addr = typed_addr; code = _; size = _} = Test.originate (contract_of C) storage 0tez in
    159 |   let contr = Test.to_contract typed_addr in
                        ^^^^^^^^^^^^^^^^
    160 |   let parameter = GetTotalSupply { callback = dummy_typed_contr; request = () } in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 161, characters 11-26:
    160 |   let parameter = GetTotalSupply { callback = dummy_typed_contr; request = () } in
    161 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
    162 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 162, characters 10-39:
    161 |   let () = Test.set_source sender_ in
    162 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    163 |   let new_storage = Test.get_storage typed_addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 163, characters 20-36:
    162 |   let _ = Test.transfer_to_contract_exn contr (Main parameter) 0tez in
    163 |   let new_storage = Test.get_storage typed_addr in
                              ^^^^^^^^^^^^^^^^
    164 |   let _ = assert ((Big_map.find_opt to_ new_storage.tokens = Some 100n) &&
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_FA12.mligo", line 169, characters 26-42:
    168 |                   (new_storage.total_supply = 300n)) in
    169 |   let dummy_new_storage = Test.get_storage dummy_typed_addr in
                                    ^^^^^^^^^^^^^^^^
    170 |   assert (dummy_new_storage = 300n)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_transfer exited with value ().
    - test_transfer_not_e_allowance exited with value ().
    - test_transfer_not_e_balance exited with value ().
    - test_approve exited with value ().
    - test_approve_unsafe exited with value ().
    - test_get_allowance exited with value ().
    - test_get_balance exited with value ().
    - test_get_total_supply exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "pack_unpack.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_string exited with value ().
    - test_int exited with value ().
    - test_string_int exited with value ().
    - test_string_string exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_pack_unpack.mligo" ];
  [%expect
    {|
    File "./test_pack_unpack.mligo", line 9, characters 13-27:
      8 |   let b = Bytes.pack 42n in
      9 |   let orig = Test.originate (contract_of C) b 0tez in
                       ^^^^^^^^^^^^^^
     10 |   let () = assert ((Bytes.unpack (Test.get_storage orig.addr) : nat option) = Some 42n) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_pack_unpack.mligo", line 10, characters 34-50:
      9 |   let orig = Test.originate (contract_of C) b 0tez in
     10 |   let () = assert ((Bytes.unpack (Test.get_storage orig.addr) : nat option) = Some 42n) in
                                            ^^^^^^^^^^^^^^^^
     11 |   let b = Bytes.pack "bonjour" in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_pack_unpack.mligo", line 12, characters 10-27:
     11 |   let b = Bytes.pack "bonjour" in
     12 |   let _ = Test.transfer_exn orig.addr (Main b) 0tez in
                    ^^^^^^^^^^^^^^^^^
     13 |   assert ((Bytes.unpack (Test.get_storage orig.addr) : string option) = Some "bonjour")
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_pack_unpack.mligo", line 13, characters 25-41:
     12 |   let _ = Test.transfer_exn orig.addr (Main b) 0tez in
     13 |   assert ((Bytes.unpack (Test.get_storage orig.addr) : string option) = Some "bonjour")
                                   ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "pairing_check.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "gas_consum.mligo" ];
  [%expect
    {|
    File "./gas_consum.mligo", line 17, characters 14-28:
     16 |   in
     17 |   let orig =  Test.originate (contract_of C) big_list 0tez in
                        ^^^^^^^^^^^^^^
     18 |   let _caching =
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./gas_consum.mligo", line 20, characters 4-17:
     19 |     (* some caching is happening on the first transaction *)
     20 |     Test.transfer orig.addr (Main true) 0tez
              ^^^^^^^^^^^^^
     21 |   in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./gas_consum.mligo", line 22, characters 12-25:
     21 |   in
     22 |   let tx1 = Test.transfer orig.addr (Main false) 0tez in
                      ^^^^^^^^^^^^^
     23 |   let tx2 = Test.transfer orig.addr (Main true) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./gas_consum.mligo", line 23, characters 12-25:
     22 |   let tx1 = Test.transfer orig.addr (Main false) 0tez in
     23 |   let tx2 = Test.transfer orig.addr (Main true) 0tez in
                      ^^^^^^^^^^^^^
     24 |   let tx3 = Test.transfer orig.addr (Main true) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./gas_consum.mligo", line 24, characters 12-25:
     23 |   let tx2 = Test.transfer orig.addr (Main true) 0tez in
     24 |   let tx3 = Test.transfer orig.addr (Main true) 0tez in
                      ^^^^^^^^^^^^^
     25 |   match (tx1 , tx2, tx3) with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (1305n , 1510n , 1510n). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_implicit_account.jsligo" ];
  [%expect
    {|
    File "./test_implicit_account.jsligo", line 5, characters 4-12:
      4 |     let a : address = Tezos.address(c);
      5 |     Test.log(Test.get_balance_of_address(a));
              ^^^^^^^^
      6 |     Test.transfer_to_contract_exn(c, unit, (123 as mutez));
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_implicit_account.jsligo", line 5, characters 13-40:
      4 |     let a : address = Tezos.address(c);
      5 |     Test.log(Test.get_balance_of_address(a));
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      6 |     Test.transfer_to_contract_exn(c, unit, (123 as mutez));
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_implicit_account.jsligo", line 6, characters 4-33:
      5 |     Test.log(Test.get_balance_of_address(a));
      6 |     Test.transfer_to_contract_exn(c, unit, (123 as mutez));
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |     Test.log(Test.get_balance_of_address(a));
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_implicit_account.jsligo", line 7, characters 4-12:
      6 |     Test.transfer_to_contract_exn(c, unit, (123 as mutez));
      7 |     Test.log(Test.get_balance_of_address(a));
              ^^^^^^^^
      8 |     return list([Tezos.address(c) , ...accList]);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_implicit_account.jsligo", line 7, characters 13-40:
      6 |     Test.transfer_to_contract_exn(c, unit, (123 as mutez));
      7 |     Test.log(Test.get_balance_of_address(a));
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |     return list([Tezos.address(c) , ...accList]);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    0mutez
    123mutez
    Everything at the top-level was executed.
    - test_addresses exited with value [tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx]. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_accounts.mligo" ];
  [%expect
    {|
    File "./test_accounts.mligo", line 1, characters 49-66:
      1 | let cast_implicit_account c : implicit_address = Test.cast_address c
                                                           ^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.to_typed_address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 4, characters 18-34:
      3 | let test_new =
      4 |   let (_sk, pk) = Test.new_account () in
                            ^^^^^^^^^^^^^^^^
      5 |   let pkh = Crypto.hash_key pk in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 8, characters 12-38:
      7 |   let a = Tezos.address c in
      8 |   let to_ = Test.nth_bootstrap_account 0 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |   let _ = Test.transfer_to_contract_exn c () 123tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 9, characters 10-39:
      8 |   let to_ = Test.nth_bootstrap_account 0 in
      9 |   let _ = Test.transfer_to_contract_exn c () 123tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |   let _ = Test.set_source a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 10, characters 10-25:
      9 |   let _ = Test.transfer_to_contract_exn c () 123tez in
     10 |   let _ = Test.set_source a in
                    ^^^^^^^^^^^^^^^
     11 |   let _ = Test.transfer_exn (cast_implicit_account to_) () 12tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 11, characters 10-27:
     10 |   let _ = Test.set_source a in
     11 |   let _ = Test.transfer_exn (cast_implicit_account to_) () 12tez in
                    ^^^^^^^^^^^^^^^^^
     12 |   Test.get_balance_of_address a
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 12, characters 2-29:
     11 |   let _ = Test.transfer_exn (cast_implicit_account to_) () 12tez in
     12 |   Test.get_balance_of_address a
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 15, characters 11-27:
     14 | let test_add =
     15 |   let () = Test.reset_state 2n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
     16 |   let sk = "edsk3FhQ1djEDDCfqseyfbrpwkw5ogTDAaryXAdQGhk5Vpw6VGgo6v" in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 18, characters 11-27:
     17 |   let pk = ("edpkv2kByfiJmUHr3SCp2rpASF2xSEhT248MSNEAZK9ho86sMBdcuE" : key) in
     18 |   let () = Test.add_account sk pk in
                     ^^^^^^^^^^^^^^^^
     19 |   let pkh = Crypto.hash_key pk in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.add` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 22, characters 12-38:
     21 |   let a = Tezos.address c in
     22 |   let to_ = Test.nth_bootstrap_account 0 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
     23 |   let _ = Test.transfer_to_contract_exn c () 123tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 23, characters 10-39:
     22 |   let to_ = Test.nth_bootstrap_account 0 in
     23 |   let _ = Test.transfer_to_contract_exn c () 123tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     24 |   let _ = Test.set_source a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 24, characters 10-25:
     23 |   let _ = Test.transfer_to_contract_exn c () 123tez in
     24 |   let _ = Test.set_source a in
                    ^^^^^^^^^^^^^^^
     25 |   let _ = Test.transfer_exn (cast_implicit_account to_) () 12tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 25, characters 10-27:
     24 |   let _ = Test.set_source a in
     25 |   let _ = Test.transfer_exn (cast_implicit_account to_) () 12tez in
                    ^^^^^^^^^^^^^^^^^
     26 |   Test.get_balance_of_address a
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_accounts.mligo", line 26, characters 2-29:
     25 |   let _ = Test.transfer_exn (cast_implicit_account to_) () 12tez in
     26 |   Test.get_balance_of_address a
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_new exited with value 110000000mutez.
    - test_add exited with value 110000000mutez. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_baker_account.mligo" ];
  [%expect
    {|
    File "./test_baker_account.mligo", line 7, characters 12-28:
      6 | let test =
      7 |   let acc = Test.new_account () in
                      ^^^^^^^^^^^^^^^^
      8 |   let () = Test.baker_account acc (None : tez option) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 8, characters 11-29:
      7 |   let acc = Test.new_account () in
      8 |   let () = Test.baker_account acc (None : tez option) in
                     ^^^^^^^^^^^^^^^^^^
      9 |   let () = Test.reset_state 2n ([] : tez list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.Reset.add_baker` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 9, characters 11-27:
      8 |   let () = Test.baker_account acc (None : tez option) in
      9 |   let () = Test.reset_state 2n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
     10 |   let pkh = Crypto.hash_key acc.1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 13, characters 11-19:
     12 |   let a = Tezos.address c in
     13 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
                     ^^^^^^^^
     14 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 14, characters 11-19:
     13 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
     14 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     15 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 14, characters 20-47:
     13 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
     14 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     15 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 15, characters 11-19:
     14 |   let () = Test.log(Test.get_balance_of_address a) in
     15 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     16 |   let () = Test.set_baker a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 15, characters 20-41:
     14 |   let () = Test.log(Test.get_balance_of_address a) in
     15 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     16 |   let () = Test.set_baker a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 16, characters 11-25:
     15 |   let () = Test.log(Test.get_voting_power pkh) in
     16 |   let () = Test.set_baker a in
                     ^^^^^^^^^^^^^^
     17 |   let orig = Test.originate (contract_of C) 41 5tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 17, characters 13-27:
     16 |   let () = Test.set_baker a in
     17 |   let orig = Test.originate (contract_of C) 41 5tez in
                       ^^^^^^^^^^^^^^
     18 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 18, characters 11-19:
     17 |   let orig = Test.originate (contract_of C) 41 5tez in
     18 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
                     ^^^^^^^^
     19 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 19, characters 11-19:
     18 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
     19 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     20 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 19, characters 20-47:
     18 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
     19 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     20 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 20, characters 11-19:
     19 |   let () = Test.log(Test.get_balance_of_address a) in
     20 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     21 |   let cc = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 20, characters 20-41:
     19 |   let () = Test.log(Test.get_balance_of_address a) in
     20 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     21 |   let cc = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 21, characters 11-27:
     20 |   let () = Test.log(Test.get_voting_power pkh) in
     21 |   let cc = Test.to_contract orig.addr in
                     ^^^^^^^^^^^^^^^^
     22 |   let _ = Test.transfer_to_contract cc (Main 1) 3tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 22, characters 10-35:
     21 |   let cc = Test.to_contract orig.addr in
     22 |   let _ = Test.transfer_to_contract cc (Main 1) 3tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
     23 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 23, characters 11-19:
     22 |   let _ = Test.transfer_to_contract cc (Main 1) 3tez in
     23 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
                     ^^^^^^^^
     24 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 24, characters 11-19:
     23 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
     24 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     25 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 24, characters 20-47:
     23 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
     24 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     25 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 25, characters 11-19:
     24 |   let () = Test.log(Test.get_balance_of_address a) in
     25 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     26 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 25, characters 20-41:
     24 |   let () = Test.log(Test.get_balance_of_address a) in
     25 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     26 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    "STARTING BALANCE AND VOTING POWER"
    3800000000000mutez
    4000000000000n
    "BALANCE AND VOTING POWER AFTER ORIGINATE"
    3800005749999mutez
    4000000000000n
    "BALANCE AND VOTING POWER AFTER TRANSFER"
    3800011499998mutez
    4000000000000n
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_register_delegate.mligo" ];
  [%expect
    {|
    File "./test_register_delegate.mligo", line 7, characters 12-28:
      6 |
      7 |   let acc = Test.new_account () in
                      ^^^^^^^^^^^^^^^^
      8 |   let pkh = Crypto.hash_key acc.1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 12, characters 10-39:
     11 |
     12 |   let _ = Test.transfer_to_contract_exn c () 1000000tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |   let () = Test.register_delegate pkh in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 13, characters 11-33:
     12 |   let _ = Test.transfer_to_contract_exn c () 1000000tez in
     13 |   let () = Test.register_delegate pkh in
                     ^^^^^^^^^^^^^^^^^^^^^^
     14 |   (*
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_delegate` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 19, characters 11-38:
     18 |   *)
     19 |   let () = Test.bake_until_n_cycle_end 8n in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     20 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.bake_until` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 21, characters 11-19:
     20 |
     21 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
                     ^^^^^^^^
     22 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 22, characters 11-19:
     21 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
     22 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     23 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 22, characters 20-47:
     21 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
     22 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     23 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 23, characters 11-19:
     22 |   let () = Test.log(Test.get_balance_of_address a) in
     23 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     24 |   let () = Test.set_baker a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 23, characters 20-41:
     22 |   let () = Test.log(Test.get_balance_of_address a) in
     23 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     24 |   let () = Test.set_baker a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 24, characters 11-25:
     23 |   let () = Test.log(Test.get_voting_power pkh) in
     24 |   let () = Test.set_baker a in
                     ^^^^^^^^^^^^^^
     25 |   let orig = Test.originate (contract_of C) 41 5tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 25, characters 13-27:
     24 |   let () = Test.set_baker a in
     25 |   let orig = Test.originate (contract_of C) 41 5tez in
                       ^^^^^^^^^^^^^^
     26 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 27, characters 11-19:
     26 |
     27 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
                     ^^^^^^^^
     28 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 28, characters 11-19:
     27 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
     28 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     29 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 28, characters 20-47:
     27 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
     28 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     29 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 29, characters 11-19:
     28 |   let () = Test.log(Test.get_balance_of_address a) in
     29 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     30 |   let _ = Test.transfer orig.addr (Main 1) 3tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 29, characters 20-41:
     28 |   let () = Test.log(Test.get_balance_of_address a) in
     29 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     30 |   let _ = Test.transfer orig.addr (Main 1) 3tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 30, characters 10-23:
     29 |   let () = Test.log(Test.get_voting_power pkh) in
     30 |   let _ = Test.transfer orig.addr (Main 1) 3tez in
                    ^^^^^^^^^^^^^
     31 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 32, characters 11-19:
     31 |
     32 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
                     ^^^^^^^^
     33 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 33, characters 11-19:
     32 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
     33 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     34 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 33, characters 20-47:
     32 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
     33 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     34 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 34, characters 11-19:
     33 |   let () = Test.log(Test.get_balance_of_address a) in
     34 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     35 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_register_delegate.mligo", line 34, characters 20-41:
     33 |   let () = Test.log(Test.get_balance_of_address a) in
     34 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     35 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    "STARTING BALANCE AND VOTING POWER"
    950000000000mutez
    0n
    "BALANCE AND VOTING POWER AFTER ORIGINATE"
    950005749999mutez
    0n
    "BALANCE AND VOTING POWER AFTER TRANSFER"
    950011499998mutez
    0n
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_global_constant.mligo" ];
  [%expect
    {|
    File "./test_global_constant.mligo", line 8, characters 20-42:
      7 |
      8 |   let ct : string = Test.register_constant (Test.eval f)
                              ^^^^^^^^^^^^^^^^^^^^^^
      9 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_constant` from `Test.Next` is encouraged for a smoother migration.

    File "./test_global_constant.mligo", line 8, characters 44-53:
      7 |
      8 |   let ct : string = Test.register_constant (Test.eval f)
                                                      ^^^^^^^^^
      9 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_global_constant.mligo", line 16, characters 13-27:
     15 | let test =
     16 |   let orig = Test.originate (contract_of C) 1 0tez in
                       ^^^^^^^^^^^^^^
     17 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_global_constant.mligo", line 17, characters 10-27:
     16 |   let orig = Test.originate (contract_of C) 1 0tez in
     17 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
     18 |   assert (Test.get_storage orig.addr = 5)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_global_constant.mligo", line 18, characters 10-26:
     17 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
     18 |   assert (Test.get_storage orig.addr = 5)
                    ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_global_constant_2.mligo" ];
  [%expect
    {|
    File "./test_global_constant_2.mligo", line 6, characters 31-65:
      5 |
      6 |   let ct : michelson_program = Test.constant_to_michelson_program "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |   let ct : string = Test.register_constant ct
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.parse` from `Test.Next` is encouraged for a smoother migration.

    File "./test_global_constant_2.mligo", line 7, characters 20-42:
      6 |   let ct : michelson_program = Test.constant_to_michelson_program "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
      7 |   let ct : string = Test.register_constant ct
                              ^^^^^^^^^^^^^^^^^^^^^^
      8 |   [@entry]
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_constant` from `Test.Next` is encouraged for a smoother migration.

    File "./test_global_constant_2.mligo", line 14, characters 13-27:
     13 | let test =
     14 |   let orig = Test.originate (contract_of C) 1 0tez in
                       ^^^^^^^^^^^^^^
     15 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_global_constant_2.mligo", line 15, characters 10-27:
     14 |   let orig = Test.originate (contract_of C) 1 0tez in
     15 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
     16 |   assert (Test.get_storage orig.addr = 5)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_global_constant_2.mligo", line 16, characters 10-26:
     15 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
     16 |   assert (Test.get_storage orig.addr = 5)
                    ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "recursion_uncurry.mligo" ];
  [%expect
    {|
    File "./recursion_uncurry.mligo", line 9, characters 13-27:
      8 | let test =
      9 |   let orig = Test.originate (contract_of C) "" 1tez in
                       ^^^^^^^^^^^^^^
     10 |   orig.size
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value 112. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_timestamp.mligo" ];
  [%expect
    {|
    File "./test_timestamp.mligo", line 4, characters 33-47:
      3 |  let the_time : timestamp = ("1970-01-04t00:00:00Z" : timestamp) in
      4 |  let new_time_mich : timestamp = Test.decompile (Test.run (fun () -> the_time - subtractthis) ()) in
                                           ^^^^^^^^^^^^^^
      5 |  let new_time : timestamp = the_time - subtractthis in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_timestamp.mligo", line 4, characters 49-57:
      3 |  let the_time : timestamp = ("1970-01-04t00:00:00Z" : timestamp) in
      4 |  let new_time_mich : timestamp = Test.decompile (Test.run (fun () -> the_time - subtractthis) ()) in
                                                           ^^^^^^^^
      5 |  let new_time : timestamp = the_time - subtractthis in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_sub exited with value ().
    - test_get_time exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_decompile_failure.mligo" ];
  [%expect
    {|
    File "./test_decompile_failure.mligo", line 8, characters 13-27:
      7 | let test =
      8 |   let orig = Test.originate (contract_of C) () 0tez in
                       ^^^^^^^^^^^^^^
      9 |   match Test.transfer orig.addr (Main ()) 0tez with
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_decompile_failure.mligo", line 9, characters 8-21:
      8 |   let orig = Test.originate (contract_of C) () 0tez in
      9 |   match Test.transfer orig.addr (Main ()) 0tez with
                  ^^^^^^^^^^^^^
     10 |   | Success _ -> Test.log "OK"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_decompile_failure.mligo", line 10, characters 17-25:
      9 |   match Test.transfer orig.addr (Main ()) 0tez with
     10 |   | Success _ -> Test.log "OK"
                           ^^^^^^^^
     11 |   | Fail (Rejected (actual, _)) ->
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_decompile_failure.mligo", line 12, characters 36-50:
     11 |   | Fail (Rejected (actual, _)) ->
     12 |     let ec : nat * (string * int) = Test.decompile actual in
                                              ^^^^^^^^^^^^^^
     13 |     Test.log ec
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_decompile_failure.mligo", line 13, characters 4-12:
     12 |     let ec : nat * (string * int) = Test.decompile actual in
     13 |     Test.log ec
              ^^^^^^^^
     14 |   | Fail _ -> Test.log "KO"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_decompile_failure.mligo", line 14, characters 14-22:
     13 |     Test.log ec
     14 |   | Fail _ -> Test.log "KO"
                        ^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    (4n , ("a" , 5))
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_context.mligo" ];
  [%expect
    {|
    File "./test_context.mligo", line 8, characters 11-19:
      7 | let test_contract =
      8 |   let () = Test.log "test_contract:" in
                     ^^^^^^^^
      9 |   let orig = Test.originate (contract_of C) 0 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 9, characters 13-27:
      8 |   let () = Test.log "test_contract:" in
      9 |   let orig = Test.originate (contract_of C) 0 0tez in
                       ^^^^^^^^^^^^^^
     10 |   let () = Test.log (Test.get_storage orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 10, characters 11-19:
      9 |   let orig = Test.originate (contract_of C) 0 0tez in
     10 |   let () = Test.log (Test.get_storage orig.addr) in
                     ^^^^^^^^
     11 |   let () = Test.save_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 10, characters 21-37:
      9 |   let orig = Test.originate (contract_of C) 0 0tez in
     10 |   let () = Test.log (Test.get_storage orig.addr) in
                               ^^^^^^^^^^^^^^^^
     11 |   let () = Test.save_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 11, characters 11-28:
     10 |   let () = Test.log (Test.get_storage orig.addr) in
     11 |   let () = Test.save_context () in
                     ^^^^^^^^^^^^^^^^^
     12 |   let _ = Test.transfer_exn orig.addr (Main 5) 123tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.save` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 12, characters 10-27:
     11 |   let () = Test.save_context () in
     12 |   let _ = Test.transfer_exn orig.addr (Main 5) 123tez in
                    ^^^^^^^^^^^^^^^^^
     13 |   let () = Test.save_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 13, characters 11-28:
     12 |   let _ = Test.transfer_exn orig.addr (Main 5) 123tez in
     13 |   let () = Test.save_context () in
                     ^^^^^^^^^^^^^^^^^
     14 |   let _ = Test.transfer_exn orig.addr (Main 5) 123tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.save` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 14, characters 10-27:
     13 |   let () = Test.save_context () in
     14 |   let _ = Test.transfer_exn orig.addr (Main 5) 123tez in
                    ^^^^^^^^^^^^^^^^^
     15 |   let () = Test.log (Test.get_storage orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 15, characters 11-19:
     14 |   let _ = Test.transfer_exn orig.addr (Main 5) 123tez in
     15 |   let () = Test.log (Test.get_storage orig.addr) in
                     ^^^^^^^^
     16 |   let () = Test.restore_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 15, characters 21-37:
     14 |   let _ = Test.transfer_exn orig.addr (Main 5) 123tez in
     15 |   let () = Test.log (Test.get_storage orig.addr) in
                               ^^^^^^^^^^^^^^^^
     16 |   let () = Test.restore_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 16, characters 11-31:
     15 |   let () = Test.log (Test.get_storage orig.addr) in
     16 |   let () = Test.restore_context () in
                     ^^^^^^^^^^^^^^^^^^^^
     17 |   let () = Test.log (Test.get_storage orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.restore` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 17, characters 11-19:
     16 |   let () = Test.restore_context () in
     17 |   let () = Test.log (Test.get_storage orig.addr) in
                     ^^^^^^^^
     18 |   let () = Test.restore_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 17, characters 21-37:
     16 |   let () = Test.restore_context () in
     17 |   let () = Test.log (Test.get_storage orig.addr) in
                               ^^^^^^^^^^^^^^^^
     18 |   let () = Test.restore_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 18, characters 11-31:
     17 |   let () = Test.log (Test.get_storage orig.addr) in
     18 |   let () = Test.restore_context () in
                     ^^^^^^^^^^^^^^^^^^^^
     19 |   let () = Test.log (Test.get_storage orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.restore` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 19, characters 11-19:
     18 |   let () = Test.restore_context () in
     19 |   let () = Test.log (Test.get_storage orig.addr) in
                     ^^^^^^^^
     20 |   let () = Test.restore_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 19, characters 21-37:
     18 |   let () = Test.restore_context () in
     19 |   let () = Test.log (Test.get_storage orig.addr) in
                               ^^^^^^^^^^^^^^^^
     20 |   let () = Test.restore_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 20, characters 11-31:
     19 |   let () = Test.log (Test.get_storage orig.addr) in
     20 |   let () = Test.restore_context () in
                     ^^^^^^^^^^^^^^^^^^^^
     21 |   let () = Test.log (Test.get_storage orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.restore` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 21, characters 11-19:
     20 |   let () = Test.restore_context () in
     21 |   let () = Test.log (Test.get_storage orig.addr) in
                     ^^^^^^^^
     22 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 21, characters 21-37:
     20 |   let () = Test.restore_context () in
     21 |   let () = Test.log (Test.get_storage orig.addr) in
                               ^^^^^^^^^^^^^^^^
     22 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 25, characters 11-19:
     24 | let test_move =
     25 |   let () = Test.log "test_move:" in
                     ^^^^^^^^
     26 |   let _ = Test.reset_state 4n ([] : tez list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 26, characters 10-26:
     25 |   let () = Test.log "test_move:" in
     26 |   let _ = Test.reset_state 4n ([] : tez list) in
                    ^^^^^^^^^^^^^^^^
     27 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 27, characters 12-38:
     26 |   let _ = Test.reset_state 4n ([] : tez list) in
     27 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
     28 |   let () = Test.log (Test.get_balance_of_address to_) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 28, characters 11-19:
     27 |   let to_ = Test.nth_bootstrap_account 2 in
     28 |   let () = Test.log (Test.get_balance_of_address to_) in
                     ^^^^^^^^
     29 |   let () = Test.save_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 28, characters 21-48:
     27 |   let to_ = Test.nth_bootstrap_account 2 in
     28 |   let () = Test.log (Test.get_balance_of_address to_) in
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     29 |   let () = Test.save_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 29, characters 11-28:
     28 |   let () = Test.log (Test.get_balance_of_address to_) in
     29 |   let () = Test.save_context () in
                     ^^^^^^^^^^^^^^^^^
     30 |   let _ = Test.transfer_exn (Test.cast_address to_: implicit_address) () 100tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.save` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 30, characters 10-27:
     29 |   let () = Test.save_context () in
     30 |   let _ = Test.transfer_exn (Test.cast_address to_: implicit_address) () 100tez in
                    ^^^^^^^^^^^^^^^^^
     31 |   let () = Test.log (Test.get_balance_of_address to_) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 30, characters 29-46:
     29 |   let () = Test.save_context () in
     30 |   let _ = Test.transfer_exn (Test.cast_address to_: implicit_address) () 100tez in
                                       ^^^^^^^^^^^^^^^^^
     31 |   let () = Test.log (Test.get_balance_of_address to_) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.to_typed_address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 31, characters 11-19:
     30 |   let _ = Test.transfer_exn (Test.cast_address to_: implicit_address) () 100tez in
     31 |   let () = Test.log (Test.get_balance_of_address to_) in
                     ^^^^^^^^
     32 |   let () = Test.restore_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 31, characters 21-48:
     30 |   let _ = Test.transfer_exn (Test.cast_address to_: implicit_address) () 100tez in
     31 |   let () = Test.log (Test.get_balance_of_address to_) in
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     32 |   let () = Test.restore_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 32, characters 11-31:
     31 |   let () = Test.log (Test.get_balance_of_address to_) in
     32 |   let () = Test.restore_context () in
                     ^^^^^^^^^^^^^^^^^^^^
     33 |   let () = Test.log (Test.get_balance_of_address to_) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.restore` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 33, characters 11-19:
     32 |   let () = Test.restore_context () in
     33 |   let () = Test.log (Test.get_balance_of_address to_) in
                     ^^^^^^^^
     34 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 33, characters 21-48:
     32 |   let () = Test.restore_context () in
     33 |   let () = Test.log (Test.get_balance_of_address to_) in
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     34 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 37, characters 11-19:
     36 | let test_drop =
     37 |   let () = Test.log "test_drop:" in
                     ^^^^^^^^
     38 |   let _ = Test.reset_state 4n ([] : tez list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 38, characters 10-26:
     37 |   let () = Test.log "test_drop:" in
     38 |   let _ = Test.reset_state 4n ([] : tez list) in
                    ^^^^^^^^^^^^^^^^
     39 |   let to_ = Test.nth_bootstrap_account 2 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 39, characters 12-38:
     38 |   let _ = Test.reset_state 4n ([] : tez list) in
     39 |   let to_ = Test.nth_bootstrap_account 2 in
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
     40 |   let () = Test.log (Test.get_balance_of_address to_) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 40, characters 11-19:
     39 |   let to_ = Test.nth_bootstrap_account 2 in
     40 |   let () = Test.log (Test.get_balance_of_address to_) in
                     ^^^^^^^^
     41 |   let () = Test.save_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 40, characters 21-48:
     39 |   let to_ = Test.nth_bootstrap_account 2 in
     40 |   let () = Test.log (Test.get_balance_of_address to_) in
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     41 |   let () = Test.save_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 41, characters 11-28:
     40 |   let () = Test.log (Test.get_balance_of_address to_) in
     41 |   let () = Test.save_context () in
                     ^^^^^^^^^^^^^^^^^
     42 |   let _ = Test.transfer_exn (Test.cast_address to_ : implicit_address) () 100tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.save` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 42, characters 10-27:
     41 |   let () = Test.save_context () in
     42 |   let _ = Test.transfer_exn (Test.cast_address to_ : implicit_address) () 100tez in
                    ^^^^^^^^^^^^^^^^^
     43 |   let () = Test.log (Test.get_balance_of_address to_) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 42, characters 29-46:
     41 |   let () = Test.save_context () in
     42 |   let _ = Test.transfer_exn (Test.cast_address to_ : implicit_address) () 100tez in
                                       ^^^^^^^^^^^^^^^^^
     43 |   let () = Test.log (Test.get_balance_of_address to_) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.to_typed_address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 43, characters 11-19:
     42 |   let _ = Test.transfer_exn (Test.cast_address to_ : implicit_address) () 100tez in
     43 |   let () = Test.log (Test.get_balance_of_address to_) in
                     ^^^^^^^^
     44 |   let () = Test.drop_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 43, characters 21-48:
     42 |   let _ = Test.transfer_exn (Test.cast_address to_ : implicit_address) () 100tez in
     43 |   let () = Test.log (Test.get_balance_of_address to_) in
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     44 |   let () = Test.drop_context () in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 44, characters 11-28:
     43 |   let () = Test.log (Test.get_balance_of_address to_) in
     44 |   let () = Test.drop_context () in
                     ^^^^^^^^^^^^^^^^^
     45 |   let () = Test.log (Test.get_balance_of_address to_) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.drop` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 45, characters 11-19:
     44 |   let () = Test.drop_context () in
     45 |   let () = Test.log (Test.get_balance_of_address to_) in
                     ^^^^^^^^
     46 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_context.mligo", line 45, characters 21-48:
     44 |   let () = Test.drop_context () in
     45 |   let () = Test.log (Test.get_balance_of_address to_) in
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     46 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    "test_contract:"
    0
    10
    5
    0
    0
    "test_move:"
    3800000000000mutez
    3800100000000mutez
    3800000000000mutez
    "test_drop:"
    3800000000000mutez
    3800100000000mutez
    3800100000000mutez
    Everything at the top-level was executed.
    - test_contract exited with value ().
    - test_move exited with value ().
    - test_drop exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_error_balance.jsligo"; "--no-warn" ];
  [%expect
    {|
    100000000000000mutez
    3799997904750mutez
    Everything at the top-level was executed.
    - test exited with value {contract_balance = 3799997904750mutez ; contract_too_low = tz1hkMbkLPkvhxyqsQoBoLPqb1mruSzZx3zy ; spend_request = 100000000000000mutez}. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_inline.mligo" ];
  [%expect
    {|
    File "./test_inline.mligo", line 32, characters 13-27:
     31 |
     32 | let test_x = Test.originate (contract_of C) C.init_storage 0mutez
                       ^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_x exited with value {addr = KT1RCTMT7fm32ZVaTT5pqtNnPsRvbMxkpVMd ; code = { parameter unit ;
      storage
        (pair (set %participants address)
              (map %secrets address bool)
              (big_map %metadata string bytes)) ;
      code { CDR ;
             PUSH bool True ;
             DUP 2 ;
             CAR ;
             ITER { SWAP ;
                    DUP 3 ;
                    GET 3 ;
                    DIG 2 ;
                    GET ;
                    IF_NONE { PUSH bool False ; AND } { DROP ; PUSH bool True ; AND } } ;
             DROP ;
             PUSH bool True ;
             DUP 2 ;
             CAR ;
             ITER { SWAP ;
                    EMPTY_MAP address bool ;
                    DIG 2 ;
                    GET ;
                    IF_NONE { PUSH bool False ; AND } { DROP ; PUSH bool True ; AND } } ;
             DROP ;
             NIL operation ;
             PAIR } } ; size = 226}. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_read_contract.mligo" ];
  [%expect
    {|
    File "./test_read_contract.mligo", line 3, characters 4-32:
      2 |   let c : (unit, unit) michelson_contract =
      3 |     Test.read_contract_from_file "contract_under_test/compiled.tz" in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |   let s = Test.decompile (Test.parse_michelson "Unit") in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 4, characters 10-24:
      3 |     Test.read_contract_from_file "contract_under_test/compiled.tz" in
      4 |   let s = Test.decompile (Test.parse_michelson "Unit") in
                    ^^^^^^^^^^^^^^
      5 |   let a = Test.originate_contract c s 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 4, characters 26-46:
      3 |     Test.read_contract_from_file "contract_under_test/compiled.tz" in
      4 |   let s = Test.decompile (Test.parse_michelson "Unit") in
                                    ^^^^^^^^^^^^^^^^^^^^
      5 |   let a = Test.originate_contract c s 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.parse` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 5, characters 10-33:
      4 |   let s = Test.decompile (Test.parse_michelson "Unit") in
      5 |   let a = Test.originate_contract c s 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^
      6 |   Test.log a
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.michelson` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 6, characters 2-10:
      5 |   let a = Test.originate_contract c s 0tez in
      6 |   Test.log a
            ^^^^^^^^
      7 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 9, characters 56-84:
      8 | let test_bar =
      9 |   let c : (unit, (int,string) map) michelson_contract = Test.read_contract_from_file "contract_under_test/other_compiled.tz" in
                                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |   let s = Test.decompile (Test.constant_to_michelson_program "{ Elt 1 \\"hi\\" }") in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 10, characters 10-24:
      9 |   let c : (unit, (int,string) map) michelson_contract = Test.read_contract_from_file "contract_under_test/other_compiled.tz" in
     10 |   let s = Test.decompile (Test.constant_to_michelson_program "{ Elt 1 \\"hi\\" }") in
                    ^^^^^^^^^^^^^^
     11 |   let a = Test.originate_contract c s 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 10, characters 26-60:
      9 |   let c : (unit, (int,string) map) michelson_contract = Test.read_contract_from_file "contract_under_test/other_compiled.tz" in
     10 |   let s = Test.decompile (Test.constant_to_michelson_program "{ Elt 1 \\"hi\\" }") in
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     11 |   let a = Test.originate_contract c s 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.parse` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 11, characters 10-33:
     10 |   let s = Test.decompile (Test.constant_to_michelson_program "{ Elt 1 \\"hi\\" }") in
     11 |   let a = Test.originate_contract c s 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^
     12 |   let s = Test.get_storage a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.michelson` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 12, characters 10-26:
     11 |   let a = Test.originate_contract c s 0tez in
     12 |   let s = Test.get_storage a in
                    ^^^^^^^^^^^^^^^^
     13 |   let () = Test.log (s) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_read_contract.mligo", line 13, characters 11-19:
     12 |   let s = Test.get_storage a in
     13 |   let () = Test.log (s) in
                     ^^^^^^^^
     14 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS
    [1 -> "hi"]
    Everything at the top-level was executed.
    - test_foo exited with value ().
    - test_bar exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "cli_arg.mligo"; "--arg"; "[ 1 ; 2 ; 3]" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_cli_arg exited with value [1 ; 2 ; 3]. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "reset_time.mligo" ];
  [%expect
    {|
  File "./reset_time.mligo", line 3, characters 11-30:
    2 |   let t1 = Tezos.get_now () in
    3 |   let () = Test.reset_state_at ("2012-02-02t10:10:10Z" : timestamp) 2n ([] : tez list) in
                   ^^^^^^^^^^^^^^^^^^^
    4 |   let t2 = Tezos.get_now () in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration.

  Everything at the top-level was executed.
  - test_x exited with value (timestamp(1970-01-01T00:00:00Z) , timestamp(2012-02-02T10:10:10Z)). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_get_account.mligo" ];
  [%expect
    {|
    File "./test_get_account.mligo", line 2, characters 20-46:
      1 | let test =
      2 |   let (a, pk, sk) = Test.get_bootstrap_account 0n in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^
      3 |   let () = Test.log (a, pk, sk) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.info` from `Test.Next` is encouraged for a smoother migration.

    File "./test_get_account.mligo", line 3, characters 11-19:
      2 |   let (a, pk, sk) = Test.get_bootstrap_account 0n in
      3 |   let () = Test.log (a, pk, sk) in
                     ^^^^^^^^
      4 |   let () = Test.log (Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_get_account.mligo", line 4, characters 11-19:
      3 |   let () = Test.log (a, pk, sk) in
      4 |   let () = Test.log (Test.get_balance_of_address a) in
                     ^^^^^^^^
      5 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_get_account.mligo", line 4, characters 21-48:
      3 |   let () = Test.log (a, pk, sk) in
      4 |   let () = Test.log (Test.get_balance_of_address a) in
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    (tz1MBWU1WkszFfkEER2pgn4ATKXE9ng7x1sR , edpkusHqa6fxkGPPL9YpgbcakvSTvcTBcwnLAmCdcevmws4Mh2MdHB , "edsk41aRaPPBpidY7w5xu54edk76uJJtJ6myTwYDEWhAwNHce9gKNo")
    3800000000000mutez
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_sign.mligo" ];
  [%expect
    {|
    File "./test_sign.mligo", line 2, characters 17-33:
      1 | let test =
      2 |   let (sk, pk) = Test.new_account () in
                           ^^^^^^^^^^^^^^^^
      3 |   let data = Bytes.pack "Bonjour le monde !" in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration.

    File "./test_sign.mligo", line 4, characters 10-19:
      3 |   let data = Bytes.pack "Bonjour le monde !" in
      4 |   let s = Test.sign sk data in
                    ^^^^^^^^^
      5 |   assert (Crypto.check pk s data)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Crypto.sign` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_create.mligo" ];
  [%expect
    {|
    File "./test_create.mligo", line 10, characters 13-27:
      9 | let test =
     10 |   let orig = Test.originate (contract_of Factory) ([] : address list) 10tez in
                       ^^^^^^^^^^^^^^
     11 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create.mligo", line 11, characters 10-27:
     10 |   let orig = Test.originate (contract_of Factory) ([] : address list) 10tez in
     11 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
                    ^^^^^^^^^^^^^^^^^
     12 |   let addr : address = Option.unopt (List.head_opt (Test.get_storage orig.addr)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create.mligo", line 12, characters 52-68:
     11 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
     12 |   let addr : address = Option.unopt (List.head_opt (Test.get_storage orig.addr)) in
                                                              ^^^^^^^^^^^^^^^^
     13 |   let taddr : (int, int) typed_address = Test.cast_address addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create.mligo", line 13, characters 41-58:
     12 |   let addr : address = Option.unopt (List.head_opt (Test.get_storage orig.addr)) in
     13 |   let taddr : (int, int) typed_address = Test.cast_address addr in
                                                   ^^^^^^^^^^^^^^^^^
     14 |   Test.log (Test.get_storage taddr)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.to_typed_address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create.mligo", line 14, characters 2-10:
     13 |   let taddr : (int, int) typed_address = Test.cast_address addr in
     14 |   Test.log (Test.get_storage taddr)
            ^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create.mligo", line 14, characters 12-28:
     13 |   let taddr : (int, int) typed_address = Test.cast_address addr in
     14 |   Test.log (Test.get_storage taddr)
                      ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    42
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_create2.mligo" ];
  [%expect
    {|
    File "./test_create2.mligo", line 10, characters 13-27:
      9 | let test =
     10 |   let orig = Test.originate (contract_of Factory) ([] : address list) 10tez in
                       ^^^^^^^^^^^^^^
     11 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create2.mligo", line 11, characters 10-27:
     10 |   let orig = Test.originate (contract_of Factory) ([] : address list) 10tez in
     11 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
                    ^^^^^^^^^^^^^^^^^
     12 |   let addr : address = Option.unopt (List.head_opt (Test.get_storage orig.addr)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create2.mligo", line 12, characters 52-68:
     11 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
     12 |   let addr : address = Option.unopt (List.head_opt (Test.get_storage orig.addr)) in
                                                              ^^^^^^^^^^^^^^^^
     13 |   let taddr : (int, int) typed_address = Test.cast_address addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create2.mligo", line 13, characters 41-58:
     12 |   let addr : address = Option.unopt (List.head_opt (Test.get_storage orig.addr)) in
     13 |   let taddr : (int, int) typed_address = Test.cast_address addr in
                                                   ^^^^^^^^^^^^^^^^^
     14 |   Test.log (Test.get_storage taddr)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.to_typed_address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create2.mligo", line 14, characters 2-10:
     13 |   let taddr : (int, int) typed_address = Test.cast_address addr in
     14 |   Test.log (Test.get_storage taddr)
            ^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_create2.mligo", line 14, characters 12-28:
     13 |   let taddr : (int, int) typed_address = Test.cast_address addr in
     14 |   Test.log (Test.get_storage taddr)
                      ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    42
    Everything at the top-level was executed.
    - test exited with value (). |}]

(*
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_transfer_entrypoint.ligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]
*)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_print.mligo" ];
  [%expect
    {|
    File "./test_print.mligo", line 3, characters 11-21:
      2 | let test =
      3 |   let () = Test.print "Hello " in
                     ^^^^^^^^^^
      4 |   let () = Test.println "world" in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print.mligo", line 4, characters 11-23:
      3 |   let () = Test.print "Hello " in
      4 |   let () = Test.println "world" in
                     ^^^^^^^^^^^^
      5 |   let () = Test.print (Option.unopt (Test.chr 64n)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print.mligo", line 5, characters 11-21:
      4 |   let () = Test.println "world" in
      5 |   let () = Test.print (Option.unopt (Test.chr 64n)) in
                     ^^^^^^^^^^
      6 |   let () = Test.print (Test.to_string 42) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print.mligo", line 5, characters 37-45:
      4 |   let () = Test.println "world" in
      5 |   let () = Test.print (Option.unopt (Test.chr 64n)) in
                                               ^^^^^^^^
      6 |   let () = Test.print (Test.to_string 42) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.chr` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print.mligo", line 6, characters 11-21:
      5 |   let () = Test.print (Option.unopt (Test.chr 64n)) in
      6 |   let () = Test.print (Test.to_string 42) in
                     ^^^^^^^^^^
      7 |   let () = Test.print Test.nl in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print.mligo", line 6, characters 23-37:
      5 |   let () = Test.print (Option.unopt (Test.chr 64n)) in
      6 |   let () = Test.print (Test.to_string 42) in
                                 ^^^^^^^^^^^^^^
      7 |   let () = Test.print Test.nl in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print.mligo", line 7, characters 11-21:
      6 |   let () = Test.print (Test.to_string 42) in
      7 |   let () = Test.print Test.nl in
                     ^^^^^^^^^^
      8 |   Test.to_string (true, 42n)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print.mligo", line 7, characters 22-29:
      6 |   let () = Test.print (Test.to_string 42) in
      7 |   let () = Test.print Test.nl in
                                ^^^^^^^
      8 |   Test.to_string (true, 42n)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.nl` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print.mligo", line 8, characters 2-16:
      7 |   let () = Test.print Test.nl in
      8 |   Test.to_string (true, 42n)
            ^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    Hello world
    @42
    Everything at the top-level was executed.
    - test exited with value "(true , 42n)". |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_eprint.mligo" ];
  [%expect
    {|
    Ooops
    File "./test_eprint.mligo", line 3, characters 2-13:
      2 | let test =
      3 |   Test.eprint ("Ooops" ^ Test.nl)
            ^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.eprint` from `Test.Next` is encouraged for a smoother migration.

    File "./test_eprint.mligo", line 3, characters 25-32:
      2 | let test =
      3 |   Test.eprint ("Ooops" ^ Test.nl)
                                   ^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.nl` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_random.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_untranspile_bls.mligo" ];
  [%expect
    {|
    File "./test_untranspile_bls.mligo", line 3, characters 14-32:
      2 |   let bls = (0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f624 : bls12_381_fr) in
      3 |   let value = Test.compile_value bls in
                        ^^^^^^^^^^^^^^^^^^
      4 |   let dec = (Test.decompile value : bls12_381_fr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 4, characters 13-27:
      3 |   let value = Test.compile_value bls in
      4 |   let dec = (Test.decompile value : bls12_381_fr) in
                       ^^^^^^^^^^^^^^
      5 |   assert (Test.to_string dec = Test.to_string bls)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 5, characters 10-24:
      4 |   let dec = (Test.decompile value : bls12_381_fr) in
      5 |   assert (Test.to_string dec = Test.to_string bls)
                    ^^^^^^^^^^^^^^
      6 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 5, characters 31-45:
      4 |   let dec = (Test.decompile value : bls12_381_fr) in
      5 |   assert (Test.to_string dec = Test.to_string bls)
                                         ^^^^^^^^^^^^^^
      6 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 9, characters 14-32:
      8 |   let bls = (0x024142bc89bf29017a38d0ee97711098639aa0bbc5b54b5104cc88b1c0fd09330fb8341e3da91e7a50f0da5c988517db0f52df51f745392ecdd3ffbb50f8a25fcdec6f48886b650de26821e244cb8ab69d49722d290a420ce1284b909d3e15a0 : bls12_381_g1) in
      9 |   let value = Test.compile_value bls in
                        ^^^^^^^^^^^^^^^^^^
     10 |   let dec = (Test.decompile value : bls12_381_g1) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 10, characters 13-27:
      9 |   let value = Test.compile_value bls in
     10 |   let dec = (Test.decompile value : bls12_381_g1) in
                       ^^^^^^^^^^^^^^
     11 |   assert (Test.to_string dec = Test.to_string bls)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 11, characters 10-24:
     10 |   let dec = (Test.decompile value : bls12_381_g1) in
     11 |   assert (Test.to_string dec = Test.to_string bls)
                    ^^^^^^^^^^^^^^
     12 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 11, characters 31-45:
     10 |   let dec = (Test.decompile value : bls12_381_g1) in
     11 |   assert (Test.to_string dec = Test.to_string bls)
                                         ^^^^^^^^^^^^^^
     12 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 15, characters 14-32:
     14 |   let bls = (0x0050b3ab4877c99ce7f180e879d91eb4df24b1e20ed88f1fdde42f91dfe0e7e451aa35d1457dd15ab507fc8f2b3180550ca7b4ea9b67810e346456c35060c8d542f37ee5fe2b1461e2f02fefac55a9863e94cab5c16befad3b866a42ee20835b1351f3f9c20a05586c1d647d756efb5c575d7ab23fbf5b3e1a6ffe024633a63a668a01fcab440866035ea2c0d4bfe30a1242f67119650e2aa605289ade2684287192382d6a01d7865fcd9e1507264a80f387b6441e37438c888159827a4efa67 : bls12_381_g2) in
     15 |   let value = Test.compile_value bls in
                        ^^^^^^^^^^^^^^^^^^
     16 |   let dec = (Test.decompile value : bls12_381_g2) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 16, characters 13-27:
     15 |   let value = Test.compile_value bls in
     16 |   let dec = (Test.decompile value : bls12_381_g2) in
                       ^^^^^^^^^^^^^^
     17 |   assert (Test.to_string dec = Test.to_string bls)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 17, characters 10-24:
     16 |   let dec = (Test.decompile value : bls12_381_g2) in
     17 |   assert (Test.to_string dec = Test.to_string bls)
                    ^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File "./test_untranspile_bls.mligo", line 17, characters 31-45:
     16 |   let dec = (Test.decompile value : bls12_381_g2) in
     17 |   assert (Test.to_string dec = Test.to_string bls)
                                         ^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_fr exited with value ().
    - test_g1 exited with value ().
    - test_g2 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "get_contract.mligo" ];
  [%expect
    {|
    File "./get_contract.mligo", line 11, characters 13-27:
     10 | let test =
     11 |   let orig = Test.originate (contract_of C) 0 0tez in
                       ^^^^^^^^^^^^^^
     12 |   let ta = orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./get_contract.mligo", line 13, characters 10-26:
     12 |   let ta = orig.addr in
     13 |   let c = Test.to_contract ta in
                    ^^^^^^^^^^^^^^^^
     14 |   let a = Tezos.address c in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_key.mligo" ];
  [%expect
    {|
    File "./test_key.mligo", line 16, characters 24-50:
     15 | let test =
     16 |   let (_, pub_key, _) = Test.get_bootstrap_account 1n in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
     17 |   let orig = Test.originate (contract_of C) {registry = Big_map.empty; next_id = 1n} 0mutez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.info` from `Test.Next` is encouraged for a smoother migration.

    File "./test_key.mligo", line 17, characters 13-27:
     16 |   let (_, pub_key, _) = Test.get_bootstrap_account 1n in
     17 |   let orig = Test.originate (contract_of C) {registry = Big_map.empty; next_id = 1n} 0mutez in
                       ^^^^^^^^^^^^^^
     18 |   let () = Test.log pub_key in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_key.mligo", line 18, characters 11-19:
     17 |   let orig = Test.originate (contract_of C) {registry = Big_map.empty; next_id = 1n} 0mutez in
     18 |   let () = Test.log pub_key in
                     ^^^^^^^^
     19 |   Test.transfer orig.addr (Main pub_key) 0mutez
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_key.mligo", line 19, characters 2-15:
     18 |   let () = Test.log pub_key in
     19 |   Test.transfer orig.addr (Main pub_key) 0mutez
            ^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    edpktom5rsehpEY6Kp2NShwsnpaaEjWxKFMJ3Rjp99VMJuHS93wxD6
    Everything at the top-level was executed.
    - test exited with value Success (1720n). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_tickets_and_bigmaps.mligo" ];
  [%expect
    {|
    File "./test_tickets_and_bigmaps.mligo", line 40, characters 11-27:
     39 | let test_one =
     40 |   let () = Test.reset_state 2n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
     41 |   let sender_ = Test.nth_bootstrap_account 1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_tickets_and_bigmaps.mligo", line 41, characters 16-42:
     40 |   let () = Test.reset_state 2n ([] : tez list) in
     41 |   let sender_ = Test.nth_bootstrap_account 1 in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
     42 |   let () = Test.set_source sender_ in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./test_tickets_and_bigmaps.mligo", line 42, characters 11-26:
     41 |   let sender_ = Test.nth_bootstrap_account 1 in
     42 |   let () = Test.set_source sender_ in
                     ^^^^^^^^^^^^^^^
     43 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "./test_tickets_and_bigmaps.mligo", line 51, characters 13-27:
     50 |
     51 |   let orig = Test.originate (contract_of C) init_storage 0mutez in
                       ^^^^^^^^^^^^^^
     52 |   let r = Test.transfer orig.addr (Main ()) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_tickets_and_bigmaps.mligo", line 52, characters 10-23:
     51 |   let orig = Test.originate (contract_of C) init_storage 0mutez in
     52 |   let r = Test.transfer orig.addr (Main ()) 1tez in
                    ^^^^^^^^^^^^^
     53 |   Test.log (r)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_tickets_and_bigmaps.mligo", line 53, characters 2-10:
     52 |   let r = Test.transfer orig.addr (Main ()) 1tez in
     53 |   Test.log (r)
            ^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    Success (2673n)
    Everything at the top-level was executed.
    - test_one exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_chain_id.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value 0x050a0000000400000000. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_print_values.mligo" ];
  [%expect
    {|
    File "./test_print_values.mligo", line 1, characters 9-32:
      1 | let () = Test.unset_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^^^
      2 | let () = Test.println "aloh"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.unset_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print_values.mligo", line 2, characters 9-21:
      1 | let () = Test.unset_print_values ()
      2 | let () = Test.println "aloh"
                   ^^^^^^^^^^^^
      3 | let test = 42
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    aloh |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_to_json.mligo" ];
  [%expect
    {|
    File "./test_to_json.mligo", line 1, characters 9-32:
      1 | let () = Test.unset_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^^^
      2 | module C = struct
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.unset_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "./test_to_json.mligo", line 8, characters 13-27:
      7 | let test_to_json =
      8 |   let orig = Test.originate (contract_of C) { foo = 42 ; bar = ["hello"; "world"] } 0tez in
                       ^^^^^^^^^^^^^^
      9 |   let () = Test.println (Test.to_json orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_to_json.mligo", line 9, characters 11-23:
      8 |   let orig = Test.originate (contract_of C) { foo = 42 ; bar = ["hello"; "world"] } 0tez in
      9 |   let () = Test.println (Test.to_json orig.addr) in
                     ^^^^^^^^^^^^
     10 |   let () = Test.println (Test.to_json (Test.get_storage orig.addr)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    File "./test_to_json.mligo", line 9, characters 25-37:
      8 |   let orig = Test.originate (contract_of C) { foo = 42 ; bar = ["hello"; "world"] } 0tez in
      9 |   let () = Test.println (Test.to_json orig.addr) in
                                   ^^^^^^^^^^^^
     10 |   let () = Test.println (Test.to_json (Test.get_storage orig.addr)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.json` from `Test.Next` is encouraged for a smoother migration.

    File "./test_to_json.mligo", line 10, characters 11-23:
      9 |   let () = Test.println (Test.to_json orig.addr) in
     10 |   let () = Test.println (Test.to_json (Test.get_storage orig.addr)) in
                     ^^^^^^^^^^^^
     11 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    File "./test_to_json.mligo", line 10, characters 25-37:
      9 |   let () = Test.println (Test.to_json orig.addr) in
     10 |   let () = Test.println (Test.to_json (Test.get_storage orig.addr)) in
                                   ^^^^^^^^^^^^
     11 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.json` from `Test.Next` is encouraged for a smoother migration.

    File "./test_to_json.mligo", line 10, characters 39-55:
      9 |   let () = Test.println (Test.to_json orig.addr) in
     10 |   let () = Test.println (Test.to_json (Test.get_storage orig.addr)) in
                                                 ^^^^^^^^^^^^^^^^
     11 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    ["typed_address","KT19SRGEVxDMKdou6Fu7vZrtPy6X9GB7Dwna"]
    ["record",[[["Label","bar",["File",{"start":{"byte":{"pos_fname":"./test_to_json.mligo","pos_lnum":3,"pos_bol":0,"pos_cnum":31},"point_num":139,"point_bol":108},"stop":{"byte":{"pos_fname":"./test_to_json.mligo","pos_lnum":3,"pos_bol":0,"pos_cnum":34},"point_num":142,"point_bol":108}}]],["list",[["constant",["string","hello"]],["constant",["string","world"]]]]],[["Label","foo",["File",{"start":{"byte":{"pos_fname":"./test_to_json.mligo","pos_lnum":3,"pos_bol":0,"pos_cnum":19},"point_num":127,"point_bol":108},"stop":{"byte":{"pos_fname":"./test_to_json.mligo","pos_lnum":3,"pos_bol":0,"pos_cnum":22},"point_num":130,"point_bol":108}}]],["constant",["int","42"]]]]] |}]

(*
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_imm.ligo" ];
  [%expect
    {test|
    Everything at the top-level was executed.
    - test_orig exited with value (). |test}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_record.ligo" ];
  [%expect
    {test|
    0
    Everything at the top-level was executed.
    - test_reproducing exited with value "OK". |test}]
*)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "tuple_long.mligo" ];
  [%expect
    {test|
    File "./tuple_long.mligo", line 11, characters 11-22:
     10 |
     11 | let test = Test.assert (x0 = 0 && f br = 0)
                     ^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |test}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_compare_setmap.mligo" ];
  [%expect
    {test|
    File "./test_compare_setmap.mligo", line 6, characters 23-37:
      5 |  let s = Set.add ("tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ" : address) s in
      6 |  let s : address set = Test.decompile (Test.eval s) in
                                 ^^^^^^^^^^^^^^
      7 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 6, characters 39-48:
      5 |  let s = Set.add ("tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ" : address) s in
      6 |  let s : address set = Test.decompile (Test.eval s) in
                                                 ^^^^^^^^^
      7 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 7, characters 1-10:
      6 |  let s : address set = Test.decompile (Test.eval s) in
      7 |  Test.eval s
           ^^^^^^^^^
      8 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 13, characters 19-33:
     12 |  let s = Set.add 3 s in
     13 |  let s : int set = Test.decompile (Test.eval s) in
                             ^^^^^^^^^^^^^^
     14 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 13, characters 35-44:
     12 |  let s = Set.add 3 s in
     13 |  let s : int set = Test.decompile (Test.eval s) in
                                             ^^^^^^^^^
     14 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 14, characters 1-10:
     13 |  let s : int set = Test.decompile (Test.eval s) in
     14 |  Test.eval s
           ^^^^^^^^^
     15 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 20, characters 30-44:
     19 |  let s = Map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
     20 |  let s : (address, int) map = Test.decompile (Test.eval s) in
                                        ^^^^^^^^^^^^^^
     21 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 20, characters 46-55:
     19 |  let s = Map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
     20 |  let s : (address, int) map = Test.decompile (Test.eval s) in
                                                        ^^^^^^^^^
     21 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 21, characters 1-10:
     20 |  let s : (address, int) map = Test.decompile (Test.eval s) in
     21 |  Test.eval s
           ^^^^^^^^^
     22 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 27, characters 34-48:
     26 |  let s = Big_map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
     27 |  let s : (address, int) big_map = Test.decompile (Test.eval s) in
                                            ^^^^^^^^^^^^^^
     28 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 27, characters 50-59:
     26 |  let s = Big_map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
     27 |  let s : (address, int) big_map = Test.decompile (Test.eval s) in
                                                            ^^^^^^^^^
     28 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 28, characters 1-10:
     27 |  let s : (address, int) big_map = Test.decompile (Test.eval s) in
     28 |  Test.eval s
           ^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_address_set exited with value { "tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" ;
      "tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ" }.
    - test_int_set exited with value { 3 ; 4 }.
    - test_map exited with value { Elt "tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" 900 ;
      Elt "KT1WoTZUkky48v3QqZWzkeJCYfhWhNaVFYuC" 100 }.
    - test_big_map exited with value { Elt "tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" 900 ;
      Elt "KT1WoTZUkky48v3QqZWzkeJCYfhWhNaVFYuC" 100 }. |test}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "test-expr"
    ; "cameligo"
    ; "type t = [@layout:comb] { num : int ; num_nat : nat ; str : string } in let v = \
       Test.parse_michelson {| { Elt 1 (Pair 1 1 \"q\") } |} in ((Test.decompile v : \
       (nat, t) big_map))"
    ];
  [%expect
    {test|
    Everything at the top-level was executed.
    - eval exited with value [1n -> {num = 1 ; num_nat = 1n ; str = "q"}]. |test}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "test"; test "display_format_json.mligo"; "--display-format"; "json" ];
  [%expect
    {xxx|
    [
      [ "test_x", [ "constant", [ "int", "65" ] ] ],
      [ "test_y", [ "constant", [ "string", "hello" ] ] ]
    ] |xxx}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "record_field_assign.jsligo" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test_simple_record_assign exited with value ().
      - test_nested_record_assign_level1 exited with value ().
      - test_nested_record_assign_level2 exited with value ().
      - test_record_assign_var exited with value ().
      - test_nested_record_assign_var_level1 exited with value ().
      - test_nested_record_assign_var_level2 exited with value ().
      - test_nested_record_assign_var_level2_expr exited with value ().
      - test_nested_record_assign_var_level2_record_access exited with value ().
      - test_nested_record_assign_var_level2_module_member exited with value ().
      - test_nested_record_assign_var_level2_module_record_member exited with value ().
      - test_nested_record_assign_var_level2_lambda exited with value ().
      - test_nested_record_assign_var_level2_lambda_app exited with value ().
      - test_simple_tuple_field_assign exited with value ().
      - test_simple_record_field_with_array_notation_assign exited with value ().
      - test_nested_record_assign_array_notation_level1 exited with value ().
      - test_nested_record_assign_array_notation_level2 exited with value ().
      - test_nested_record_assign_tuple_assign_array_notation_level2 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_originate_module.mligo" ];
  [%expect
    {test|
    File "./test_originate_module.mligo", line 1, characters 9-32:
      1 | let () = Test.unset_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.unset_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 13, characters 40-54:
     12 | let test =
     13 |   let {addr = ta; code = m; size = _} = Test.originate (contract_of Bar.Foo) 0 0tez in
                                                  ^^^^^^^^^^^^^^
     14 |   let () = Test.println "Deployed the contract:" in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 14, characters 11-23:
     13 |   let {addr = ta; code = m; size = _} = Test.originate (contract_of Bar.Foo) 0 0tez in
     14 |   let () = Test.println "Deployed the contract:" in
                     ^^^^^^^^^^^^
     15 |   let () = Test.println (Test.to_string m) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 15, characters 11-23:
     14 |   let () = Test.println "Deployed the contract:" in
     15 |   let () = Test.println (Test.to_string m) in
                     ^^^^^^^^^^^^
     16 |   let () = Test.println ("With storage: " ^ Test.to_string (Test.get_storage ta)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 15, characters 25-39:
     14 |   let () = Test.println "Deployed the contract:" in
     15 |   let () = Test.println (Test.to_string m) in
                                   ^^^^^^^^^^^^^^
     16 |   let () = Test.println ("With storage: " ^ Test.to_string (Test.get_storage ta)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 16, characters 11-23:
     15 |   let () = Test.println (Test.to_string m) in
     16 |   let () = Test.println ("With storage: " ^ Test.to_string (Test.get_storage ta)) in
                     ^^^^^^^^^^^^
     17 |   let c : (Bar.Foo parameter_of) contract = Test.to_contract ta in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 16, characters 44-58:
     15 |   let () = Test.println (Test.to_string m) in
     16 |   let () = Test.println ("With storage: " ^ Test.to_string (Test.get_storage ta)) in
                                                      ^^^^^^^^^^^^^^
     17 |   let c : (Bar.Foo parameter_of) contract = Test.to_contract ta in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 16, characters 60-76:
     15 |   let () = Test.println (Test.to_string m) in
     16 |   let () = Test.println ("With storage: " ^ Test.to_string (Test.get_storage ta)) in
                                                                      ^^^^^^^^^^^^^^^^
     17 |   let c : (Bar.Foo parameter_of) contract = Test.to_contract ta in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 17, characters 44-60:
     16 |   let () = Test.println ("With storage: " ^ Test.to_string (Test.get_storage ta)) in
     17 |   let c : (Bar.Foo parameter_of) contract = Test.to_contract ta in
                                                      ^^^^^^^^^^^^^^^^
     18 |   let _ = Test.transfer_to_contract_exn c (Add 42) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 18, characters 10-39:
     17 |   let c : (Bar.Foo parameter_of) contract = Test.to_contract ta in
     18 |   let _ = Test.transfer_to_contract_exn c (Add 42) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     19 |   let () = Test.println ("Storage after call: " ^ Test.to_string (Test.get_storage ta)) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 19, characters 11-23:
     18 |   let _ = Test.transfer_to_contract_exn c (Add 42) 0tez in
     19 |   let () = Test.println ("Storage after call: " ^ Test.to_string (Test.get_storage ta)) in
                     ^^^^^^^^^^^^
     20 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 19, characters 50-64:
     18 |   let _ = Test.transfer_to_contract_exn c (Add 42) 0tez in
     19 |   let () = Test.println ("Storage after call: " ^ Test.to_string (Test.get_storage ta)) in
                                                            ^^^^^^^^^^^^^^
     20 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.mligo", line 19, characters 66-82:
     18 |   let _ = Test.transfer_to_contract_exn c (Add 42) 0tez in
     19 |   let () = Test.println ("Storage after call: " ^ Test.to_string (Test.get_storage ta)) in
                                                                            ^^^^^^^^^^^^^^^^
     20 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Deployed the contract:
    { parameter (or (int %sub) (int %add)) ;
      storage int ;
      code { UNPAIR ; IF_LEFT { SWAP ; SUB } { ADD } ; NIL operation ; PAIR } ;
      view "get" unit int { CDR } ;
      view "get_diff" int int { UNPAIR ; SWAP ; SUB } }
    With storage: 0
    Storage after call: 42 |test}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_originate_module.jsligo" ];
  [%expect
    {test|
    File "./test_originate_module.jsligo", line 13, characters 13-27:
     12 |   let initial_storage = 42;
     13 |   let orig = Test.originate(contract_of(C), initial_storage, 0 as tez);
                       ^^^^^^^^^^^^^^
     14 |   let contr : contract<parameter_of C> = Test.to_contract(orig.addr);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.jsligo", line 14, characters 41-57:
     13 |   let orig = Test.originate(contract_of(C), initial_storage, 0 as tez);
     14 |   let contr : contract<parameter_of C> = Test.to_contract(orig.addr);
                                                   ^^^^^^^^^^^^^^^^
     15 |   let p : parameter_of C = Increment(1);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.jsligo", line 16, characters 2-31:
     15 |   let p : parameter_of C = Increment(1);
     16 |   Test.transfer_to_contract_exn(contr, p, 1 as mutez);
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     17 |   return assert(Test.get_storage(orig.addr) == initial_storage + 1);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_module.jsligo", line 17, characters 16-32:
     16 |   Test.transfer_to_contract_exn(contr, p, 1 as mutez);
     17 |   return assert(Test.get_storage(orig.addr) == initial_storage + 1);
                          ^^^^^^^^^^^^^^^^
     18 | }) ();
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_increment exited with value (). |test}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_originate_single_view.mligo" ];
  [%expect
    {test|
    File "./test_originate_single_view.mligo", line 1, characters 9-32:
      1 | let () = Test.unset_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.unset_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_single_view.mligo", line 12, characters 13-27:
     11 | let test =
     12 |   let orig = Test.originate (contract_of Bar.Foo) 0 0tez in
                       ^^^^^^^^^^^^^^
     13 |   let _ = Test.transfer_exn orig.addr (Add 42) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_originate_single_view.mligo", line 13, characters 10-27:
     12 |   let orig = Test.originate (contract_of Bar.Foo) 0 0tez in
     13 |   let _ = Test.transfer_exn orig.addr (Add 42) 0tez in
                    ^^^^^^^^^^^^^^^^^
     14 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration. |test}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "contract_with_ticket_storage.mligo" ];
  [%expect
    {|
    File "./contract_with_ticket_storage.mligo", line 22, characters 13-21:
     21 |   | Some { ticketer=_ ; value ; amount } ->
     22 |     let () = Test.log ("unforged_ticket", unforged_storage) in
                       ^^^^^^^^
     23 |     let () = assert (value = ticket_info.0) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    ("unforged_ticket" , Some ({amount = 15n ; ticketer = KT1CDHnKFHBMFtyzC92oTfi4Z5wthR4Yk3LW ; value = 0x0202}))
    Everything at the top-level was executed.
    - test_originate_contract exited with value (). |}];
  run_ligo_good [ "run"; "test"; test "contract_with_ticket_param.mligo" ];
  [%expect
    {|
    File "./contract_with_ticket_param.mligo", line 12, characters 13-27:
     11 | let test_transfer_to_contract =
     12 |   let orig = Test.originate (contract_of C) ("bye",Test.nth_bootstrap_account 1) 1mutez in
                       ^^^^^^^^^^^^^^
     13 |   let main_addr = Tezos.address (Test.to_contract orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 12, characters 51-77:
     11 | let test_transfer_to_contract =
     12 |   let orig = Test.originate (contract_of C) ("bye",Test.nth_bootstrap_account 1) 1mutez in
                                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |   let main_addr = Tezos.address (Test.to_contract orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 13, characters 33-49:
     12 |   let orig = Test.originate (contract_of C) ("bye",Test.nth_bootstrap_account 1) 1mutez in
     13 |   let main_addr = Tezos.address (Test.to_contract orig.addr) in
                                           ^^^^^^^^^^^^^^^^
     14 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 32, characters 16-43:
     31 |   in
     32 |   let s, addr = Test.get_storage_of_address main_addr in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     33 |   let p_addr = proxy_taddr |> Test.to_contract |> Tezos.address in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 33, characters 30-46:
     32 |   let s, addr = Test.get_storage_of_address main_addr in
     33 |   let p_addr = proxy_taddr |> Test.to_contract |> Tezos.address in
                                        ^^^^^^^^^^^^^^^^
     34 |   assert (s = "world" && addr = p_addr)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_transfer_to_contract exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_prefix_posfix_ops.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_prefix_ops exited with value ().
    - test_postfix_ops exited with value ().
    - test1 exited with value ().
    - test2 exited with value ().
    - test3 exited with value ().
    - test4 exited with value ().
    - test5 exited with value ().
    - test6 exited with value ().
    - test7 exited with value ().
    - test8 exited with value ().
    - test9 exited with value ().
    - test10 exited with value ().
    - test11 exited with value ().
    - test12 exited with value ().
    - test1_ exited with value ().
    - test2_ exited with value ().
    - test3_ exited with value ().
    - test4_ exited with value ().
    - test5_ exited with value ().
    - test6_ exited with value ().
    - test7_ exited with value ().
    - test8_ exited with value ().
    - test9_ exited with value ().
    - test10_ exited with value ().
    - test11_ exited with value ().
    - test12_ exited with value (). |}]

let%expect_test "for loops" =
  run_ligo_good [ "run"; "test"; test "/for_loop/single_loop.jsligo" ];
  [%expect
    {|
      File ".//for_loop/single_loop.jsligo", line 23, characters 2-13:
       22 |   const abba = "abba";
       23 |   Test.assert(isPalindrome(abba));
              ^^^^^^^^^^^
       24 |   Test.assert(isPalindrome_(abba));
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

      File ".//for_loop/single_loop.jsligo", line 24, characters 2-13:
       23 |   Test.assert(isPalindrome(abba));
       24 |   Test.assert(isPalindrome_(abba));
              ^^^^^^^^^^^
       25 |   const ababa = "ababa";
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

      File ".//for_loop/single_loop.jsligo", line 26, characters 2-13:
       25 |   const ababa = "ababa";
       26 |   Test.assert(isPalindrome(ababa));
              ^^^^^^^^^^^
       27 |   Test.assert(isPalindrome_(ababa));
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

      File ".//for_loop/single_loop.jsligo", line 27, characters 2-13:
       26 |   Test.assert(isPalindrome(ababa));
       27 |   Test.assert(isPalindrome_(ababa));
              ^^^^^^^^^^^
       28 |   const abcd = "abcd";
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

      File ".//for_loop/single_loop.jsligo", line 29, characters 2-13:
       28 |   const abcd = "abcd";
       29 |   Test.assert(!isPalindrome(abcd));
              ^^^^^^^^^^^
       30 |   Test.assert(!isPalindrome_(abcd));
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

      File ".//for_loop/single_loop.jsligo", line 30, characters 2-13:
       29 |   Test.assert(!isPalindrome(abcd));
       30 |   Test.assert(!isPalindrome_(abcd));
              ^^^^^^^^^^^
       31 |   const abcde = "abcde";
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

      File ".//for_loop/single_loop.jsligo", line 32, characters 2-13:
       31 |   const abcde = "abcde";
       32 |   Test.assert(!isPalindrome(abcde));
              ^^^^^^^^^^^
       33 |   Test.assert(!isPalindrome_(abcde));
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

      File ".//for_loop/single_loop.jsligo", line 33, characters 2-13:
       32 |   Test.assert(!isPalindrome(abcde));
       33 |   Test.assert(!isPalindrome_(abcde));
              ^^^^^^^^^^^
       34 | })();
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

      Everything at the top-level was executed.
      - testPalindrome exited with value (). |}];
  run_ligo_good [ "run"; "test"; test "/for_loop/pascal_triangle.jsligo" ];
  [%expect
    {|
    File ".//for_loop/pascal_triangle.jsligo", line 13, characters 35-45:
     12 | const printSpaces = (n: int): unit => {
     13 |     for (let i = n ; i >= 0 ; i--) Test.print(" ")
                                             ^^^^^^^^^^
     14 | }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 20, characters 8-18:
     19 |     for (const x of xs) {
     20 |         Test.print(x > 9 ? "  " : "   ");
                  ^^^^^^^^^^
     21 |         Test.print(Test.to_string(x));
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 21, characters 8-18:
     20 |         Test.print(x > 9 ? "  " : "   ");
     21 |         Test.print(Test.to_string(x));
                  ^^^^^^^^^^
     22 |     }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 21, characters 19-33:
     20 |         Test.print(x > 9 ? "  " : "   ");
     21 |         Test.print(Test.to_string(x));
                             ^^^^^^^^^^^^^^
     22 |     }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 36, characters 8-20:
     35 |         printNums(nums);
     36 |         Test.println("")
                  ^^^^^^^^^^^^
     37 |     };
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 42, characters 18-30:
     41 | const test8 = printPascalTriangle(8)
     42 | const testspace = Test.println("")
                            ^^^^^^^^^^^^
     43 | const test10 = printPascalTriangle(9)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

                      1
                    1   1
                  1   2   1
                1   3   3   1
              1   4   6   4   1
            1   5  10  10   5   1
          1   6  15  20  15   6   1
        1   7  21  35  35  21   7   1

                        1
                      1   1
                    1   2   1
                  1   3   3   1
                1   4   6   4   1
              1   5  10  10   5   1
            1   6  15  20  15   6   1
          1   7  21  35  35  21   7   1
        1   8  28  56  70  56  28   8   1
    Everything at the top-level was executed.
    - test8 exited with value "".
    - testspace exited with value ().
    - test10 exited with value "". |}];
  run_ligo_good [ "run"; "test"; test "/for_loop/matrix_multiplication.jsligo" ];
  [%expect
    {|
    File ".//for_loop/matrix_multiplication.jsligo", line 15, characters 4-26:
     14 | {
     15 |     Test.assert_with_error(m1c == m2r,
              ^^^^^^^^^^^^^^^^^^^^^^
     16 |     "Error: The number of columns in 1st matrix must be equal to number of rows in 2nd matrix");
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.Error.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/matrix_multiplication.jsligo", line 39, characters 4-15:
     38 |         Map.literal(list([[[0, 0], 4], [[0, 1], 5], [[0, 2], 6]])), 1, 3);
     39 |     Test.assert(m == Map.literal(list([
              ^^^^^^^^^^^
     40 |         [[0, 0], 4] , [[0, 1], 5] , [[0, 2], 6],
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/matrix_multiplication.jsligo", line 55, characters 4-15:
     54 |                           [[2, 0], 16], [[2, 1], 17], [[2, 2], 18]])), 3, 3);
     55 |     Test.assert(m == Map.literal(list([
              ^^^^^^^^^^^
     56 |         [[0, 0], 84] , [[0, 1], 90] , [[0, 2], 96] ,
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/matrix_multiplication.jsligo", line 69, characters 4-15:
     68 |                           [[2, 0], 6]])), 3, 1);
     69 |     Test.assert(m == Map.literal(list([
              ^^^^^^^^^^^
     70 |         [[0, 0], 32]
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/matrix_multiplication.jsligo", line 84, characters 4-15:
     83 |                           [[2, 0], 16], [[2, 1], 17], [[2, 2], 18]])), 3, 3);
     84 |     Test.assert(m == Map.literal(list([
              ^^^^^^^^^^^
     85 |         [[0, 0], 10], [[0, 1], 11], [[0, 2], 12] ,
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_3x1_1x3 exited with value ().
    - test_3x3_3x3 exited with value ().
    - test_1x3_3x1 exited with value ().
    - test_I3x3_3x3 exited with value (). |}];
  run_ligo_good [ "run"; "test"; test "/for_loop/for_loops.jsligo" ];
  [%expect
    {|
    File ".//for_loop/for_loops.jsligo", line 7, characters 2-13:
      6 |   };
      7 |   Test.assert(a == 10)
            ^^^^^^^^^^^
      8 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 13, characters 2-13:
     12 |   };
     13 |   Test.assert(b == 12);
            ^^^^^^^^^^^
     14 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 23, characters 2-13:
     22 |   };
     23 |   Test.assert(d == 15);
            ^^^^^^^^^^^
     24 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 33, characters 2-13:
     32 |   };
     33 |   Test.assert(f == 10);
            ^^^^^^^^^^^
     34 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 37, characters 2-13:
     36 |   for ( ; g < 10 ; ) ;
     37 |   Test.assert(g == 11);
            ^^^^^^^^^^^
     38 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 41, characters 2-13:
     40 |   for ( ;h < 10; h++) ;
     41 |   Test.assert(h == 10);
            ^^^^^^^^^^^
     42 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 47, characters 2-13:
     46 |   for (j = 11 ; j < 10 ; ) ;
     47 |   Test.assert(j == 11);
            ^^^^^^^^^^^
     48 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 53, characters 2-13:
     52 |   for (l = 0 ;l < 10; l++) ;
     53 |   Test.assert(l == 10);
            ^^^^^^^^^^^
     54 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - testLoop exited with value (). |}];
  run_ligo_good [ "run"; "test"; test "/for_loop/for_map.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value Some ((2 , "goodbye")). |}]

let%expect_test "aggregation regression" =
  run_ligo_good [ "run"; "test"; test "agg_bar.mligo" ];
  [%expect
    {|
    File "agg_foo.mligo", line 26, characters 11-23:
     25 | let run_suite (suite : suite) =
     26 |   let () = Test.println ("Running " ^ "<" ^ suite.suite_name ^ ">") in
                     ^^^^^^^^^^^^
     27 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    Running <A simple list extension>
    Everything at the top-level was executed. |}]

(* do not remove that :) *)
let () = Caml.Sys.chdir pwd

let () =
  Caml.Sys.chdir
    "../../test/contracts/interpreter_tests/originate_from_relative_path/test/a/b/"


let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test.mligo" ];
  [%expect
    {|
    File "../../c/e/g/h/bar.mligo", line 4, characters 46-70:
      3 | let originate () =
      4 |     let x : (unit, unit) origination_result = Test.originate_from_file f () 0tez in
                                                        ^^^^^^^^^^^^^^^^^^^^^^^^
      5 |     x.addr
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test.mligo", line 4, characters 10-34:
      3 | let test_originate_from_file_relative_path : (unit, unit) typed_address =
      4 |   let x = Test.originate_from_file "../../../src/contract/unit.mligo" () 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^
      5 |   x.addr
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test.mligo", line 10, characters 13-29:
      9 |   let addr = Foo.originate () in
     10 |   let bef  = Test.get_balance addr in
                       ^^^^^^^^^^^^^^^^
     11 |   let ()   = ignore (Test.transfer addr () 10tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test.mligo", line 11, characters 21-34:
     10 |   let bef  = Test.get_balance addr in
     11 |   let ()   = ignore (Test.transfer addr () 10tez) in
                               ^^^^^^^^^^^^^
     12 |   let aft  = Test.get_balance addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test.mligo", line 12, characters 13-29:
     11 |   let ()   = ignore (Test.transfer addr () 10tez) in
     12 |   let aft  = Test.get_balance addr in
                       ^^^^^^^^^^^^^^^^
     13 |   aft = (bef + 10tez)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate_from_file_relative_path exited with value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS.
    - test_originate_from_file_relative_path_w_r_t_imported_file exited with value true. |}];
  run_ligo_good [ "run"; "test"; test "test.jsligo" ];
  [%expect
    {|
    File "../../c/e/g/h/bar.jsligo", line 4, characters 45-69:
      3 | export const originate = () => {
      4 |   const x : origination_result<unit, unit> = Test.originate_from_file(f, unit, 0mutez);
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^
      5 |   return x.addr
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test.jsligo", line 4, characters 10-34:
      3 | const _test_originate_from_file_relative_path = () : typed_address<unit, unit> => {
      4 |   let x = Test.originate_from_file ("../../../src/contract/unit.mligo", unit, 0 as mutez);
                    ^^^^^^^^^^^^^^^^^^^^^^^^
      5 |   return x.addr
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test.jsligo", line 13, characters 13-29:
     12 |   let addr = Foo.originate();
     13 |   let bef  = Test.get_balance(addr);
                       ^^^^^^^^^^^^^^^^
     14 |   ignore(Test.transfer (addr, unit, 10 as mutez));
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test.jsligo", line 14, characters 9-22:
     13 |   let bef  = Test.get_balance(addr);
     14 |   ignore(Test.transfer (addr, unit, 10 as mutez));
                   ^^^^^^^^^^^^^
     15 |   let aft  = Test.get_balance(addr);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test.jsligo", line 15, characters 13-29:
     14 |   ignore(Test.transfer (addr, unit, 10 as mutez));
     15 |   let aft  = Test.get_balance(addr);
                       ^^^^^^^^^^^^^^^^
     16 |   return aft == (bef + (10 as mutez))
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate_from_file_relative_path exited with value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS.
    - test_originate_from_file_relative_path_w_r_t_imported_file exited with value true. |}]

let () = Caml.Sys.chdir pwd

let () =
  Caml.Sys.chdir "../../test/contracts/interpreter_tests/originate_from_relative_path/"


let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test/a/b/test.mligo" ];
  [%expect
    {|
    File "test/c/e/g/h/bar.mligo", line 4, characters 46-70:
      3 | let originate () =
      4 |     let x : (unit, unit) origination_result = Test.originate_from_file f () 0tez in
                                                        ^^^^^^^^^^^^^^^^^^^^^^^^
      5 |     x.addr
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test/a/b/test.mligo", line 4, characters 10-34:
      3 | let test_originate_from_file_relative_path : (unit, unit) typed_address =
      4 |   let x = Test.originate_from_file "../../../src/contract/unit.mligo" () 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^
      5 |   x.addr
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test/a/b/test.mligo", line 10, characters 13-29:
      9 |   let addr = Foo.originate () in
     10 |   let bef  = Test.get_balance addr in
                       ^^^^^^^^^^^^^^^^
     11 |   let ()   = ignore (Test.transfer addr () 10tez) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test/a/b/test.mligo", line 11, characters 21-34:
     10 |   let bef  = Test.get_balance addr in
     11 |   let ()   = ignore (Test.transfer addr () 10tez) in
                               ^^^^^^^^^^^^^
     12 |   let aft  = Test.get_balance addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test/a/b/test.mligo", line 12, characters 13-29:
     11 |   let ()   = ignore (Test.transfer addr () 10tez) in
     12 |   let aft  = Test.get_balance addr in
                       ^^^^^^^^^^^^^^^^
     13 |   aft = (bef + 10tez)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate_from_file_relative_path exited with value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS.
    - test_originate_from_file_relative_path_w_r_t_imported_file exited with value true. |}];
  run_ligo_good [ "run"; "test"; test "test/a/b/test.jsligo" ];
  [%expect
    {|
    File "test/c/e/g/h/bar.jsligo", line 4, characters 45-69:
      3 | export const originate = () => {
      4 |   const x : origination_result<unit, unit> = Test.originate_from_file(f, unit, 0mutez);
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^
      5 |   return x.addr
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test/a/b/test.jsligo", line 4, characters 10-34:
      3 | const _test_originate_from_file_relative_path = () : typed_address<unit, unit> => {
      4 |   let x = Test.originate_from_file ("../../../src/contract/unit.mligo", unit, 0 as mutez);
                    ^^^^^^^^^^^^^^^^^^^^^^^^
      5 |   return x.addr
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "./test/a/b/test.jsligo", line 13, characters 13-29:
     12 |   let addr = Foo.originate();
     13 |   let bef  = Test.get_balance(addr);
                       ^^^^^^^^^^^^^^^^
     14 |   ignore(Test.transfer (addr, unit, 10 as mutez));
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test/a/b/test.jsligo", line 14, characters 9-22:
     13 |   let bef  = Test.get_balance(addr);
     14 |   ignore(Test.transfer (addr, unit, 10 as mutez));
                   ^^^^^^^^^^^^^
     15 |   let aft  = Test.get_balance(addr);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test/a/b/test.jsligo", line 15, characters 13-29:
     14 |   ignore(Test.transfer (addr, unit, 10 as mutez));
     15 |   let aft  = Test.get_balance(addr);
                       ^^^^^^^^^^^^^^^^
     16 |   return aft == (bef + (10 as mutez))
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate_from_file_relative_path exited with value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS.
    - test_originate_from_file_relative_path_w_r_t_imported_file exited with value true. |}]

let () = Caml.Sys.chdir pwd
let bad_test n = bad_test ("/interpreter_tests/" ^ n)

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_capture_meta_type.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_capture_meta_type.mligo", line 9, characters 2-16:
      8 | let orig =
      9 |   Test.originate (contract_of C) () 0tez
            ^^^^^^^^^^^^^^
     10 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_capture_meta_type.mligo", line 19, characters 11-20:
     18 |
     19 | let test = Test.eval g
                     ^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_capture_meta_type.mligo", line 15, characters 26-27:
     14 |
     15 | let f = fun (_ : unit) -> v.x
                                    ^
     16 |

    Invalid usage of a Test type: typed_address (sum[Main -> unit({ name: Main })] ,
    unit) in record[x -> int ,
                    y -> typed_address (sum[Main -> unit({ name: Main })] , unit)({ name: x }, { name: y })] cannot be translated to Michelson. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_random.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_random.mligo", line 6, characters 46-49:
      5 |   (* We generate the property *)
      6 |   let test = PBT.make_test (PBT.gen_small : ((int contract) list) pbt_gen) (fun (xs : (int contract) list) -> List.length xs = 42n) in
                                                        ^^^
      7 |   (* And run it *)

    Generator for type contract (int) is not implemented. For now, only unit, string, bytes, address, int, nat, tez, records, sums, lists, sets, maps and big_maps can be generated. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_failure1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_failure1.mligo", line 2, characters 2-25:
      1 | let test : unit =
      2 |   failwith "I am failing"
            ^^^^^^^^^^^^^^^^^^^^^^^

    An uncaught error occured:
    Failwith: "I am failing"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_failure1.mligo", line 2, characters 2-25 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_failure2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 2, characters 4-16:
      1 | let test =
      2 |     assert false
              ^^^^^^^^^^^^

    An uncaught error occured:
    Failwith: "failed assertion"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 2, characters 4-16 ,
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 2, characters 4-16 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "bad_balances_reset.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/bad_balances_reset.mligo", line 1, characters 11-27:
      1 | let test = Test.reset_state 2n [4000tez;4000tez]
                     ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/bad_balances_reset.mligo", line 1, characters 11-48:
      1 | let test = Test.reset_state 2n [4000tez;4000tez]
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

     baker account initial balance must at least reach 6000 tez |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_failure3.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_failure3.mligo", line 1, characters 31-32:
      1 | module C = struct [@entry] let f = (fun () () -> ()) end
                                         ^
      2 | let test =

    Not an entrypoint: [_, _]unit -> unit -> unit |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_trace.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 3, characters 4-31:
      2 |   if x < 0 then
      3 |     (failwith "negative" : int)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |   else

    An uncaught error occured:
    Failwith: "negative"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 3, characters 4-31 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 14-49 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 14-49 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_trace2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 7, characters 11-38:
      6 | let make_call (contr : C parameter_of contract) =
      7 |   let () = Test.get_storage_of_address ("KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL" : address) in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |   Test.transfer_to_contract_exn contr (Main ()) 10tez
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 8, characters 2-31:
      7 |   let () = Test.get_storage_of_address ("KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL" : address) in
      8 |   Test.transfer_to_contract_exn contr (Main ()) 10tez
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 12, characters 13-27:
     11 | let test =
     12 |   let orig = Test.originate (contract_of C) () 1tez in
                       ^^^^^^^^^^^^^^
     13 |   make_call (Test.to_contract orig.addr)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 13, characters 13-29:
     12 |   let orig = Test.originate (contract_of C) () 1tez in
     13 |   make_call (Test.to_contract orig.addr)
                       ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 7, characters 11-89:
      6 | let make_call (contr : C parameter_of contract) =
      7 |   let () = Test.get_storage_of_address ("KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL" : address) in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |   Test.transfer_to_contract_exn contr (Main ()) 10tez

    An uncaught error occured:
    Did not find service: GET ocaml:context/contracts/KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL/storage
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 7, characters 11-89 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 7, characters 11-89 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 13, characters 2-40 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_mutation_loop.mligo"; "--steps"; "1000" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 15, characters 10-28:
     14 | let test_mutation =
     15 |     match Test.mutation_test my_rec_fun run_my_function with
                    ^^^^^^^^^^^^^^^^^^
     16 |     | None -> ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.func` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 17, characters 37-45:
     16 |     | None -> ()
     17 |     | Some (_, mutation) -> let () = Test.log(mutation) in
                                               ^^^^^^^^
     18 |                                     failwith "Some mutation also passes the tests!"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 18, characters 36-83:
     17 |     | Some (_, mutation) -> let () = Test.log(mutation) in
     18 |                                     failwith "Some mutation also passes the tests!"
                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    An uncaught error occured:
    Failwith: "Some mutation also passes the tests!"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 18, characters 36-83
    Mutation at: File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 3, characters 29-30:
      2 |     if rounds > 0 then
      3 |         my_rec_fun (rounds - 1)
                                       ^
      4 |     else

    Replacing by: 2. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_source1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_source1.mligo", line 4, characters 45-59:
      3 | let test =
      4 |   let {addr = taddr ; code = _ ; size = _} = Test.originate (contract_of C) () 0tez in
                                                       ^^^^^^^^^^^^^^
      5 |   let contr = Test.to_contract taddr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source1.mligo", line 5, characters 14-30:
      4 |   let {addr = taddr ; code = _ ; size = _} = Test.originate (contract_of C) () 0tez in
      5 |   let contr = Test.to_contract taddr in
                        ^^^^^^^^^^^^^^^^
      6 |   let addr = Tezos.address contr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source1.mligo", line 7, characters 11-19:
      6 |   let addr = Tezos.address contr in
      7 |   let () = Test.log addr in
                     ^^^^^^^^
      8 |   let () = Test.set_source addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source1.mligo", line 8, characters 11-26:
      7 |   let () = Test.log addr in
      8 |   let () = Test.set_source addr in
                     ^^^^^^^^^^^^^^^
      9 |   let _ = Test.originate (contract_of C) () 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source1.mligo", line 9, characters 10-24:
      8 |   let () = Test.set_source addr in
      9 |   let _ = Test.originate (contract_of C) () 0tez in
                    ^^^^^^^^^^^^^^
     10 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source1.mligo", line 9, characters 10-48:
      8 |   let () = Test.set_source addr in
      9 |   let _ = Test.originate (contract_of C) () 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |   ()

    The source address is not an implicit account
    KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_source2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_source2.mligo", line 4, characters 13-27:
      3 | let test =
      4 |   let orig = Test.originate (contract_of C) () 0tez in
                       ^^^^^^^^^^^^^^
      5 |   let () = Test.log orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source2.mligo", line 5, characters 11-19:
      4 |   let orig = Test.originate (contract_of C) () 0tez in
      5 |   let () = Test.log orig.addr in
                     ^^^^^^^^
      6 |   let () = Test.set_source (Test.to_address orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source2.mligo", line 6, characters 11-26:
      5 |   let () = Test.log orig.addr in
      6 |   let () = Test.set_source (Test.to_address orig.addr) in
                     ^^^^^^^^^^^^^^^
      7 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source2.mligo", line 6, characters 28-43:
      5 |   let () = Test.log orig.addr in
      6 |   let () = Test.set_source (Test.to_address orig.addr) in
                                      ^^^^^^^^^^^^^^^
      7 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source2.mligo", line 7, characters 10-27:
      6 |   let () = Test.set_source (Test.to_address orig.addr) in
      7 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
      8 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_source2.mligo", line 7, characters 10-52:
      6 |   let () = Test.set_source (Test.to_address orig.addr) in
      7 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |   ()

    The source address is not an implicit account
    KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_run_types.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types.jsligo", line 2, characters 26-44:
      1 | const foo = (x: {field: int}): {field: int} => {return x};
      2 | const bar = Test.run(foo, {property: "toto"});
                                    ^^^^^^^^^^^^^^^^^^
      3 |

    Mismatching record labels. Expected record of type "record[field -> int]". |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_run_types2.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types2.jsligo", line 2, characters 26-32:
      1 | const foo = (x:  {b:int}):  {b:int} => {return x};
      2 | const bar = Test.run(foo, "toto");
                                    ^^^^^^

    This expression has type "string", but an expression was expected of type
    "record[b -> int]".
    Type "string" is not compatible with type "record[b -> int]". |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_run_types3.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types3.jsligo", line 2, characters 26-41:
      1 | const foo = (x: int): int => {return x};
      2 | const bar = Test.run(foo, {field: "toto"});
                                    ^^^^^^^^^^^^^^^

    This expression has type "record[field -> string]", but an expression was expected of type
    "int".
    Type "record[field -> string]" is not compatible with type "int". |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_decompile.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_decompile.mligo", line 2, characters 10-19:
      1 | let test =
      2 |   let x = Test.eval 4n in
                    ^^^^^^^^^
      3 |   (Test.decompile x : string)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_decompile.mligo", line 3, characters 3-17:
      2 |   let x = Test.eval 4n in
      3 |   (Test.decompile x : string)
             ^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_decompile.mligo", line 3, characters 2-29:
      2 |   let x = Test.eval 4n in
      3 |   (Test.decompile x : string)
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This Michelson value has assigned type 'nat', which does not coincide with expected type 'string'. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_register_delegate.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 7, characters 12-28:
      6 |
      7 |   let acc = Test.new_account () in
                      ^^^^^^^^^^^^^^^^
      8 |   let pkh = Crypto.hash_key acc.1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 12, characters 10-39:
     11 |
     12 |   let _ = Test.transfer_to_contract_exn c () 100000tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |   let () = Test.register_delegate pkh in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 13, characters 11-33:
     12 |   let _ = Test.transfer_to_contract_exn c () 100000tez in
     13 |   let () = Test.register_delegate pkh in
                     ^^^^^^^^^^^^^^^^^^^^^^
     14 |   let () = Test.bake_until_n_cycle_end 2n in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_delegate` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 14, characters 11-38:
     13 |   let () = Test.register_delegate pkh in
     14 |   let () = Test.bake_until_n_cycle_end 2n in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     15 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.bake_until` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 16, characters 11-19:
     15 |
     16 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
                     ^^^^^^^^
     17 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 17, characters 11-19:
     16 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
     17 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     18 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 17, characters 20-47:
     16 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
     17 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     18 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 18, characters 11-19:
     17 |   let () = Test.log(Test.get_balance_of_address a) in
     18 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     19 |   let () = Test.set_baker a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 18, characters 20-41:
     17 |   let () = Test.log(Test.get_balance_of_address a) in
     18 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     19 |   let () = Test.set_baker a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 19, characters 11-25:
     18 |   let () = Test.log(Test.get_voting_power pkh) in
     19 |   let () = Test.set_baker a in
                     ^^^^^^^^^^^^^^
     20 |   let {addr = ta ; code = _ ; size = _} = Test.originate (contract_of C) 41 5tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 20, characters 42-56:
     19 |   let () = Test.set_baker a in
     20 |   let {addr = ta ; code = _ ; size = _} = Test.originate (contract_of C) 41 5tez in
                                                    ^^^^^^^^^^^^^^
     21 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 22, characters 11-19:
     21 |
     22 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
                     ^^^^^^^^
     23 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 23, characters 11-19:
     22 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
     23 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     24 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 23, characters 20-47:
     22 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
     23 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     24 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 24, characters 11-19:
     23 |   let () = Test.log(Test.get_balance_of_address a) in
     24 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     25 |   let cc = Test.to_contract ta in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 24, characters 20-41:
     23 |   let () = Test.log(Test.get_balance_of_address a) in
     24 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     25 |   let cc = Test.to_contract ta in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 25, characters 11-27:
     24 |   let () = Test.log(Test.get_voting_power pkh) in
     25 |   let cc = Test.to_contract ta in
                     ^^^^^^^^^^^^^^^^
     26 |   let _ = Test.transfer_to_contract cc (Main 1) 3tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 26, characters 10-35:
     25 |   let cc = Test.to_contract ta in
     26 |   let _ = Test.transfer_to_contract cc (Main 1) 3tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
     27 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 28, characters 11-19:
     27 |
     28 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
                     ^^^^^^^^
     29 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 29, characters 11-19:
     28 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
     29 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     30 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 29, characters 20-47:
     28 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
     29 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     30 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 30, characters 11-19:
     29 |   let () = Test.log(Test.get_balance_of_address a) in
     30 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     31 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 30, characters 20-41:
     29 |   let () = Test.log(Test.get_balance_of_address a) in
     30 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     31 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 20, characters 42-80:
     19 |   let () = Test.set_baker a in
     20 |   let {addr = ta ; code = _ ; size = _} = Test.originate (contract_of C) 41 5tez in
                                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     21 |

    Baker cannot bake. Enough rolls? Enough cycles passed?
    "STARTING BALANCE AND VOTING POWER"
    95000000000mutez
    100000000000n |}];
  run_ligo_bad [ "run"; "test"; bad_test "test_register_delegate_stake.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 6, characters 12-28:
      5 | let test =
      6 |   let acc = Test.new_account () in
                      ^^^^^^^^^^^^^^^^
      7 |   let pkh = Crypto.hash_key acc.1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 10, characters 10-39:
      9 |
     10 |   let _ = Test.transfer_to_contract_exn c () 1000000tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     11 |   let () = Test.register_delegate pkh in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 11, characters 11-33:
     10 |   let _ = Test.transfer_to_contract_exn c () 1000000tez in
     11 |   let () = Test.register_delegate pkh in
                     ^^^^^^^^^^^^^^^^^^^^^^
     12 |   let () = Test.stake pkh 1000000tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_delegate` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 12, characters 11-21:
     11 |   let () = Test.register_delegate pkh in
     12 |   let () = Test.stake pkh 1000000tez in
                     ^^^^^^^^^^
     13 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.stake` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 12, characters 11-36:
     11 |   let () = Test.register_delegate pkh in
     12 |   let () = Test.stake pkh 1000000tez in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |   ()

    An uncaught error occured:
    { "id": "proto.018-Proxford.operation.manual_staking_forbidden",
      "description":
        "Manual staking operations are forbidden because staking is currently automated.",
      "data": {} }
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 12, characters 11-36 |}]

let pwd = Caml.Sys.getcwd ()
let () = Caml.Sys.chdir "../../test/contracts/negative/interpreter_tests/"

(* using typed_address in Bytes.pack *)
let%expect_test _ =
  run_ligo_bad [ "run"; "test"; "typed_addr_in_bytes_pack.mligo" ];
  [%expect
    {|
  File "typed_addr_in_bytes_pack.mligo", line 2, characters 15-39:
    1 | let originate_record () =
    2 |     let orig = Test.originate_from_file "./unit_contract.mligo" () 0tez in
                       ^^^^^^^^^^^^^^^^^^^^^^^^
    3 |     let addr = Test.to_address orig.addr in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

  File "typed_addr_in_bytes_pack.mligo", line 3, characters 15-30:
    2 |     let orig = Test.originate_from_file "./unit_contract.mligo" () 0tez in
    3 |     let addr = Test.to_address orig.addr in
                       ^^^^^^^^^^^^^^^
    4 |     let contr = Test.to_contract orig.addr in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

  File "typed_addr_in_bytes_pack.mligo", line 4, characters 16-32:
    3 |     let addr = Test.to_address orig.addr in
    4 |     let contr = Test.to_contract orig.addr in
                        ^^^^^^^^^^^^^^^^
    5 |     {
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

  File "typed_addr_in_bytes_pack.mligo", line 18, characters 13-21:
   17 |     ) in
   18 |     let () = Test.log(packed) in
                     ^^^^^^^^
   19 |     ()
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

  File "typed_addr_in_bytes_pack.mligo", line 13, character 17 to line 17, character 5:
   12 |     let r = originate_record () in
   13 |     let packed = Bytes.pack (fun() ->
                         ^^^^^^^^^^^^^^^^^^^^^
   14 |         match (Tezos.get_entrypoint_opt "%transfer" r.addr : unit contract option) with
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   15 |           Some(c) -> let op = Tezos.transaction () 0mutez c in [op]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   16 |         | None ->  ([] : operation list)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   17 |     ) in
        ^^^^^
   18 |     let () = Test.log(packed) in

  Cannot decompile value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS of type typed_address (unit ,
  unit) |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_michelson_non_func.mligo" ];
  [%expect
    {test|
    File "../../test/contracts/negative//interpreter_tests/test_michelson_non_func.mligo", line 4, characters 4-12:
      3 |   begin
      4 |     Test.log x;
              ^^^^^^^^
      5 |     assert (x = x);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_michelson_non_func.mligo", line 2, characters 16-55:
      1 | let test =
      2 |   let x : int = [%Michelson ({|{ PUSH int 1 }|} : int)] in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      3 |   begin

    Embedded raw code can only have a functional type |test}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "get_contract.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/get_contract.mligo", line 11, characters 42-56:
     10 | let test =
     11 |   let {addr = ta ; code = _ ; size = _} = Test.originate (contract_of C) 0 0tez in
                                                    ^^^^^^^^^^^^^^
     12 |   let c = Test.to_contract ta in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/get_contract.mligo", line 12, characters 10-26:
     11 |   let {addr = ta ; code = _ ; size = _} = Test.originate (contract_of C) 0 0tez in
     12 |   let c = Test.to_contract ta in
                    ^^^^^^^^^^^^^^^^
     13 |   let a = Tezos.address c in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/get_contract.mligo", line 18, characters 10-66:
     17 |   let _ = (Tezos.get_contract a : (C parameter_of contract)) in
     18 |   let _ = (Tezos.get_contract_with_error a "foo" : (int contract)) in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     19 |   ()

    An uncaught error occured:
    Failwith: "foo"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/get_contract.mligo", line 18, characters 10-66 ,
    File "../../test/contracts/negative//interpreter_tests/get_contract.mligo", line 18, characters 10-66 |}]
