open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "option_map.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (option int) ;
      code { CDR ;
             MAP { PUSH string "foo" ; PAIR } ;
             MAP { CDR } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "option_map.mligo" ];
  [%expect
    {|
    File "../../test/contracts/option_map.mligo", line 12, characters 10-16:
     11 |   let b = Option.map to_tup a in
     12 |   let _ = assert (b = (Some ("foo", 1))) in
                    ^^^^^^
     13 |   let _ = assert (a = (Option.map to_int b)) in
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/option_map.mligo", line 13, characters 10-16:
     12 |   let _ = assert (b = (Some ("foo", 1))) in
     13 |   let _ = assert (a = (Option.map to_int b)) in
                    ^^^^^^
     14 |   let a : int option = None in
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/option_map.mligo", line 16, characters 10-16:
     15 |   let b = Option.map to_tup a in
     16 |   let _ = assert (b = (None : (string * int) option)) in
                    ^^^^^^
     17 |   let _ = assert (a = (Option.map to_int b)) in
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/option_map.mligo", line 17, characters 10-16:
     16 |   let _ = assert (b = (None : (string * int) option)) in
     17 |   let _ = assert (a = (Option.map to_int b)) in
                    ^^^^^^
     18 |   ()
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "option_map.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (option int) ;
      code { CDR ;
             MAP { PUSH string "foo" ; PAIR } ;
             MAP { CDR } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "option_map.mligo" ];
  [%expect
    {|
    File "../../test/contracts/option_map.mligo", line 12, characters 10-16:
     11 |   let b = Option.map to_tup a in
     12 |   let _ = assert (b = (Some ("foo", 1))) in
                    ^^^^^^
     13 |   let _ = assert (a = (Option.map to_int b)) in
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/option_map.mligo", line 13, characters 10-16:
     12 |   let _ = assert (b = (Some ("foo", 1))) in
     13 |   let _ = assert (a = (Option.map to_int b)) in
                    ^^^^^^
     14 |   let a : int option = None in
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/option_map.mligo", line 16, characters 10-16:
     15 |   let b = Option.map to_tup a in
     16 |   let _ = assert (b = (None : (string * int) option)) in
                    ^^^^^^
     17 |   let _ = assert (a = (Option.map to_int b)) in
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/option_map.mligo", line 17, characters 10-16:
     16 |   let _ = assert (b = (None : (string * int) option)) in
     17 |   let _ = assert (a = (Option.map to_int b)) in
                    ^^^^^^
     18 |   ()
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "x"; "--init-file"; test "option.mligo" ];
  [%expect {| None |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "n"; "--init-file"; test "option.mligo" ];
  [%expect {| None |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "option_record.mligo" ];
  [%expect
    {|
    File "../../test/contracts/option_record.mligo", line 12, characters 13-27:
     11 | let test =
     12 |   let orig = Test.originate (contract_of C) (None : t) 0tez in
                       ^^^^^^^^^^^^^^
     13 |   let ctr = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/option_record.mligo", line 13, characters 12-28:
     12 |   let orig = Test.originate (contract_of C) (None : t) 0tez in
     13 |   let ctr = Test.to_contract orig.addr in
                      ^^^^^^^^^^^^^^^^
     14 |   let _ = Test.transfer_to_contract_exn ctr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/option_record.mligo", line 14, characters 10-39:
     13 |   let ctr = Test.to_contract orig.addr in
     14 |   let _ = Test.transfer_to_contract_exn ctr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     15 |   let v = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/option_record.mligo", line 15, characters 10-26:
     14 |   let _ = Test.transfer_to_contract_exn ctr (Main ()) 0tez in
     15 |   let v = Test.get_storage orig.addr in
                    ^^^^^^^^^^^^^^^^
     16 |   let v = Option.unopt v in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/option_record.mligo", line 16, characters 10-22:
     15 |   let v = Test.get_storage orig.addr in
     16 |   let v = Option.unopt v in
                    ^^^^^^^^^^^^
     17 |   let s = Option.unopt v.s in
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/option_record.mligo", line 17, characters 10-22:
     16 |   let v = Option.unopt v in
     17 |   let s = Option.unopt v.s in
                    ^^^^^^^^^^^^
     18 |   assert (s = 1)
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/option_record.mligo", line 18, characters 2-8:
     17 |   let s = Option.unopt v.s in
     18 |   assert (s = 1)
            ^^^^^^
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]
