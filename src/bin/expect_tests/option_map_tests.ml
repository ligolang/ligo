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

    Everything at the top-level was executed.
    - test exited with value (). |}]
