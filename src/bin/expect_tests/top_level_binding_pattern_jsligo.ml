open Cli_expect

(* Testing *)

let test_ file = test ("top_level_patterns/interpreter/" ^ file)

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/nested_record.jsligo" ] ;
  [%expect{| TODO |}] *)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "jsligo/nested_tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 1, characters 10-31:
      1 | const _ = Test.set_print_values ()
                    ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 4, characters 4-12:
      3 | const f = () => {
      4 |     Test.log("Once");
              ^^^^^^^^
      5 |     return [[1 as nat, 1, "H"], [2 as nat, 2, "E"], [3 as nat, 3, "L"]]
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/record_tuple.jsligo" ] ;
  [%expect{| Everything at the top-level was executed. |}] *)

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/tuple_record.jsligo" ] ;
  [%expect{| Everything at the top-level was executed. |}] *)

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/record.jsligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {| TODO |}] *)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "jsligo/tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 1, characters 10-31:
      1 | const _ = Test.set_print_values ()
                    ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 4, characters 2-10:
      3 | const f = () => {
      4 |   Test.log("Once");
            ^^^^^^^^
      5 |   return [1 as nat, 1, "Hello"]
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* Negative - linearity *)

let contract file = test ("top_level_patterns/negative/" ^ file)

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/nested_record.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "jsligo/nested_tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/nested_tuple.jsligo", line 2, characters 26-28:
      1 | const r = [[1 as nat, 1, "H"], [2 as nat, 2, "E"], [3 as nat, 3, "Hello"]]
      2 | const [[a1, a2, a3], [b1, a2, b3], [c1, c2, c3]] = r
                                    ^^

    Duplicate identifier. |}]

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/record.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "jsligo/tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/tuple.jsligo", line 2, characters 10-11:
      1 | const r = [1 as nat, 1, "Hello"]
      2 | const [a, a, c] = r
                    ^

    Duplicate identifier. |}]

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/record_tuple.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/tuple_record.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

(* Negative - much use *)

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "jsligo/ticket_record.jsligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 3, characters 8-9:
      2 |
      3 | const { b } = { b: Option.unopt(Tezos.create_ticket("one", 10 as nat)) };
                  ^
      4 |
    :
    Warning: variable cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 3, characters 8-9:
      2 |
      3 | const { b } = { b: Option.unopt(Tezos.create_ticket("one", 10 as nat)) };
                  ^
      4 |
    :
    Warning: variable cannot be used more than once.
    { parameter unit ;
      storage (ticket string) ;
      code { DROP ;
             PUSH nat 10 ;
             PUSH string "one" ;
             TICKET ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             DUP ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "jsligo/ticket_tuple.jsligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 1, characters 7-8:
      1 | const [b, _] = [Option.unopt(Tezos.create_ticket("one", 10 as nat)), 1];
                 ^
      2 |
    :
    Warning: variable cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 1, characters 7-8:
      1 | const [b, _] = [Option.unopt(Tezos.create_ticket("one", 10 as nat)), 1];
                 ^
      2 |
    :
    Warning: variable cannot be used more than once.
    { parameter unit ;
      storage (ticket string) ;
      code { DROP ;
             PUSH int 1 ;
             PUSH nat 10 ;
             PUSH string "one" ;
             TICKET ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             SWAP ;
             DROP ;
             DUP ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}]
