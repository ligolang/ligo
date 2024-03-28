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

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 13, characters 4-10:
     12 | const _test = () => {
     13 |     assert ([a1 + b1 + c1] == [a4 + b4 + c4]);
              ^^^^^^
     14 |     assert ([a2 + b2 + c2] == [a5 + b5 + c5]);
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 14, characters 4-10:
     13 |     assert ([a1 + b1 + c1] == [a4 + b4 + c4]);
     14 |     assert ([a2 + b2 + c2] == [a5 + b5 + c5]);
              ^^^^^^
     15 |     assert ([a3 + b3 + c3] == [a6 + b6 + c6])
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 15, characters 4-10:
     14 |     assert ([a2 + b2 + c2] == [a5 + b5 + c5]);
     15 |     assert ([a3 + b3 + c3] == [a6 + b6 + c6])
              ^^^^^^
     16 | }
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

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

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 12, characters 4-10:
     11 | const _test = () => {
     12 |     assert (a == a1);
              ^^^^^^
     13 |     assert (b == b1);
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 13, characters 4-10:
     12 |     assert (a == a1);
     13 |     assert (b == b1);
              ^^^^^^
     14 |     assert (c == c1)
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 14, characters 4-10:
     13 |     assert (b == b1);
     14 |     assert (c == c1)
              ^^^^^^
     15 | }
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

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

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 3, characters 19-31:
      2 |
      3 | const { b } = { b: Option.unopt(Tezos.create_ticket("one", 10 as nat)) };
                             ^^^^^^^^^^^^
      4 |
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 3, characters 32-51:
      2 |
      3 | const { b } = { b: Option.unopt(Tezos.create_ticket("one", 10 as nat)) };
                                          ^^^^^^^^^^^^^^^^^^^
      4 |
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 9, characters 13-25:
      8 | const main = (_p: unit, _s: storage): [list<operation>, storage] =>
      9 |   [list([]), Option.unopt(Tezos.join_tickets([b, b]))];
                       ^^^^^^^^^^^^
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 9, characters 26-44:
      8 | const main = (_p: unit, _s: storage): [list<operation>, storage] =>
      9 |   [list([]), Option.unopt(Tezos.join_tickets([b, b]))];
                                    ^^^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 3, characters 8-9:
      2 |
      3 | const { b } = { b: Option.unopt(Tezos.create_ticket("one", 10 as nat)) };
                  ^
      4 |
    :
    Warning: variable cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 3, characters 19-31:
      2 |
      3 | const { b } = { b: Option.unopt(Tezos.create_ticket("one", 10 as nat)) };
                             ^^^^^^^^^^^^
      4 |
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 3, characters 32-51:
      2 |
      3 | const { b } = { b: Option.unopt(Tezos.create_ticket("one", 10 as nat)) };
                                          ^^^^^^^^^^^^^^^^^^^
      4 |
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 9, characters 13-25:
      8 | const main = (_p: unit, _s: storage): [list<operation>, storage] =>
      9 |   [list([]), Option.unopt(Tezos.join_tickets([b, b]))];
                       ^^^^^^^^^^^^
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 9, characters 26-44:
      8 | const main = (_p: unit, _s: storage): [list<operation>, storage] =>
      9 |   [list([]), Option.unopt(Tezos.join_tickets([b, b]))];
                                    ^^^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.
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

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 1, characters 16-28:
      1 | const [b, _] = [Option.unopt(Tezos.create_ticket("one", 10 as nat)), 1];
                          ^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 1, characters 29-48:
      1 | const [b, _] = [Option.unopt(Tezos.create_ticket("one", 10 as nat)), 1];
                                       ^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 7, characters 13-25:
      6 | const main = (_p: unit, _s: storage): [list<operation>, storage] =>
      7 |   [list([]), Option.unopt(Tezos.join_tickets([b, b]))];
                       ^^^^^^^^^^^^
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 7, characters 26-44:
      6 | const main = (_p: unit, _s: storage): [list<operation>, storage] =>
      7 |   [list([]), Option.unopt(Tezos.join_tickets([b, b]))];
                                    ^^^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 1, characters 7-8:
      1 | const [b, _] = [Option.unopt(Tezos.create_ticket("one", 10 as nat)), 1];
                 ^
      2 |
    :
    Warning: variable cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 1, characters 16-28:
      1 | const [b, _] = [Option.unopt(Tezos.create_ticket("one", 10 as nat)), 1];
                          ^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 1, characters 29-48:
      1 | const [b, _] = [Option.unopt(Tezos.create_ticket("one", 10 as nat)), 1];
                                       ^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 7, characters 13-25:
      6 | const main = (_p: unit, _s: storage): [list<operation>, storage] =>
      7 |   [list([]), Option.unopt(Tezos.join_tickets([b, b]))];
                       ^^^^^^^^^^^^
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 7, characters 26-44:
      6 | const main = (_p: unit, _s: storage): [list<operation>, storage] =>
      7 |   [list([]), Option.unopt(Tezos.join_tickets([b, b]))];
                                    ^^^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.
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
