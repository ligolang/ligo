open Cli_expect

let contract file = test ("top_level_patterns/contracts/" ^ file)

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; contract "cameligo/nested_record.mligo" ];
  [%expect {| 208 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; contract "cameligo/nested_tuple.mligo" ];
  [%expect {| 216 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; contract "cameligo/record_tuple.mligo" ];
  [%expect {| 216 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; contract "cameligo/record.mligo" ];
  [%expect {| 104 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; contract "cameligo/ticket_record.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 4, characters 12-24:
      3 | let { a = a1 ; b = b1 ; c = c1 }
      4 |     = { a = Option.unopt (Tezos.create_ticket 1 10n)
                      ^^^^^^^^^^^^
      5 |       ; b = Option.unopt (Tezos.create_ticket "one" 10n)
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 4, characters 26-45:
      3 | let { a = a1 ; b = b1 ; c = c1 }
      4 |     = { a = Option.unopt (Tezos.create_ticket 1 10n)
                                    ^^^^^^^^^^^^^^^^^^^
      5 |       ; b = Option.unopt (Tezos.create_ticket "one" 10n)
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 5, characters 12-24:
      4 |     = { a = Option.unopt (Tezos.create_ticket 1 10n)
      5 |       ; b = Option.unopt (Tezos.create_ticket "one" 10n)
                      ^^^^^^^^^^^^
      6 |       ; c = Option.unopt (Tezos.create_ticket 1n 10n)
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 5, characters 26-45:
      4 |     = { a = Option.unopt (Tezos.create_ticket 1 10n)
      5 |       ; b = Option.unopt (Tezos.create_ticket "one" 10n)
                                    ^^^^^^^^^^^^^^^^^^^
      6 |       ; c = Option.unopt (Tezos.create_ticket 1n 10n)
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 6, characters 12-24:
      5 |       ; b = Option.unopt (Tezos.create_ticket "one" 10n)
      6 |       ; c = Option.unopt (Tezos.create_ticket 1n 10n)
                      ^^^^^^^^^^^^
      7 |       }
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 6, characters 26-45:
      5 |       ; b = Option.unopt (Tezos.create_ticket "one" 10n)
      6 |       ; c = Option.unopt (Tezos.create_ticket 1n 10n)
                                    ^^^^^^^^^^^^^^^^^^^
      7 |       }
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 10, characters 12-24:
      9 | let { a = a2 ; c = c2 ; b = b2 }
     10 |     = { a = Option.unopt (Tezos.create_ticket 2 10n)
                      ^^^^^^^^^^^^
     11 |       ; b = Option.unopt (Tezos.create_ticket "TWO" 10n)
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 10, characters 26-45:
      9 | let { a = a2 ; c = c2 ; b = b2 }
     10 |     = { a = Option.unopt (Tezos.create_ticket 2 10n)
                                    ^^^^^^^^^^^^^^^^^^^
     11 |       ; b = Option.unopt (Tezos.create_ticket "TWO" 10n)
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 11, characters 12-24:
     10 |     = { a = Option.unopt (Tezos.create_ticket 2 10n)
     11 |       ; b = Option.unopt (Tezos.create_ticket "TWO" 10n)
                      ^^^^^^^^^^^^
     12 |       ; c = Option.unopt (Tezos.create_ticket 3n 10n)
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 11, characters 26-45:
     10 |     = { a = Option.unopt (Tezos.create_ticket 2 10n)
     11 |       ; b = Option.unopt (Tezos.create_ticket "TWO" 10n)
                                    ^^^^^^^^^^^^^^^^^^^
     12 |       ; c = Option.unopt (Tezos.create_ticket 3n 10n)
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 12, characters 12-24:
     11 |       ; b = Option.unopt (Tezos.create_ticket "TWO" 10n)
     12 |       ; c = Option.unopt (Tezos.create_ticket 3n 10n)
                      ^^^^^^^^^^^^
     13 |       }
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 12, characters 26-45:
     11 |       ; b = Option.unopt (Tezos.create_ticket "TWO" 10n)
     12 |       ; c = Option.unopt (Tezos.create_ticket 3n 10n)
                                    ^^^^^^^^^^^^^^^^^^^
     13 |       }
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 20, characters 13-25:
     19 |   = [],
     20 |     (let a = Option.unopt (Tezos.join_tickets (a1, a2)) in
                       ^^^^^^^^^^^^
     21 |     let b  = Option.unopt (Tezos.join_tickets (b1, b2)) in
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 20, characters 27-45:
     19 |   = [],
     20 |     (let a = Option.unopt (Tezos.join_tickets (a1, a2)) in
                                     ^^^^^^^^^^^^^^^^^^
     21 |     let b  = Option.unopt (Tezos.join_tickets (b1, b2)) in
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 21, characters 13-25:
     20 |     (let a = Option.unopt (Tezos.join_tickets (a1, a2)) in
     21 |     let b  = Option.unopt (Tezos.join_tickets (b1, b2)) in
                       ^^^^^^^^^^^^
     22 |     let c  = Option.unopt (Tezos.join_tickets (c1, c2)) in
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 21, characters 27-45:
     20 |     (let a = Option.unopt (Tezos.join_tickets (a1, a2)) in
     21 |     let b  = Option.unopt (Tezos.join_tickets (b1, b2)) in
                                     ^^^^^^^^^^^^^^^^^^
     22 |     let c  = Option.unopt (Tezos.join_tickets (c1, c2)) in
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 22, characters 13-25:
     21 |     let b  = Option.unopt (Tezos.join_tickets (b1, b2)) in
     22 |     let c  = Option.unopt (Tezos.join_tickets (c1, c2)) in
                       ^^^^^^^^^^^^
     23 |     (a, b, c))
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_record.mligo", line 22, characters 27-45:
     21 |     let b  = Option.unopt (Tezos.join_tickets (b1, b2)) in
     22 |     let c  = Option.unopt (Tezos.join_tickets (c1, c2)) in
                                     ^^^^^^^^^^^^^^^^^^
     23 |     (a, b, c))
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    517 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; contract "cameligo/ticket_tuple.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 2, characters 6-18:
      1 | let (a1, a2, a3)
      2 |   = ( Option.unopt (Tezos.create_ticket 1 10n)
                ^^^^^^^^^^^^
      3 |     , Option.unopt (Tezos.create_ticket "one" 10n)
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 2, characters 20-39:
      1 | let (a1, a2, a3)
      2 |   = ( Option.unopt (Tezos.create_ticket 1 10n)
                              ^^^^^^^^^^^^^^^^^^^
      3 |     , Option.unopt (Tezos.create_ticket "one" 10n)
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 3, characters 6-18:
      2 |   = ( Option.unopt (Tezos.create_ticket 1 10n)
      3 |     , Option.unopt (Tezos.create_ticket "one" 10n)
                ^^^^^^^^^^^^
      4 |     , Option.unopt (Tezos.create_ticket 1n 10n)
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 3, characters 20-39:
      2 |   = ( Option.unopt (Tezos.create_ticket 1 10n)
      3 |     , Option.unopt (Tezos.create_ticket "one" 10n)
                              ^^^^^^^^^^^^^^^^^^^
      4 |     , Option.unopt (Tezos.create_ticket 1n 10n)
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 4, characters 6-18:
      3 |     , Option.unopt (Tezos.create_ticket "one" 10n)
      4 |     , Option.unopt (Tezos.create_ticket 1n 10n)
                ^^^^^^^^^^^^
      5 |     )
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 4, characters 20-39:
      3 |     , Option.unopt (Tezos.create_ticket "one" 10n)
      4 |     , Option.unopt (Tezos.create_ticket 1n 10n)
                              ^^^^^^^^^^^^^^^^^^^
      5 |     )
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 8, characters 6-18:
      7 | let (b1, b2, b3)
      8 |   = ( Option.unopt (Tezos.create_ticket 2 10n)
                ^^^^^^^^^^^^
      9 |     , Option.unopt (Tezos.create_ticket "TWO" 10n)
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 8, characters 20-39:
      7 | let (b1, b2, b3)
      8 |   = ( Option.unopt (Tezos.create_ticket 2 10n)
                              ^^^^^^^^^^^^^^^^^^^
      9 |     , Option.unopt (Tezos.create_ticket "TWO" 10n)
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 9, characters 6-18:
      8 |   = ( Option.unopt (Tezos.create_ticket 2 10n)
      9 |     , Option.unopt (Tezos.create_ticket "TWO" 10n)
                ^^^^^^^^^^^^
     10 |     , Option.unopt (Tezos.create_ticket 3n 10n)
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 9, characters 20-39:
      8 |   = ( Option.unopt (Tezos.create_ticket 2 10n)
      9 |     , Option.unopt (Tezos.create_ticket "TWO" 10n)
                              ^^^^^^^^^^^^^^^^^^^
     10 |     , Option.unopt (Tezos.create_ticket 3n 10n)
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 10, characters 6-18:
      9 |     , Option.unopt (Tezos.create_ticket "TWO" 10n)
     10 |     , Option.unopt (Tezos.create_ticket 3n 10n)
                ^^^^^^^^^^^^
     11 |     )
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 10, characters 20-39:
      9 |     , Option.unopt (Tezos.create_ticket "TWO" 10n)
     10 |     , Option.unopt (Tezos.create_ticket 3n 10n)
                              ^^^^^^^^^^^^^^^^^^^
     11 |     )
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 18, characters 13-25:
     17 |   = [],
     18 |     (let a = Option.unopt (Tezos.join_tickets (a1, b1)) in
                       ^^^^^^^^^^^^
     19 |     let b  = Option.unopt (Tezos.join_tickets (a2, b2)) in
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 18, characters 27-45:
     17 |   = [],
     18 |     (let a = Option.unopt (Tezos.join_tickets (a1, b1)) in
                                     ^^^^^^^^^^^^^^^^^^
     19 |     let b  = Option.unopt (Tezos.join_tickets (a2, b2)) in
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 19, characters 13-25:
     18 |     (let a = Option.unopt (Tezos.join_tickets (a1, b1)) in
     19 |     let b  = Option.unopt (Tezos.join_tickets (a2, b2)) in
                       ^^^^^^^^^^^^
     20 |     let c  = Option.unopt (Tezos.join_tickets (a3, b3)) in
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 19, characters 27-45:
     18 |     (let a = Option.unopt (Tezos.join_tickets (a1, b1)) in
     19 |     let b  = Option.unopt (Tezos.join_tickets (a2, b2)) in
                                     ^^^^^^^^^^^^^^^^^^
     20 |     let c  = Option.unopt (Tezos.join_tickets (a3, b3)) in
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 20, characters 13-25:
     19 |     let b  = Option.unopt (Tezos.join_tickets (a2, b2)) in
     20 |     let c  = Option.unopt (Tezos.join_tickets (a3, b3)) in
                       ^^^^^^^^^^^^
     21 |     (a, b, c))
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/contracts/cameligo/ticket_tuple.mligo", line 20, characters 27-45:
     19 |     let b  = Option.unopt (Tezos.join_tickets (a2, b2)) in
     20 |     let c  = Option.unopt (Tezos.join_tickets (a3, b3)) in
                                     ^^^^^^^^^^^^^^^^^^
     21 |     (a, b, c))
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    517 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; contract "cameligo/tuple_record.mligo" ];
  [%expect {| 208 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; contract "cameligo/tuple.mligo" ];
  [%expect {| 104 bytes |}]

let%expect_test _ =
  run_ligo_good
    [ "info"; "measure-contract"; contract "cameligo/constr_tuple_destructuring.mligo" ];
  [%expect {| 58 bytes |}]

let%expect_test _ =
  run_ligo_good
    [ "info"; "measure-contract"; contract "cameligo/constr_record_destructuring.mligo" ];
  [%expect {| 58 bytes |}]

(* Testing *)

let test_ file = test ("top_level_patterns/interpreter/" ^ file)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "cameligo/nested_record.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_record.mligo", line 1, characters 9-30:
      1 | let () = Test.set_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_record.mligo", line 7, characters 13-21:
      6 | let f () =
      7 |     let () = Test.log "Once" in
                       ^^^^^^^^
      8 |     { a = { c = 1n ; d = 1 ; e = "H" }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_record.mligo", line 29, characters 8-14:
     28 |     begin
     29 |         assert ((c1 + c2 + c3) = (c4 + c5 + c6));
                  ^^^^^^
     30 |         assert ((d1 + d2 + d3) = (d4 + d5 + d6));
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_record.mligo", line 30, characters 8-14:
     29 |         assert ((c1 + c2 + c3) = (c4 + c5 + c6));
     30 |         assert ((d1 + d2 + d3) = (d4 + d5 + d6));
                  ^^^^^^
     31 |         assert ((e1 ^ e2 ^ e3) = (e4 ^ e5 ^ e6))
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_record.mligo", line 31, characters 8-14:
     30 |         assert ((d1 + d2 + d3) = (d4 + d5 + d6));
     31 |         assert ((e1 ^ e2 ^ e3) = (e4 ^ e5 ^ e6))
                  ^^^^^^
     32 |     end
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "cameligo/nested_tuple.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_tuple.mligo", line 1, characters 9-30:
      1 | let () = Test.set_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_tuple.mligo", line 4, characters 13-21:
      3 | let f () =
      4 |     let () = Test.log "Once" in
                       ^^^^^^^^
      5 |     ((1n, 1, "H"), (2n, 2, "E"), (3n, 3, "L"))
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_tuple.mligo", line 13, characters 8-14:
     12 |     begin
     13 |         assert ((a1 + b1 + c1) = (a4 + b4 + c4));
                  ^^^^^^
     14 |         assert ((a2 + b2 + c2) = (a5 + b5 + c5));
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_tuple.mligo", line 14, characters 8-14:
     13 |         assert ((a1 + b1 + c1) = (a4 + b4 + c4));
     14 |         assert ((a2 + b2 + c2) = (a5 + b5 + c5));
                  ^^^^^^
     15 |         assert ((a3 ^ b3 ^ c3) = (a6 ^ b6 ^ c6))
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/nested_tuple.mligo", line 15, characters 8-14:
     14 |         assert ((a2 + b2 + c2) = (a5 + b5 + c5));
     15 |         assert ((a3 ^ b3 ^ c3) = (a6 ^ b6 ^ c6))
                  ^^^^^^
     16 |     end
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "cameligo/record_tuple.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record_tuple.mligo", line 1, characters 9-30:
      1 | let () = Test.set_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record_tuple.mligo", line 10, characters 11-19:
      9 | let f () =
     10 |   let () = Test.log "Once" in
                     ^^^^^^^^
     11 |   { a = (1n     , 1 , "H")
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record_tuple.mligo", line 32, characters 4-10:
     31 |   begin
     32 |     assert ((a1 + b2 + c3) = (a4 + b5 + c6));
              ^^^^^^
     33 |     assert ((b1 + a2 + c2) = (a5 + b4 + c5));
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record_tuple.mligo", line 33, characters 4-10:
     32 |     assert ((a1 + b2 + c3) = (a4 + b5 + c6));
     33 |     assert ((b1 + a2 + c2) = (a5 + b4 + c5));
              ^^^^^^
     34 |     assert ((a3 ^ b3 ^ c1) = (a6 ^ b6 ^ c4))
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record_tuple.mligo", line 34, characters 4-10:
     33 |     assert ((b1 + a2 + c2) = (a5 + b4 + c5));
     34 |     assert ((a3 ^ b3 ^ c1) = (a6 ^ b6 ^ c4))
              ^^^^^^
     35 |   end
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "cameligo/tuple_record.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple_record.mligo", line 1, characters 9-30:
      1 | let () = Test.set_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple_record.mligo", line 6, characters 11-19:
      5 | let f () =
      6 |   let () = Test.log "Once" in
                     ^^^^^^^^
      7 |   ( { a = 1n ; b = 1 ; c = "H" }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple_record.mligo", line 26, characters 4-10:
     25 |   begin
     26 |     assert (a1 + a2 + a3 = a4 + a5 + a6);
              ^^^^^^
     27 |     assert (b1 + b2 + b3 = b4 + b5 + b6);
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple_record.mligo", line 27, characters 4-10:
     26 |     assert (a1 + a2 + a3 = a4 + a5 + a6);
     27 |     assert (b1 + b2 + b3 = b4 + b5 + b6);
              ^^^^^^
     28 |     assert (c1 ^ c2 ^ c3 = c4 ^ c5 ^ c6)
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple_record.mligo", line 28, characters 4-10:
     27 |     assert (b1 + b2 + b3 = b4 + b5 + b6);
     28 |     assert (c1 ^ c2 ^ c3 = c4 ^ c5 ^ c6)
              ^^^^^^
     29 |   end
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "cameligo/record.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record.mligo", line 1, characters 9-30:
      1 | let () = Test.set_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record.mligo", line 6, characters 11-19:
      5 | let f () =
      6 |   let () = Test.log "Once" in
                     ^^^^^^^^
      7 |   { a = 1n ; b = 1 ; c = "Hello" }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record.mligo", line 13, characters 4-10:
     12 |   begin
     13 |     assert (a = a1);
              ^^^^^^
     14 |     assert (b = b1);
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record.mligo", line 14, characters 4-10:
     13 |     assert (a = a1);
     14 |     assert (b = b1);
              ^^^^^^
     15 |     assert (c = c1)
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/record.mligo", line 15, characters 4-10:
     14 |     assert (b = b1);
     15 |     assert (c = c1)
              ^^^^^^
     16 |   end
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "cameligo/tuple.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple.mligo", line 1, characters 9-30:
      1 | let () = Test.set_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple.mligo", line 4, characters 11-19:
      3 | let f () =
      4 |   let () = Test.log "Once" in
                     ^^^^^^^^
      5 |   (1n, 1, "Hello")
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple.mligo", line 12, characters 4-10:
     11 |   begin
     12 |     assert (a = a1);
              ^^^^^^
     13 |     assert (b = b1);
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple.mligo", line 13, characters 4-10:
     12 |     assert (a = a1);
     13 |     assert (b = b1);
              ^^^^^^
     14 |     assert (c = c1)
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/tuple.mligo", line 14, characters 4-10:
     13 |     assert (b = b1);
     14 |     assert (c = c1)
              ^^^^^^
     15 |   end
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "cameligo/constr_tuple_destructuring.mligo" ];
  [%expect
    {|
      File "../../test/contracts/top_level_patterns/interpreter/cameligo/constr_tuple_destructuring.mligo", line 4, characters 11-19:
        3 | let f () =
        4 |   let () = Test.log "Once" in
                       ^^^^^^^^
        5 |   1, (Foo 2, "hey")
      :
      Warning: deprecated value.
      In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

      File "../../test/contracts/top_level_patterns/interpreter/cameligo/constr_tuple_destructuring.mligo", line 10, characters 2-8:
        9 | let test =
       10 |   assert ((a + x + String.length c) = 6)
              ^^^^^^
      :
      Warning: deprecated value.
      In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

      "Once"
      Everything at the top-level was executed.
      - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "cameligo/constr_record_destructuring.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/cameligo/constr_record_destructuring.mligo", line 5, characters 11-19:
      4 | let f () =
      5 |   let () = Test.log "Once" in
                     ^^^^^^^^
      6 |   { a = 1 ; b = Foo 2 ; c = "hey" }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/cameligo/constr_record_destructuring.mligo", line 11, characters 2-8:
     10 | let test =
     11 |   assert ((a + x + String.length c) = 6)
            ^^^^^^
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* Negative - linearity *)

let contract file = test ("top_level_patterns/negative/" ^ file)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-core"; contract "cameligo/inside_module.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/inside_module.mligo", line 2, characters 7-11:
      1 | module A = struct
      2 |   let (x, x) = (1, "string")
                 ^^^^
      3 | end

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-core"; contract "cameligo/inside_mod_in.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/inside_mod_in.mligo", line 3, characters 9-13:
      2 |   module A = struct
      3 |     let (x, x) = (1, "1")
                   ^^^^
      4 |   end in

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "cameligo/nested_record.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/nested_record.mligo", line 8, character 4 to line 11, character 5:
      7 |         }
      8 | let { a = { c = c1 ; d = d1 ; e = e1 }
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |     ; b = { c = c2 ; d = d2 ; e = e2 }
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |     ; c = { c = c3 ; d = c1 ; e = e3 }
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     11 |     } = r
          ^^^^^

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "cameligo/nested_tuple.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/nested_tuple.mligo", line 2, characters 5-45:
      1 | let r = ((1n, 1, "H"), (2n, 2, "E"), (3n, 3, "Hello"))
      2 | let ((a1, a2, a3), (b1, a2, b3), (c1, c2, c3)) = r
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "cameligo/record.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/record.mligo", line 4, characters 4-21:
      3 | let r = { a = 1n ; b = 1 ; c = "Hello" }
      4 | let { a ; b = a ; c } = r
              ^^^^^^^^^^^^^^^^^

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "cameligo/tuple.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/tuple.mligo", line 2, characters 5-12:
      1 | let r = (1n, 1, "Hello")
      2 | let (a, a, c) = r
               ^^^^^^^

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "cameligo/record_tuple.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/record_tuple.mligo", line 10, character 4 to line 13, character 5:
      9 |         }
     10 | let { a = (a1, a2, a3)
              ^^^^^^^^^^^^^^^^^^
     11 |     ; b = (b1, a2, b3)
          ^^^^^^^^^^^^^^^^^^^^^^
     12 |     ; c = (c1, c2, c3)
          ^^^^^^^^^^^^^^^^^^^^^^
     13 |     } = r
          ^^^^^

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "cameligo/tuple_record.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/tuple_record.mligo", line 7, character 6 to line 9, character 34:
      6 |         )
      7 | let ( { a = a1 ; b = b1 ; c = c1 }
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |     , { a = a2 ; b = b2 ; c = a2 }
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |     , { a = a3 ; b = b3 ; c = c3 }
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |     ) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

(* Negative - much use *)

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "cameligo/ticket_record.mligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 3, characters 5-6:
      2 |
      3 | let {b} = {b = Option.unopt (Tezos.create_ticket "one" 10n)}
               ^
      4 |
    :
    Warning: variable cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 3, characters 15-27:
      2 |
      3 | let {b} = {b = Option.unopt (Tezos.create_ticket "one" 10n)}
                         ^^^^^^^^^^^^
      4 |
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 3, characters 29-48:
      2 |
      3 | let {b} = {b = Option.unopt (Tezos.create_ticket "one" 10n)}
                                       ^^^^^^^^^^^^^^^^^^^
      4 |
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 9, characters 6-18:
      8 | let main (_ : unit) (_ : storage) : operation list * storage =
      9 |   [], Option.unopt (Tezos.join_tickets (b, b))
                ^^^^^^^^^^^^
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 9, characters 20-38:
      8 | let main (_ : unit) (_ : storage) : operation list * storage =
      9 |   [], Option.unopt (Tezos.join_tickets (b, b))
                              ^^^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 3, characters 5-6:
      2 |
      3 | let {b} = {b = Option.unopt (Tezos.create_ticket "one" 10n)}
               ^
      4 |
    :
    Warning: variable cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 3, characters 15-27:
      2 |
      3 | let {b} = {b = Option.unopt (Tezos.create_ticket "one" 10n)}
                         ^^^^^^^^^^^^
      4 |
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 3, characters 29-48:
      2 |
      3 | let {b} = {b = Option.unopt (Tezos.create_ticket "one" 10n)}
                                       ^^^^^^^^^^^^^^^^^^^
      4 |
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 9, characters 6-18:
      8 | let main (_ : unit) (_ : storage) : operation list * storage =
      9 |   [], Option.unopt (Tezos.join_tickets (b, b))
                ^^^^^^^^^^^^
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_record.mligo", line 9, characters 20-38:
      8 | let main (_ : unit) (_ : storage) : operation list * storage =
      9 |   [], Option.unopt (Tezos.join_tickets (b, b))
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
    ; contract "cameligo/ticket_tuple.mligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 1, characters 5-6:
      1 | let (b, _) = (Option.unopt (Tezos.create_ticket "one" 10n), 1)
               ^
      2 |
    :
    Warning: variable cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 1, characters 14-26:
      1 | let (b, _) = (Option.unopt (Tezos.create_ticket "one" 10n), 1)
                        ^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 1, characters 28-47:
      1 | let (b, _) = (Option.unopt (Tezos.create_ticket "one" 10n), 1)
                                      ^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 7, characters 6-18:
      6 | let main (_ : unit) (_ : storage) : operation list * storage =
      7 |   [], Option.unopt (Tezos.join_tickets (b, b))
                ^^^^^^^^^^^^
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 7, characters 20-38:
      6 | let main (_ : unit) (_ : storage) : operation list * storage =
      7 |   [], Option.unopt (Tezos.join_tickets (b, b))
                              ^^^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.join` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 1, characters 5-6:
      1 | let (b, _) = (Option.unopt (Tezos.create_ticket "one" 10n), 1)
               ^
      2 |
    :
    Warning: variable cannot be used more than once.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 1, characters 14-26:
      1 | let (b, _) = (Option.unopt (Tezos.create_ticket "one" 10n), 1)
                        ^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 1, characters 28-47:
      1 | let (b, _) = (Option.unopt (Tezos.create_ticket "one" 10n), 1)
                                      ^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.create` from `Tezos.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 7, characters 6-18:
      6 | let main (_ : unit) (_ : storage) : operation list * storage =
      7 |   [], Option.unopt (Tezos.join_tickets (b, b))
                ^^^^^^^^^^^^
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/top_level_patterns/negative/cameligo/ticket_tuple.mligo", line 7, characters 20-38:
      6 | let main (_ : unit) (_ : storage) : operation list * storage =
      7 |   [], Option.unopt (Tezos.join_tickets (b, b))
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

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "cameligo/constr_record_destructuring.mligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/constr_record_destructuring.mligo", line 4, characters 0-62:
      3 |
      4 | let { a ; b = (Foo x) ; c} = { a = 1 ; b = Foo 2 ; c = "hey" }
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - {
     a = _;
     b = Bar;
     c = _
    } |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "cameligo/constr_tuple_destructuring.mligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/constr_tuple_destructuring.mligo", line 3, characters 0-40:
      2 |
      3 | let (a,  (Foo x), c) = (1, Foo 2, "hey")
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (_, Bar, _) |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; contract "cameligo/constr_let_in.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/constr_let_in.mligo", line 4, character 2 to line 5, character 4:
      3 | let test =
      4 |   let B = B in
            ^^^^^^^^^^^^
      5 |   ()
          ^^^^

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - A |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; contract "cameligo/constr_let_in2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/cameligo/constr_let_in2.mligo", line 2, character 2 to line 3, character 4:
      1 | let test =
      2 |   let True = true in
            ^^^^^^^^^^^^^^^^^^
      3 |   42
          ^^^^

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - False |}]
