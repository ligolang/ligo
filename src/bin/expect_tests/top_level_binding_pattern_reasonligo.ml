open Cli_expect

(* Testing *)

let test_ file = test ("top_level_patterns/interpreter/" ^ file)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "reasonligo/nested_record.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "reasonligo/nested_tuple.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "reasonligo/record_tuple.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "reasonligo/tuple_record.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "reasonligo/record.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "reasonligo/tuple.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* Negative - linearity *)

let contract file = test ("top_level_patterns/negative/" ^ file)

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; contract "reasonligo/nested_record.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/nested_record.religo", line 8, character 4 to line 11, character 5:
      7 |         }
      8 | let { a : { c : c1 , d : d1 , e : e1 }
      9 |     , b : { c : c2 , d : d2 , e : e2 }
     10 |     , c : { c : c3 , d : c1 , e : e3 }
     11 |     } = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; contract "reasonligo/nested_tuple.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/nested_tuple.religo", line 2, characters 5-45:
      1 | let r = ((1n, 1, "H"), (2n, 2, "E"), (3n, 3, "Hello"))
      2 | let ((a1, a2, a3), (b1, a2, b3), (c1, c2, c3)) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "reasonligo/record.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/record.religo", line 4, characters 4-21:
      3 | let r = { a : 1n , b : 1 , c : "Hello" }
      4 | let { a , b : a , c } = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "reasonligo/tuple.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/tuple.religo", line 2, characters 5-12:
      1 | let r = (1n, 1, "Hello")
      2 | let (a, a, c) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; contract "reasonligo/record_tuple.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/record_tuple.religo", line 10, character 4 to line 13, character 5:
      9 |         }
     10 | let { a : (a1, a2, a3)
     11 |     , b : (b1, a2, b3)
     12 |     , c : (c1, c2, c3)
     13 |     } = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; contract "reasonligo/tuple_record.religo" ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/tuple_record.religo", line 7, character 6 to line 9, character 34:
      6 |         )
      7 | let ( { a : a1 , b : b1 , c : c1 }
      8 |     , { a : a2 , b : b2 , c : a2 }
      9 |     , { a : a3 , b : b3 , c : c3 }
     10 |     ) = r

    Repeated variable in pattern.
    Hint: Change the name. |}]

(* Negative - much use *)

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "reasonligo/ticket_record.religo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/ticket_record.religo", line 3, characters 6-7:
      2 |
      3 | let { b } = { b : Tezos.create_ticket ("one", 10n) }
      4 |
    :
    Warning: variable "b" cannot be used more than once.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/ticket_record.religo", line 3, characters 6-7:
      2 |
      3 | let { b } = { b : Tezos.create_ticket ("one", 10n) }
      4 |
    :
    Warning: variable "b" cannot be used more than once.
    { parameter unit ;
      storage (ticket string) ;
      code { DROP ;
             PUSH nat 10 ;
             PUSH string "one" ;
             TICKET ;
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
    ; contract "reasonligo/ticket_tuple.religo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/ticket_tuple.religo", line 1, characters 5-6:
      1 | let (b, _) = (Tezos.create_ticket ("one", 10n), 1)
      2 |
    :
    Warning: variable "b" cannot be used more than once.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    File "../../test/contracts/top_level_patterns/negative/reasonligo/ticket_tuple.religo", line 1, characters 5-6:
      1 | let (b, _) = (Tezos.create_ticket ("one", 10n), 1)
      2 |
    :
    Warning: variable "b" cannot be used more than once.
    { parameter unit ;
      storage (ticket string) ;
      code { DROP ;
             PUSH int 1 ;
             PUSH nat 10 ;
             PUSH string "one" ;
             TICKET ;
             SWAP ;
             DROP ;
             DUP ;
             PAIR ;
             JOIN_TICKETS ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             NIL operation ;
             PAIR } } |}]