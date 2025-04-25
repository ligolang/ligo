open Cli_expect

(* Testing *)

let test_ file = test ("top_level_patterns/interpreter/" ^ file)

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/nested_record.jsligo" ] ;
  [%expect{|
"Once"
Everything at the top-level was executed.
- test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "jsligo/nested_tuple.jsligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/record_tuple.jsligo" ] ;
  [%expect
    {|
"Once"
Everything at the top-level was executed.
- test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/tuple_record.jsligo" ] ;
  [%expect{|
"Once"
Everything at the top-level was executed.
- test exited with value (). |}]

 let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/record.jsligo" ] ;
  [%expect{|
"Once"
Everything at the top-level was executed.
- test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "jsligo/tuple.jsligo" ];
  [%expect
    {|
    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* Negative - linearity *)

let contract file = test ("top_level_patterns/negative/" ^ file)

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "jsligo/nested_tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/nested_tuple.jsligo", line 2, characters 26-28:
      1 | const r = [[1 as nat, 1, "H"], [2 as nat, 2, "E"], [3 as nat, 3, "Hello"]]
      2 | const [[a1, a2, a3], [b1, a2, b3], [c1, c2, c3]] = r
                                    ^^

    Duplicate identifier. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "jsligo/tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/tuple.jsligo", line 2, characters 10-11:
      1 | const r = [1 as nat, 1, "Hello"]
      2 | const [a, a, c] = r
                    ^

    Duplicate identifier. |}]

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
File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 5, characters 8-9:
  4 |
  5 | const { b } = {
              ^
  6 |   b: Option.value_with_error("option is None",
:
Warning: variable cannot be used more than once.

File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_record.jsligo", line 5, characters 8-9:
  4 |
  5 | const { b } = {
              ^
  6 |   b: Option.value_with_error("option is None",
:
Warning: variable cannot be used more than once.
{ parameter unit ;
  storage (ticket string) ;
  code { DROP ;
         PUSH nat 10 ;
         PUSH string "one" ;
         TICKET ;
         PUSH string "option is None" ;
         SWAP ;
         IF_NONE { FAILWITH } { SWAP ; DROP } ;
         DUP ;
         PAIR ;
         JOIN_TICKETS ;
         PUSH string "option is None" ;
         SWAP ;
         IF_NONE { FAILWITH } { SWAP ; DROP } ;
         NIL operation ;
         PAIR } }
|}]

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
File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 3, characters 7-8:
  2 |
  3 | const [b, _] =
             ^
  4 |       [Option.value_with_error("option is None",
:
Warning: variable cannot be used more than once.

File "../../test/contracts/top_level_patterns/negative/jsligo/ticket_tuple.jsligo", line 3, characters 7-8:
  2 |
  3 | const [b, _] =
             ^
  4 |       [Option.value_with_error("option is None",
:
Warning: variable cannot be used more than once.
{ parameter unit ;
  storage (ticket string) ;
  code { DROP ;
         PUSH int 1 ;
         PUSH nat 10 ;
         PUSH string "one" ;
         TICKET ;
         PUSH string "option is None" ;
         SWAP ;
         IF_NONE { FAILWITH } { SWAP ; DROP } ;
         SWAP ;
         DROP ;
         DUP ;
         PAIR ;
         JOIN_TICKETS ;
         PUSH string "option is None" ;
         SWAP ;
         IF_NONE { FAILWITH } { SWAP ; DROP } ;
         NIL operation ;
         PAIR } } |}]
