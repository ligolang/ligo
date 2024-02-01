open Cli_expect

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "match_with_block()"
    ; "--init-file"
    ; test "match_with_block.jsligo"
    ];
  [%expect {|
    2 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match1.jsligo"; "--test" ];
  [%expect
    {|
    File "../../test/contracts/negative/pattern_match1.jsligo", line 2, character 9 to line 4, character 3:
      1 | let test_foo = (x : test_exec_result) : string => {
      2 |   return match(x) {
                   ^^^^^^^^^^
      3 |     when(Fail(_)): ""
          ^^^^^^^^^^^^^^^^^^^^^
      4 |   };
          ^^^
      5 | }

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - Success _ |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match2.jsligo"; "--test" ];
  [%expect
    {|
    File "../../test/contracts/negative/pattern_match2.jsligo", line 3, characters 9-18:
      2 |   match(x) {
      3 |     when(Success()): "";
                   ^^^^^^^^^
      4 |     when(Fail(_)): ""

    Pattern not of the expected type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match5.jsligo"; "--test" ];
  [%expect
    {|
    File "../../test/contracts/negative/pattern_match5.jsligo", line 2, character 2 to line 5, character 3:
      1 | let test_foo = (x : test_exec_result) : string => {
      2 |   match(x) {
            ^^^^^^^^^^
      3 |     when(Success(x, y)): "";
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |     when(Fail(_)): ""
          ^^^^^^^^^^^^^^^^^^^^^
      5 |   };
          ^^^
      6 | }

    Can not unify the types "( ^a * ^b )" and "nat".
    Type "( ^a * ^b )" is not compatible with type "nat".
    Hint: "^a", "^b" represent placeholder type(s). |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match3.jsligo"; "--test" ];
  [%expect
    {|
    File "../../test/contracts/negative/pattern_match3.jsligo", line 4, characters 9-19:
      3 |     when(Success(_)): "";
      4 |     when(Failure(_)): ""
                   ^^^^^^^^^^
      5 |   };

    Pattern not of the expected type "test_exec_result". |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match6.jsligo" ];
  [%expect
    {|
  File "../../test/contracts/negative/pattern_match6.jsligo", line 7, character 20 to line 10, character 9:
    6 |     return match (state) {
    7 |         when(S1()): match (action) {
                            ^^^^^^^^^^^^^^^^
    8 |             when(A()): S1();
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    9 |             when(B()): S2()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
   10 |         };
        ^^^^^^^^^
   11 |         when(S2()): match (action) {

  Error : this pattern-matching is not exhaustive.
  Here are examples of cases that are not matched:
  - C |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match7.jsligo"; "--no-color" ];
  [%expect
    {|
    File "../../test/contracts/negative/pattern_match7.jsligo", line 1, characters 47-60:
      1 | let foo = ([a,b,c,d] : [int,int,int]) : int => a + b + c + d;
                                                         ^^^^^^^^^^^^^

    Can not unify the types "( ^a * ^b * ^c * ^d )" and "( int * int * int )".
    Type "( ^a * ^b * ^c * ^d )" is not compatible with type "( int * int * int )".
    Difference between the types:
    - ^a
    + int
    - ^b
    + int
    - ^c
    + int
    - ^d
    Hint: "^a", "^b", "^c", "^d" represent placeholder type(s). |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2(Nil(), Nil())"
    ; "--init-file"
    ; test "/deep_pattern_matching/pm_test.jsligo"
    ];
  [%expect {|
    1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2(Nil(), Cons([1,2]))"
    ; "--init-file"
    ; test "/deep_pattern_matching/pm_test.jsligo"
    ];
  [%expect {|
    3 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2(Cons([1,2]), Cons([1,2]))"
    ; "--init-file"
    ; test "/deep_pattern_matching/pm_test.jsligo"
    ];
  [%expect {|
    6 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2(Cons([1,2]), Nil())"
    ; "--init-file"
    ; test "/deep_pattern_matching/pm_test.jsligo"
    ];
  [%expect {|
    7 |}]
