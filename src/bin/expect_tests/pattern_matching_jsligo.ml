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
File "../../test/contracts/negative/pattern_match1.jsligo", line 2, characters 2-34:
  1 | const test_foo = (x : test_exec_result) : string =>
  2 |   $match(x, { "Fail": (_) => "" });
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Error : this pattern-matching is not exhaustive.
Here are examples of cases that are not matched:
- Success _
 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match2.jsligo"; "--test" ];
  [%expect
    {|
File "../../test/contracts/negative/pattern_match2.jsligo", line 3, characters 4-13:
  2 |   $match(x, {
  3 |     "Success": () => "",
          ^^^^^^^^^
  4 |     "Fail": (_) => ""

Pattern not of the expected type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match5.jsligo"; "--test" ];
  [%expect
    {|
File "../../test/contracts/negative/pattern_match5.jsligo", line 2, character 2 to line 5, character 4:
  1 | const test_foo = (x : test_exec_result) : string =>
  2 |   $match(x, {
        ^^^^^^^^^^^
  3 |     "Success": ([x, y]) => "",
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  4 |     "Fail": (_) => ""
      ^^^^^^^^^^^^^^^^^^^^^
  5 |   });
      ^^^^

Can not unify the types "( ^a * ^b )" and "nat".
Type "( ^a * ^b )" is not compatible with type "nat".
Hint: "^a", "^b" represent placeholder type(s). |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match3.jsligo"; "--test" ];
  [%expect
    {|
File "../../test/contracts/negative/pattern_match3.jsligo", line 4, characters 4-17:
  3 |     "Success": (_) => "",
  4 |     "Failure": (_) => ""
          ^^^^^^^^^^^^^
  5 |   });

Pattern not of the expected type "test_exec_result". |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match6.jsligo" ];
  [%expect
    {|
File "../../test/contracts/negative/pattern_match6.jsligo", line 6, character 16 to line 9, character 18:
  5 |   $match(state, {
  6 |     "S1": () => $match(action, {
                      ^^^^^^^^^^^^^^^^
  7 |                   "A": () => ["S1" as "S1"],
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  8 |                   "B": () => ["S2" as "S2"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  9 |                 }),
      ^^^^^^^^^^^^^^^^^^
 10 |     "S2": () => $match(action, {

Error : this pattern-matching is not exhaustive.
Here are examples of cases that are not matched:
- C |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pattern_match7.jsligo"; "--no-color" ];
  [%expect
    {|
File "../../test/contracts/negative/pattern_match7.jsligo", line 1, characters 53-66:
  1 | const foo = ([a, b, c, d]: [int, int, int]) : int => a + b + c + d;
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
    ; "t2([\"Nil\" as \"Nil\"], [\"Nil\" as \"Nil\"])"
    ; "--init-file"
    ; test "/deep_pattern_matching/pm_test.jsligo"
    ];
  [%expect {|
    1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2([\"Nil\" as \"Nil\"], [\"Cons\" as \"Cons\", [1, 2]])"
    ; "--init-file"
    ; test "/deep_pattern_matching/pm_test.jsligo"
    ];
  [%expect {|
    3 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2([\"Cons\" as \"Cons\", [1, 2]], [\"Cons\" as \"Cons\", [1, 2]])"
    ; "--init-file"
    ; test "/deep_pattern_matching/pm_test.jsligo"
    ];
  [%expect {|
    6 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2([\"Cons\" as \"Cons\", [1, 2]], [\"Nil\" as \"Nil\"])"
    ; "--init-file"
    ; test "/deep_pattern_matching/pm_test.jsligo"
    ];
  [%expect {|
    7 |}]
