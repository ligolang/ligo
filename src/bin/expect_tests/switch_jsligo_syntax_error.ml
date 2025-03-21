open Cli_expect

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/empty_switch.jsligo"
    ];
  [%expect
      {|
File "../../test/contracts/negative/switch_jsligo/empty_switch.jsligo", line 3, character 13 to line 4, character 3:
  2 |   let output = "Hello";
  3 |   switch (n) {
                   ^
  4 |   };
      ^^^
  5 |   output = output + "World";
Empty switches are not supported in JsLIGO. |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/more_than_one_default.jsligo"
    ];
  [%expect
    {|
File "../../test/contracts/negative/switch_jsligo/more_than_one_default.jsligo", line 9, characters 4-37:
  8 |     default: output = output + "###";
  9 |     default: output = output + "***";
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 10 |     };
Multiple default switch cases are not supported in JsLIGO. |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/break_outside_case1.jsligo"
    ];
  [%expect
    {|
File "../../test/contracts/negative/switch_jsligo/break_outside_case1.jsligo", line 3, characters 2-7:
  2 |   const output = "Hello";
  3 |   break;
        ^^^^^
  4 |   return output;

Break statement is not supported in that position |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/break_outside_case2.jsligo"
    ];
  [%expect
    {|
File "../../test/contracts/negative/switch_jsligo/break_outside_case2.jsligo", line 3, characters 20-25:
  2 |   let output = "Hello";
  3 |   if (output == "") break; else output = output + " World";
                          ^^^^^
  4 |   return output;

Break statement is not supported in that position |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/break_outside_case3.jsligo"
    ];
  [%expect
    {|
File "../../test/contracts/negative/switch_jsligo/break_outside_case3.jsligo", line 4, characters 30-35:
  3 |   switch (n) {
  4 |     case 1: if (output == "") break; else output = output + "World";
                                    ^^^^^
  5 |     case 2: output = output + "World";

Break statement is not supported in that position |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/break_outside_case4.jsligo"
    ];
  [%expect
    {|
File "../../test/contracts/negative/switch_jsligo/break_outside_case4.jsligo", line 4, characters 41-46:
  3 |   switch (n) {
  4 |     case 1: { output = output + "World"; break; };
                                               ^^^^^
  5 |     case 2: output = output + "World"; break;

Break statement is not supported in that position |}]
