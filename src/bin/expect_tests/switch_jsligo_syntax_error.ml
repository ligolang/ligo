open Cli_expect

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/empty_switch.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/switch_jsligo/empty_switch.jsligo", line 4, characters 4-5:
      3 |     switch (n) {
      4 |     };
              ^
      5 |     output = output + "World";
    Ill-formed switch.
    At this point, one of the following is expected:
      * the keyword 'case' introducing a new case;
      * the keyword 'default' introducing the default case. |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/default_in_between.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/switch_jsligo/default_in_between.jsligo", line 10, characters 6-10:
      9 |         output = output + "###";
     10 |       case 3:
                ^^^^
     11 |         output = output + "@@@";
    Ill-formed switch.
    At this point, if the case is complete, one of the following is
    expected:
      * another case (starting by 'case' or 'default');
      * a closing brace '}' is no more cases. |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/more_than_one_default.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/switch_jsligo/more_than_one_default.jsligo", line 14, characters 6-13:
     13 |         output = output + "###";
     14 |       default:
                ^^^^^^^
     15 |         output = output + "***";
    Ill-formed switch.
    At this point, if the case is complete, one of the following is
    expected:
      * another case (starting by 'case' or 'default');
      * a closing brace '}' is no more cases. |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/break_outside_case1.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/switch_jsligo/break_outside_case1.jsligo", line 3, characters 4-9:
      2 |     let output = "Hello";
      3 |     break;
              ^^^^^
      4 |     return output;

    Break statement is not supported in that position |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/break_outside_case2.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/switch_jsligo/break_outside_case2.jsligo", line 4, characters 8-13:
      3 |     if (output == "") {
      4 |         break;
                  ^^^^^
      5 |     } else {

    Break statement is not supported in that position |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/break_outside_case3.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/switch_jsligo/break_outside_case3.jsligo", line 6, characters 16-21:
      5 |             if (output == "") {
      6 |                 break;
                          ^^^^^
      7 |             } else {

    Break statement is not supported in that position |}]

let%expect_test _ =
  run_ligo_bad
    [ "print"
    ; "ast-core"
    ; "../../test/contracts/negative/switch_jsligo/break_outside_case4.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/switch_jsligo/break_outside_case4.jsligo", line 7, characters 16-21:
      6 |                 output = output + "World";
      7 |                 break;
                          ^^^^^
      8 |             };

    Break statement is not supported in that position |}]
