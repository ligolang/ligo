open Cli_expect

let gs s = "../../test/contracts/no_color_underline/" ^ s

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; gs "one_line_error.mligo"; "--no-color" ];
  [%expect
    {xxx|
    File "../../test/contracts/no_color_underline/one_line_error.mligo", line 2, characters 9-68:
      1 |
      2 | let main {| I should be underlined in error message with no-color |} blah-blah
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Ill-formed value declaration.
    At this point, one of the following is expected:
      * parameters as irrefutable patterns, e.g. variables, if defining a
        function;
      * the assignment symbol '=' followed by an expression;
      * a type annotation starting with a colon ':';
      * a comma ',' followed by another tuple component, if defining a
        tuple. |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; gs "two_lines_error.mligo"; "--no-color" ];
  [%expect
    {xxx|
    File "../../test/contracts/no_color_underline/two_lines_error.mligo", line 2, character 9 to line 3, character 32:
      1 |
      2 | let main {| I should be underlined in
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      3 |   error message with no-color |} blah-blah
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Ill-formed value declaration.
    At this point, one of the following is expected:
      * parameters as irrefutable patterns, e.g. variables, if defining a
        function;
      * the assignment symbol '=' followed by an expression;
      * a type annotation starting with a colon ':';
      * a comma ',' followed by another tuple component, if defining a
        tuple. |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; gs "several_lines_error.mligo"; "--no-color" ];
  [%expect
    {xxx|
    File "../../test/contracts/no_color_underline/several_lines_error.mligo", line 2, character 9 to line 5, character 15:
      1 |
      2 | let main {| I should be underlined in
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      3 |   error message with no-color and
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |   I am a long message spreading
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |   on 4 lines |} blah-blah
          ^^^^^^^^^^^^^^^
    Ill-formed value declaration.
    At this point, one of the following is expected:
      * parameters as irrefutable patterns, e.g. variables, if defining a
        function;
      * the assignment symbol '=' followed by an expression;
      * a type annotation starting with a colon ':';
      * a comma ',' followed by another tuple component, if defining a
        tuple. |xxx}]
