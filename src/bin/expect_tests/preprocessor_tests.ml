open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/dangling_elif.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/dangling_elif.ligo", line 1, characters 0-5:
  1 | #elif a
Dangling #elif directive.
Hint: Remove it or add an #if before.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/dangling_else.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/dangling_else.ligo", line 1, characters 0-5:
  1 | #else
Directive #else without #if.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/dangling_endif.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/dangling_endif.ligo", line 1, characters 0-6:
  1 | #endif
Dangling #endif directive.
Hint: Remove it or add an #if before.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/elif_follows_else.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/elif_follows_else.ligo", line 3, characters 0-5:
  2 | #else
  3 | #elif b
Directive #elif found in a clause #else.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/else_follows_else.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/else_follows_else.ligo", line 3, characters 0-5:
  2 | #else
  3 | #else
Directive #else found in a clause #else.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/error_directive1.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/error_directive1.ligo", line 1, characters 0-6:
  1 | #error
Directive #error reached.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/error_directive2.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/error_directive2.ligo", line 1, characters 7-21:
  1 | #error Stopping here.
  2 | This should not be copied to the output
Stopping here.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/file_not_found.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/file_not_found.ligo", line 1, characters 9-24:
  1 | #include "foobarbaz.txt"
File "foobarbaz.txt" not found.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/if_follows_elif.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/if_follows_elif.ligo", line 3, characters 0-3:
  2 | #elif b
  3 | #if c
Directive #if found in a clause #elif.
|test}];
  run_ligo_bad
    [ "compile"; "contract"; "../../test/preprocessor/invalid_character_in_string.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/invalid_character_in_string.ligo", line 1, characters 27-28:
  1 | "containing a tabulation ->\t<-"
Invalid character "\\t" in string.
Hint: Use non-control characters 7-bit ASCII.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/invalid_character.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/invalid_character.ligo", line 1, characters 4-5:
  1 | #if $
  2 | #endif
Invalid character '$' (36).
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/invalid_symbol.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/invalid_symbol.ligo", line 1, characters 8-9:
  1 | #define _
Invalid symbol.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/missing_endif.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/missing_endif.ligo", line 2, character 0:
  1 | #if false
Missing #endif directive.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/missing_filename.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/missing_filename.ligo", line 1, characters 7-8:
  1 | #import\n
File name expected in a string literal.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/missing_module.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/missing_module.ligo", line 1, characters 24-25:
  1 | #import "included.ligo" M
Module name expected in a string literal.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/missing_space.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/missing_space.ligo", line 1, character 7:
  1 | #defineC
At least a space character is expected.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/newline_in_string.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/newline_in_string.ligo", line 1, characters 0-1:
  1 | "broken string
A string cannot be interrupted by a line break.
Hint: Remove it or close the string before.
|test}];
  run_ligo_bad [ "compile"; "contract"; "../../test/preprocessor/parse_error.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/parse_error.ligo", line 1, characters 6-7:
  1 | #if A ! = B
  2 | #endif
Parse error in Boolean expression.
|test}];
  run_ligo_bad
    [ "compile"; "contract"; "../../test/preprocessor/unexpected_argument.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/unexpected_argument.ligo", line 1, characters 25-26:
  1 | #include "included.ligo" "M" C
Unexpected argument.
Hint: Remove it.
|test}];
  run_ligo_bad
    [ "compile"; "contract"; "../../test/preprocessor/unterminated_comment.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/unterminated_comment.ligo", line 1, characters 0-2:
  1 | (* Unterminated
  2 |    comment
The comment starting here is not closed.
Hint: Close it with "*)".
|test}];
  run_ligo_bad
    [ "compile"; "contract"; "../../test/preprocessor/unterminated_string.ligo" ];
  [%expect
    {test|
File "../../test/preprocessor/unterminated_string.ligo", line 1, characters 0-1:
  1 | "open string
The string starting here is not closed.
Hint: Close it with "\"".
|test}]
