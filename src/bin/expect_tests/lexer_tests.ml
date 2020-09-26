open Cli_expect

let%expect_test _ =
    run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.ligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/broken_string.ligo", line 1, characters 18-19:[0m
  1 | const a: string = [1m[31m"[0mbroken
  2 | over

[1m[31mError[0m: The string starting here is interrupted by a line break.
Hint: Remove the break, close the string before or insert a backslash.
 |} ];

    run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.mligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/broken_string.mligo", line 1, characters 8-9:[0m
  1 | let a = [1m[31m"[0mbroken
  2 | over

[1m[31mError[0m: The string starting here is interrupted by a line break.
Hint: Remove the break, close the string before or insert a backslash.
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.religo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/broken_string.religo", line 1, characters 8-9:[0m
  1 | let a = [1m[31m"[0mbroken
  2 | over

[1m[31mError[0m: The string starting here is interrupted by a line break.
Hint: Remove the break, close the string before or insert a backslash.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/negative_byte_sequence.ligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/negative_byte_sequence.ligo", line 1, characters 18-31:[0m
  1 | const a: string = [1m[31m- (**) 0x2222[0m

[1m[31mError[0m: Negative byte sequence.
Hint: Remove the leading minus sign.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/negative_byte_sequence.mligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/negative_byte_sequence.mligo", line 1, characters 8-21:[0m
  1 | let a = [1m[31m- (**) 0x2222[0m

[1m[31mError[0m: Negative byte sequence.
Hint: Remove the leading minus sign.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/negative_byte_sequence.religo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/negative_byte_sequence.religo", line 1, characters 8-21:[0m
  1 | let a = [1m[31m- /**/ 0x2222[0m;

[1m[31mError[0m: Negative byte sequence.
Hint: Remove the leading minus sign.
 |} ];

(*
 run_ligo_bad [ "compile-contract" ; "../../test/lexer/reserved_name.ligo" ; "main" ] ;
  [%expect {|
ligo: : Lexical error in file "reserved_name.ligo", line 1, characters 4-13:
      Reserved name: "arguments".
      Hint: Change the name.
       {}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/introduction
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];
 *)

run_ligo_bad [ "compile-contract" ; "../../test/lexer/reserved_name.religo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/reserved_name.religo", line 1, characters 4-7:[0m
  1 | let [1m[31mend[0m = 1;

[1m[31mError[0m: Reserved name: "end".
Hint: Change the name.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/reserved_name.mligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/reserved_name.mligo", line 1, characters 4-10:[0m
  1 | let [1m[31mobject[0m = 1;

[1m[31mError[0m: Reserved name: "object".
Hint: Change the name.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.ligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/unexpected_character.ligo", line 1, characters 18-19:[0m
  1 | const x: string = [1m[31mï[0m¿½ï¿½ï¿½;

[1m[31mError[0m: Unexpected character '\239'.
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.mligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/unexpected_character.mligo", line 1, characters 8-9:[0m
  1 | let x = [1m[31mï[0m¿½ï¿½ï¿½;

[1m[31mError[0m: Unexpected character '\239'.
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.religo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/unexpected_character.religo", line 1, characters 8-9:[0m
  1 | let x = [1m[31mï[0m¿½ï¿½ï¿½;

[1m[31mError[0m: Unexpected character '\239'.
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unterminated_comment.mligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/unterminated_comment.mligo", line 1, characters 0-2:[0m
  1 | [1m[31m(*[0m not closed

[1m[31mError[0m: Unterminated comment.
Hint: Close with "*)".
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_symbol.ligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/invalid_symbol.ligo", line 1, characters 17-20:[0m
  1 | const b: int = 1 [1m[31m...[0m 10;

[1m[31mError[0m: Invalid symbol.
Hint: Check the LIGO syntax you use.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_symbol.mligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/invalid_symbol.mligo", line 1, characters 10-13:[0m
  1 | let b = 1 [1m[31m...[0m 10;

[1m[31mError[0m: Invalid symbol.
Hint: Check the LIGO syntax you use.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_symbol.religo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/invalid_symbol.religo", line 1, characters 10-11:[0m
  1 | let b = 1 [1m[31m#[0m 10;

[1m[31mError[0m: Invalid symbol.
Hint: Check the LIGO syntax you use.
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/missing_break.ligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/missing_break.ligo", line 1, characters 18-18:[0m
  1 | const a: int = 300[1m[31m[0mzennies;

[1m[31mError[0m: Missing break.
Hint: Insert some space.
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/missing_break.mligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/missing_break.mligo", line 1, characters 11-11:[0m
  1 | let a = 300[1m[31m[0mzennies;

[1m[31mError[0m: Missing break.
Hint: Insert some space.
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/missing_break.religo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/missing_break.religo", line 1, characters 11-11:[0m
  1 | let a = 300[1m[31m[0mzennies;

[1m[31mError[0m: Missing break.
Hint: Insert some space.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_character_in_string.ligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/invalid_character_in_string.ligo", line 1, characters 19-20:[0m
  1 | const z: string = "[1m[31m	[0m";

[1m[31mError[0m: Invalid character in string.
Hint: Remove or replace the character.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_character_in_string.mligo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/invalid_character_in_string.mligo", line 1, characters 9-10:[0m
  1 | let z = "[1m[31m	[0m";

[1m[31mError[0m: Invalid character in string.
Hint: Remove or replace the character.
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_character_in_string.religo" ; "main" ] ;
  [%expect {|
[1mFile "../../test/lexer/invalid_character_in_string.religo", line 1, characters 9-10:[0m
  1 | let z = "[1m[31m	[0m";

[1m[31mError[0m: Invalid character in string.
Hint: Remove or replace the character.
 |} ]
