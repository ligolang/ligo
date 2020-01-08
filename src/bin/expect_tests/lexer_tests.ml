open Cli_expect

let%expect_test _ =
    run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.ligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: The string starting here is interrupted by a line break.
      Hint: Remove the break, close the string before or insert a backslash.
       {"parser_loc":"in file \"broken_string.ligo\", line 1, characters 18-19"}      


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

    run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: The string starting here is interrupted by a line break.
      Hint: Remove the break, close the string before or insert a backslash.
       {"parser_loc":"in file \"broken_string.mligo\", line 1, characters 8-9"}      


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: The string starting here is interrupted by a line break.
      Hint: Remove the break, close the string before or insert a backslash.
       {"parser_loc":"in file \"broken_string.religo\", line 1, characters 8-9"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/negative_byte_sequence.ligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Negative byte sequence.
      Hint: Remove the leading minus sign.
       {"parser_loc":"in file \"negative_byte_sequence.ligo\", line 1, characters 18-23"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/negative_byte_sequence.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Negative byte sequence.
      Hint: Remove the leading minus sign.
       {"parser_loc":"in file \"negative_byte_sequence.mligo\", line 1, characters 8-13"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/negative_byte_sequence.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Negative byte sequence.
      Hint: Remove the leading minus sign.
       {"parser_loc":"in file \"negative_byte_sequence.religo\", line 1, characters 8-13"}            


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/reserved_name.ligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Reserved name: args.
      Hint: Change the name.
       {"parser_loc":"in file \"reserved_name.ligo\", line 1, characters 4-8"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/reserved_name.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Reserved name: end.
      Hint: Change the name.
       {"parser_loc":"in file \"reserved_name.religo\", line 1, characters 4-7"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/reserved_name.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Reserved name: object.
      Hint: Change the name.
       {"parser_loc":"in file \"reserved_name.mligo\", line 1, characters 4-10"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.ligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Unexpected character '\239'.
       {"parser_loc":"in file \"unexpected_character.ligo\", line 1, characters 18-19"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Unexpected character '\239'.
       {"parser_loc":"in file \"unexpected_character.mligo\", line 1, characters 8-9"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Unexpected character '\239'.
       {"parser_loc":"in file \"unexpected_character.religo\", line 1, characters 8-9"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unterminated_comment.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Unterminated comment.
      Hint: Close with "*)".
       {"parser_loc":"in file \"unterminated_comment.mligo\", line 1, characters 0-2"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_symbol.ligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid symbol.
      Hint: Check the LIGO syntax you use.
       {"parser_loc":"in file \"invalid_symbol.ligo\", line 1, characters 17-20"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_symbol.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid symbol.
      Hint: Check the LIGO syntax you use.
       {"parser_loc":"in file \"invalid_symbol.mligo\", line 1, characters 10-13"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_symbol.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid symbol.
      Hint: Check the LIGO syntax you use.
       {"parser_loc":"in file \"invalid_symbol.religo\", line 1, characters 10-11"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/missing_break.ligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Missing break.
      Hint: Insert some space.
       {"parser_loc":"in file \"missing_break.ligo\", line 1, characters 18-18"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/missing_break.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Missing break.
      Hint: Insert some space.
       {"parser_loc":"in file \"missing_break.mligo\", line 1, characters 11-11"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/missing_break.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Missing break.
      Hint: Insert some space.
       {"parser_loc":"in file \"missing_break.religo\", line 1, characters 11-11"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_character_in_string.ligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid character in string.
      Hint: Remove or replace the character.
       {"parser_loc":"in file \"invalid_character_in_string.ligo\", line 1, characters 19-20"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_character_in_string.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid character in string.
      Hint: Remove or replace the character.
       {"parser_loc":"in file \"invalid_character_in_string.mligo\", line 1, characters 9-10"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_character_in_string.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid character in string.
      Hint: Remove or replace the character.
       {"parser_loc":"in file \"invalid_character_in_string.religo\", line 1, characters 9-10"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'
 |} ]