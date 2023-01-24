open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "bad_field_record.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ foo_record#0  ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 4, characters 8-9
    [ foo_record#0  ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 5, characters 8-9
    [ c#1 foo_record#0  ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11
    [ i#2 c#1 foo_record#0  ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    [ a#3 c#1 foo_record#0  ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    [ j#4 a#3 c#1 foo_record#0  ] File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3

    Variable definitions:
    (a#3 -> a)
    Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 8, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, character 2 to line 10, character 3
    Content: |resolved: int|
    references: []
    (b#5 -> b)
    Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 12, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, character 2 to line 14, character 3
    Content: |unresolved|
    references: []
    (c#1 -> c)
    Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 3, character 8 to line 6, character 1
    Content: |resolved: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-11 ,
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-11
    (i#2 -> i)
    Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 9, characters 10-15
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 10, characters 2-3
    (j#4 -> j)
    Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-15
    Content: |unresolved|
    references:
      File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 14, characters 2-3
    Type definitions:
    (foo_record#0 -> foo_record)
    Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 5-15
    Body Range: File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 1, characters 18-43
    Content: : |record[bar -> int , foo -> int]|
    references: []
    Module definitions:
    Errors:
    File "../../test/contracts/get_scope_tests/bad_field_record.mligo", line 13, characters 10-15:
     12 | let b =
     13 |   let j = c.boo in
     14 |   j

    Invalid record field "boo" in record.

|}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "local_type.ligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ u#0  ] File "../../test/contracts/get_scope_tests/local_type.ligo", line 2, characters 12-14
    [ b#4 toto#2 y#1 u#0  ] File "../../test/contracts/get_scope_tests/local_type.ligo", line 4, characters 42-43
    [ foo#3 toto#2 y#1 u#0  ] File "../../test/contracts/get_scope_tests/local_type.ligo", line 5, characters 7-11
    [ local_type#5  ] File "../../test/contracts/get_scope_tests/local_type.ligo", line 7, characters 10-12

    Variable definitions:
    (b#4 -> b)
    Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 4, characters 22-23
    Body Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 4, characters 42-43
    Content: |core: toto|
    references:
      File "../../test/contracts/get_scope_tests/local_type.ligo", line 4, characters 42-43
    (foo#3 -> foo)
    Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 4, characters 11-14
    Body Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 4, characters 2-43
    Content: |core: toto -> toto|
    references: []
    (local_type#5 -> local_type)
    Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 1, characters 9-19
    Body Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 1, character 0 to line 5, character 11
    Content: |core: unit -> int|
    references: []
    (u#0 -> u)
    Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 1, characters 25-26
    Body Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 1, character 44 to line 5, character 11
    Content: |core: unit|
    references: []
    (x#6 -> x)
    Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 7, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 7, characters 10-12
    Content: |resolved: int|
    references: []
    (y#1 -> y)
    Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 2, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 2, characters 12-14
    Content: |unresolved|
    references: []
    Type definitions:
    (toto#2 -> toto)
    Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 3, characters 7-11
    Body Range: File "../../test/contracts/get_scope_tests/local_type.ligo", line 3, characters 15-18
    Content: : |int|
    references:
      File "../../test/contracts/get_scope_tests/local_type.ligo", line 4, characters 26-30 ,
      File "../../test/contracts/get_scope_tests/local_type.ligo", line 4, characters 34-38
    Module definitions:
    Errors:
    File "../../test/contracts/get_scope_tests/local_type.ligo", line 5, characters 7-11:
      4 |   function foo (const b : toto) : toto is b
      5 | } with titi
      6 |

    Variable "titi" not found.
 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "errors/syntax_error.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect{|
    Scopes:

    Variable definitions:
    Type definitions:
    Module definitions:
    Errors:
    File "../../test/contracts/get_scope_tests/errors/syntax_error.mligo", line 1, characters 0-4:
      1 | letx : int = 0
    Ill-formed contract.
    At this point, if the declaration is complete, one of the following is
    expected:
      * another declaration;
      * the end of the file. |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "errors/type_error.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect{|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/errors/type_error.mligo", line 1, characters 14-21

    Variable definitions:
    (x#0 -> x)
    Range: File "../../test/contracts/get_scope_tests/errors/type_error.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/errors/type_error.mligo", line 1, characters 14-21
    Content: |core: int|
    references: []
    Type definitions:
    Module definitions:
    Errors:
    File "../../test/contracts/get_scope_tests/errors/type_error.mligo", line 1, characters 14-21:
      1 | let x : int = "Hello"

    Invalid type(s).
    Expected "int", but got: "string". |}]
