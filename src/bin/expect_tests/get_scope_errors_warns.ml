open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s
let () = Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:"true"

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
    ; gs "local_type.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ u#0  ] File "../../test/contracts/get_scope_tests/local_type.mligo", line 2, characters 10-12
    [ b#4 toto#2 y#1 u#0  ] File "../../test/contracts/get_scope_tests/local_type.mligo", line 4, characters 30-31
    [ foo#3 toto#2 y#1 u#0  ] File "../../test/contracts/get_scope_tests/local_type.mligo", line 5, characters 2-6
    [ local_type#5  ] File "../../test/contracts/get_scope_tests/local_type.mligo", line 7, characters 8-10

    Variable definitions:
    (b#4 -> b)
    Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 4, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 4, characters 30-31
    Content: |core: toto|
    references:
      File "../../test/contracts/get_scope_tests/local_type.mligo", line 4, characters 30-31
    (foo#3 -> foo)
    Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 4, characters 6-9
    Body Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 4, characters 11-12
    Content: |core: toto -> toto|
    references: []
    (local_type#5 -> local_type)
    Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 1, characters 4-14
    Body Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 1, characters 16-17
    Content: |core: unit -> int|
    references: []
    (u#0 -> u)
    Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 1, characters 16-17
    Body Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 2, character 2 to line 5, character 6
    Content: |core: unit|
    references: []
    (x#6 -> x)
    Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 7, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 7, characters 8-10
    Content: |resolved: int|
    references: []
    (y#1 -> y)
    Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 2, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 2, characters 10-12
    Content: |unresolved|
    references: []
    Type definitions:
    (toto#2 -> toto)
    Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 3, characters 7-11
    Body Range: File "../../test/contracts/get_scope_tests/local_type.mligo", line 3, characters 14-17
    Content: : |int|
    references:
      File "../../test/contracts/get_scope_tests/local_type.mligo", line 4, characters 15-19 ,
      File "../../test/contracts/get_scope_tests/local_type.mligo", line 4, characters 23-27
    Module definitions:
    Errors:
    File "../../test/contracts/get_scope_tests/local_type.mligo", line 5, characters 2-6:
      4 |   let foo (b : toto) : toto = b in
      5 |   titi
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
  [%expect
    {|
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
  [%expect
    {|
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
    Expected "int", but got: "string". |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; test "warning_unused.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ x#1 storage#0  ] File "../../test/contracts/warning_unused.mligo", line 6, characters 20-21
    [ x#3 foo#2 storage#0  ] File "../../test/contracts/warning_unused.mligo", line 7, characters 20-29
    [ bar#4 foo#2 storage#0  ] File "../../test/contracts/warning_unused.mligo", line 9, characters 10-13
    [ s#5 bar#4 foo#2 storage#0  ] File "../../test/contracts/warning_unused.mligo", line 10, characters 10-17
    [ x#6 s#5 bar#4 foo#2 storage#0  ] File "../../test/contracts/warning_unused.mligo", line 11, characters 10-15
    [ x#7 s#5 bar#4 foo#2 storage#0  ] File "../../test/contracts/warning_unused.mligo", line 12, characters 10-15
    [ x#8 s#5 bar#4 foo#2 storage#0  ] File "../../test/contracts/warning_unused.mligo", line 13, characters 26-38

    Variable definitions:
    (bar#4 -> bar)
    Range: File "../../test/contracts/warning_unused.mligo", line 7, characters 4-7
    Body Range: File "../../test/contracts/warning_unused.mligo", line 7, characters 9-10
    Content: |resolved: int -> int|
    references:
      File "../../test/contracts/warning_unused.mligo", line 12, characters 10-13
    (foo#2 -> foo)
    Range: File "../../test/contracts/warning_unused.mligo", line 6, characters 4-7
    Body Range: File "../../test/contracts/warning_unused.mligo", line 6, characters 9-10
    Content: |resolved: int -> int|
    references:
      File "../../test/contracts/warning_unused.mligo", line 11, characters 10-13
    (main#9 -> main)
    Range: File "../../test/contracts/warning_unused.mligo", line 9, characters 4-8
    Body Range: File "../../test/contracts/warning_unused.mligo", line 9, characters 10-13
    Content: |resolved: ( int *
                          record[x -> int , y -> int({ name: x }, { name: y })] ) ->
    ( list (operation) *
      record[x -> int , y -> int({ name: x }, { name: y })] )|
    references: []
    (s#5 -> s)
    Range: File "../../test/contracts/warning_unused.mligo", line 9, characters 12-13
    Body Range: File "../../test/contracts/warning_unused.mligo", line 10, character 2 to line 13, character 39
    Content: |resolved: storage|
    references:
      File "../../test/contracts/warning_unused.mligo", line 10, characters 10-11 ,
      File "../../test/contracts/warning_unused.mligo", line 12, characters 14-15 ,
      File "../../test/contracts/warning_unused.mligo", line 13, characters 26-27
    (x#1 -> x)
    Range: File "../../test/contracts/warning_unused.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/warning_unused.mligo", line 6, characters 20-21
    Content: |core: int|
    references:
      File "../../test/contracts/warning_unused.mligo", line 6, characters 20-21
    (x#3 -> x)
    Range: File "../../test/contracts/warning_unused.mligo", line 7, characters 9-10
    Body Range: File "../../test/contracts/warning_unused.mligo", line 7, characters 20-29
    Content: |core: int|
    references:
      File "../../test/contracts/warning_unused.mligo", line 7, characters 20-21
    (x#6 -> x)
    Range: File "../../test/contracts/warning_unused.mligo", line 10, characters 6-7
    Body Range: File "../../test/contracts/warning_unused.mligo", line 10, characters 10-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/warning_unused.mligo", line 11, characters 14-15
    (x#7 -> x)
    Range: File "../../test/contracts/warning_unused.mligo", line 11, characters 6-7
    Body Range: File "../../test/contracts/warning_unused.mligo", line 11, characters 10-15
    Content: |resolved: int|
    references: []
    (x#8 -> x)
    Range: File "../../test/contracts/warning_unused.mligo", line 12, characters 6-7
    Body Range: File "../../test/contracts/warning_unused.mligo", line 12, characters 10-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/warning_unused.mligo", line 13, characters 37-38
    Type definitions:
    (storage#0 -> storage)
    Range: File "../../test/contracts/warning_unused.mligo", line 1, characters 5-12
    Body Range: File "../../test/contracts/warning_unused.mligo", line 1, character 15 to line 4, character 1
    Content: : |record[x -> int , y -> int]|
    references:
      File "../../test/contracts/warning_unused.mligo", line 9, characters 22-29
    Module definitions:
    Warnings:
    File "../../test/contracts/warning_unused.mligo", line 11, characters 6-7:
     10 |   let x = s.x + 3 in
     11 |   let x = foo x in
     12 |   let x = bar s.x in
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning. |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; test "warning_duplicate2.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/warning_duplicate2.mligo", line 1, characters 8-35
    [ x#0  ] File "../../test/contracts/warning_duplicate2.mligo", line 2, characters 9-10
    [ x#0  ] File "../../test/contracts/warning_duplicate2.mligo", line 2, characters 12-13

    Variable definitions:
    (x#0 -> x)
    Range: File "../../test/contracts/warning_duplicate2.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/warning_duplicate2.mligo", line 1, characters 8-35
    Content: |resolved: option (ticket (nat))|
    references:
      File "../../test/contracts/warning_duplicate2.mligo", line 2, characters 9-10 ,
      File "../../test/contracts/warning_duplicate2.mligo", line 2, characters 12-13
    (x#1 -> x)
    Range: File "../../test/contracts/warning_duplicate2.mligo", line 2, characters 4-5
    Body Range: File "../../test/contracts/warning_duplicate2.mligo", line 2, characters 9-13
    Content: |resolved: option (ticket (nat))|
    references: []
    Type definitions:
    Module definitions:
    Warnings:
    File "../../test/contracts/warning_duplicate2.mligo", line 1, characters 4-5:
      1 | let x = Tezos.create_ticket 42n 42n
      2 | let x = (x, x)
    :
    Warning: variable "x" cannot be used more than once. |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; test "warning_duplicate.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/warning_duplicate.mligo", line 2, characters 23-64
    [ Foo#1 x#0  ] File "../../test/contracts/warning_duplicate.mligo", line 5, characters 9-14
    [ Foo#1 x#0  ] File "../../test/contracts/warning_duplicate.mligo", line 5, characters 16-21

    Variable definitions:
    (x#2 -> x)
    Range: File "../../test/contracts/warning_duplicate.mligo", line 5, characters 4-5
    Body Range: File "../../test/contracts/warning_duplicate.mligo", line 5, characters 9-21
    Content: |resolved: ticket (nat)|
    references: []
    Type definitions:
    Module definitions:
    (Foo#1 -> Foo)
    Range: File "../../test/contracts/warning_duplicate.mligo", line 1, characters 7-10
    Body Range: File "../../test/contracts/warning_duplicate.mligo", line 1, character 0 to line 3, character 3
    Content: Members: Variable definitions:
                      (x#0 -> x)
                      Range: File "../../test/contracts/warning_duplicate.mligo", line 2, characters 6-7
                      Body Range: File "../../test/contracts/warning_duplicate.mligo", line 2, characters 23-65
                      Content: |core: ticket (nat)|
                      references:
                        File "../../test/contracts/warning_duplicate.mligo", line 5, characters 13-14 ,
                        File "../../test/contracts/warning_duplicate.mligo", line 5, characters 20-21
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/warning_duplicate.mligo", line 5, characters 9-12 ,
      File "../../test/contracts/warning_duplicate.mligo", line 5, characters 16-19

    Warnings:
    File "../../test/contracts/warning_duplicate.mligo", line 2, characters 2-65:
      1 | module Foo = struct
      2 |   let x : nat ticket = Option.unopt (Tezos.create_ticket 42n 42n)
      3 | end
    :
    Warning: variable "Foo.x" cannot be used more than once. |}]

let () = Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:""
