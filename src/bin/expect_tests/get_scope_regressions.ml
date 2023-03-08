open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/regressions/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "missing_stdlib_and_let_mut_in.jsligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ _useless#2 s#0  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 44-48
    [ do_nothing#1 s#0  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 2-26
    [ s#0  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, character 16 to line 4, character 1
    [ iter_op#3  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20

    Variable definitions:
    (_useless#2 -> _useless)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 20-28
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 44-48
    Content: |core: int|
    references: []
    (do_nothing#1 -> do_nothing)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 6-16
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 6-16
    Content: |resolved: int -> unit|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 13-23
    (iter_op#3 -> iter_op)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 6-13
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 6-13
    Content: |resolved: list (int) -> unit|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20
    (s#0 -> s)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 17-18
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, character 16 to line 4, character 1
    Content: |core: list (int)|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 25-26
    (test#4 -> test)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 6-10
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20
    Content: |resolved: list (int) -> unit|
    references: []
    Type definitions:
    Module definitions: |}]

(*
let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "missing_stdlib.ligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ p#0  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 2, characters 30-55
    [ c#1 p#0  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 3, characters 7-23

    Variable definitions:
    (c#1 -> c)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 2, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 2, characters 30-56
    Content: |core: contract (unit)|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 3, characters 22-23
    (main#2 -> main)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 1, characters 9-13
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 1, character 0 to line 3, character 24
    Content: |core: key_hash -> address|
    references: []
    (p#0 -> p)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 1, characters 21-22
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 1, character 48 to line 3, character 24
    Content: |core: key_hash|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.ligo", line 2, characters 54-55
    Type definitions:
    Module definitions: |}]
*)

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "buggy_file_with_core_types.jsligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ user#0  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 8, character 13 to line 9, character 17
    [ user#0  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 10, characters 13-20
    [ alice#1 user#0  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-30

    Variable definitions:
    (alice#1 -> alice)
    Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, characters 6-11
    Body Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, character 21 to line 11, character 1
    Content: |core: user|
    references:
      File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-30
    (alice_admin#2 -> alice_admin)
    Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 4-15
    Body Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-32
    Content: |core: bool|
    references: []
    Type definitions:
    (user#0 -> user)
    Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 1, characters 5-9
    Body Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 1, character 12 to line 5, character 1
    Content: : |record[id -> nat , is_admin -> bool , name -> string]|
    references:
      File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, characters 14-18
    Module definitions:
    Errors:
    File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-32:
     12 |
     13 | let alice_admin : bool = alice.i

    Invalid record field "i" in record.
    Warnings:
    File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 0-32:
     12 |
     13 | let alice_admin : bool = alice.i

    Toplevel let declaration are silently change to const declaration. |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "wrong_reference1.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 1, characters 8-10
    [ x#1  ] File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 10-11
    [ f#2 x#0  ] File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 8-9

    Variable definitions:
    (f#2 -> f)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 6-7
    Content: |resolved: ∀ gen#7 : * . gen#7 -> int|
    references: []
    (g#3 -> g)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 8-9
    Content: |resolved: int|
    references: []
    (x#0 -> x)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 1, characters 8-10
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 8-9
    (x#1 -> x)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 10-11
    Content: |resolved: gen#7|
    references: []
    Type definitions:
    Module definitions:
    Warnings:
    File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 6-7:
      1 | let x = 42
      2 | let f x = 0
      3 | let g = x
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 6-7:
      1 | let x = 42
      2 | let f x = 0
      3 | let g = x
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning. |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module_alias_def_reference.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 19-20
    [ D#3 A#2 B#1 toto#0  ] File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 15-21

    Variable definitions:
    Type definitions:
    Module definitions:
    (A#2 -> A)
    Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 2, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 2, character 0 to line 6, character 3
    Content: Members: Variable definitions:
                      Type definitions:
                      Module definitions:
                      (B#1 -> B)
                      Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 3, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 3, character 4 to line 5, character 7
                      Content: Members: Variable definitions:
                                        (toto#0 -> toto)
                                        Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 12-16
                                        Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 19-20
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 17-21
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 17-18


    references:
      File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 15-16

    (C#5 -> C)
    Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 8, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 8, character 0 to line 11, character 3
    Content: Members: Variable definitions:
                      (tata#4 -> tata)
                      Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 8-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 15-21
                      Content: |resolved: int|
                      references: []
                      Type definitions:
                      Module definitions:
                      (D#3 -> D)
                      Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 15-18
                      Content: Alias: A#2.B#1
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 15-16


    references: [] |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "local_module_alias_def_reference.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ titi#0  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 25-27
    [ E#5 D#4 A#3 C#2 toto#1 titi#0  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 12, characters 4-10

    Variable definitions:
    (toto#6 -> toto)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, character 4 to line 12, character 10
    Content: |core: D.titi|
    references: []
    Type definitions:
    Module definitions:
    (A#3 -> A)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 1, character 0 to line 6, character 3
    Content: Members: Variable definitions:
                      Type definitions:
                      (titi#0 -> titi)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 2, characters 9-13
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 2, characters 16-19
                      Content: : |int|
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 18-22 ,
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 13-17
                      Module definitions:
                      (C#2 -> C)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 3, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 3, character 4 to line 5, character 7
                      Content: Members: Variable definitions:
                                        (toto#1 -> toto)
                                        Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 12-16
                                        Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 25-27
                                        Content: |core: titi|
                                        references:
                                          File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 12, characters 6-10
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 17-18


    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 8, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 15-16

    (D#4 -> D)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 8, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 8, characters 11-12
    Content: Alias: A#3
    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 11-12

    (E#5 -> E)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 15-18
    Content: Alias: A#3.C#2
    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 12, characters 4-5 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "local_module_alias_def_reference2.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 23-24
    [ F#3 A#2 C#1 toto#0  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 19-25
    [ E#5 toto#4 F#3 A#2 C#1 toto#0  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 11, characters 4-10

    Variable definitions:
    (toto#6 -> toto)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 1, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 2, character 4 to line 11, character 10
    Content: |core: int|
    references: []
    Type definitions:
    Module definitions:
    (A#2 -> A)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 2, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 2, character 4 to line 6, character 7
    Content: Members: Variable definitions:
                      Type definitions:
                      Module definitions:
                      (C#1 -> C)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 3, characters 15-16
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 3, character 8 to line 5, character 11
                      Content: Members: Variable definitions:
                                        (toto#0 -> toto)
                                        Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 16-20
                                        Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 23-24
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 21-25
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 21-22


    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 19-20

    (E#5 -> E)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 7, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 7, character 4 to line 10, character 7
    Content: Members: Variable definitions:
                      (toto#4 -> toto)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 12-16
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 19-25
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 11, characters 6-10
                      Type definitions:
                      Module definitions:
                      (F#3 -> F)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 15-16
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 19-22
                      Content: Alias: A#2.C#1
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 19-20


    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 11, characters 4-5 |}]
