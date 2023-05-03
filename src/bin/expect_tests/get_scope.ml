open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "lambda_letin.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  print_endline @@ replace_extraneous_slashes @@ [%expect.output];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 11-28
    [ a#1:4-5 i#4:37-38 j#4:47-48  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
    [ a#1:4-5 i#4:37-38 j#4:47-48 g#5:8-9  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
    [ a#1:4-5 i#4:37-38 j#4:47-48 g#5:8-9 k#6:8-9  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
    [ a#1:4-5 f#4:6-7  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, character 2 to line 9, character 7
    Content: |resolved: int|
    references: []
    (f#4:6-7 -> f)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, character 32 to line 7, character 21
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
    (i#4:37-38 -> i)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, character 32 to line 7, character 21
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
    (j#4:47-48 -> j)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, character 4 to line 7, character 21
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
    (g#5:8-9 -> g)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
    (k#6:8-9 -> k)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "letin.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  print_endline @@ replace_extraneous_slashes @@ [%expect.output];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ a#1:4-5 c#4:6-7  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ a#1:4-5 c#4:6-7 e#6:8-9  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ a#1:4-5 c#4:6-7 e#6:8-9 f#7:8-9  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ a#1:4-5 c#4:6-7 d#5:6-7  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, character 2 to line 10, character 11
    Content: |resolved: int|
    references: []
    (c#4:6-7 -> c)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#5:6-7 -> d)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, character 4 to line 8, character 17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#6:8-9 -> e)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#7:8-9 -> f)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "lambda.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 11-27
    [ a#1:4-5 i#4:36-37 j#4:46-47  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
    [ a#1:4-5 f#4:6-7  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references: []
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, character 2 to line 5, character 7
    Content: |resolved: int|
    references: []
    (f#4:6-7 -> f)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 31-63
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
    (i#4:36-37 -> i)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 31-63
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
    (j#4:46-47 -> j)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "match.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 14-40
    [ mytype#1:5-11  ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 8-9
    [ mytype#1:5-11 a#3:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18
    [ mytype#1:5-11 a#3:4-5 c#6:13-14  ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ mytype#1:5-11 a#3:4-5 x#7:8-9  ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ mytype#1:5-11 a#3:4-5 y#8:8-9  ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ mytype#1:5-11 a#3:4-5 b#5:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 9-21
    [ mytype#1:5-11 a#3:4-5 b#5:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 12-13
    [ mytype#1:5-11 a#3:4-5 b#5:4-5 c#13:8-9  ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ mytype#1:5-11 a#3:4-5 b#5:4-5 hd#15:4-6 tl#15:8-10  ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ mytype#1:5-11 a#3:4-5 b#5:4-5 c#10:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 17-18
    [ mytype#1:5-11 a#3:4-5 b#5:4-5 c#10:4-5 d#18:13-14  ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ mytype#1:5-11 a#3:4-5 b#5:4-5 c#10:4-5 s#19:10-11  ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ mytype#1:5-11 a#3:4-5 b#5:4-5 c#10:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13

    Variable definitions:
    (a#3:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    (b#5:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 6, character 2 to line 8, character 18
    Content: |resolved: int|
    references: []
    (c#6:13-14 -> c)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (x#7:8-9 -> x)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#8:8-9 -> y)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    Content: |resolved: string|
    references: []
    (c#10:4-5 -> c)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 11, character 2 to line 15, character 15
    Content: |resolved: int|
    references: []
    (c#13:8-9 -> c)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 12-13
    Content: |resolved: int|
    references: []
    (hd#15:4-6 -> hd)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    Content: |resolved: int|
    references: []
    (tl#15:8-10 -> tl)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    Content: |resolved: list (int)|
    references: []
    (d#17:4-5 -> d)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 18, character 2 to line 20, character 13
    Content: |resolved: int|
    references: []
    (d#18:13-14 -> d)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 17-18
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (s#19:10-11 -> s)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    Type definitions:
    (mytype#1:5-11 -> mytype)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 5-11
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 14-40
    Content: : |sum[Bar -> string , Foo -> int]|
    references: []
    Module definitions:
    Warnings:
    File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9:
      7 |   | Foo x -> x + a
      8 |   | Bar y -> 1 + a
                  ^
      9 |
    :
    Warning: unused variable "y".
    Hint: replace it by "_y" to prevent this warning.

    File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10:
     14 |     a
     15 |   | hd::tl -> 2
                  ^^
     16 |
    :
    Warning: unused variable "tl".
    Hint: replace it by "_tl" to prevent this warning.

    File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6:
     14 |     a
     15 |   | hd::tl -> 2
              ^^
     16 |
    :
    Warning: unused variable "hd".
    Hint: replace it by "_hd" to prevent this warning.

    File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9:
     12 |   | [] ->
     13 |     let c = 2 in
                  ^
     14 |     a
    :
    Warning: unused variable "c".
    Hint: replace it by "_c" to prevent this warning. |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "rec.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 8-9
    [ a#1:4-5 c#4:10-11  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 4, character 2 to line 13, character 10
    [ a#1:4-5 c#4:10-11 i#4:37-38 j#4:39-40  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ a#1:4-5 c#4:10-11 i#4:37-38 j#4:39-40 k#5:8-9  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-11
    [ a#1:4-5 c#4:10-11 z#8:10-11 n#8:13-14 m#8:23-24  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 7-12
    [ a#1:4-5 c#4:10-11 z#8:10-11 n#8:13-14 m#8:23-24  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 18-19
    [ a#1:4-5 c#4:10-11 z#8:10-11 n#8:13-14 m#8:23-24  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 25-36
    [ a#1:4-5 c#4:10-11 z#8:10-11  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 10-18
    [ a#1:4-5 c#4:10-11 z#8:10-11 v#11:6-7  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 10-15
    [ a#1:4-5 c#4:10-11 z#8:10-11 v#11:6-7 b#12:6-7  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 2-10
    [ a#1:4-5 b#3:4-5 x#15:8-9 y#15:11-12  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 16, characters 5-10
    [ a#1:4-5 b#3:4-5 x#15:8-9 y#15:11-12  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 16, characters 16-18
    [ a#1:4-5 b#3:4-5 x#15:8-9 y#15:11-12  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 17, characters 7-16

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 5-6
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, character 2 to line 13, character 10
    Content: |resolved: int|
    references: []
    (c#4:10-11 -> c)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, character 4 to line 6, character 11
    Content: |core: ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 2-3
    (i#4:37-38 -> i)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, character 2 to line 13, character 10
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#4:39-40 -> j)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, character 2 to line 13, character 10
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#5:8-9 -> k)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    (z#8:10-11 -> z)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 13, character 10
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 25-26 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 10-11
    (n#8:13-14 -> n)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 13-14
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 13, character 10
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 7-8 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 28-29
    (m#8:23-24 -> m)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 23-24
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 4-36
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 18-19 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 35-36
    (v#11:6-7 -> v)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 10-18
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 14-15
    (b#12:6-7 -> b)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 10-15
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 8-9
    (x#15:8-9 -> x)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 15, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 16, character 2 to line 17, character 16
    Content: |core: int -> unit|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 17, characters 7-8
    (y#15:11-12 -> y)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 15, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 16, character 2 to line 17, character 16
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 16, characters 5-6 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 17, characters 10-11
    Type definitions:
    Module definitions:
 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "shadowing.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ a#1:4-5 c#4:6-7  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ a#1:4-5 c#4:6-7 e#6:8-9  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ c#4:6-7 e#6:8-9 a#7:8-9  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ a#1:4-5 c#4:6-7 d#5:6-7  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, character 2 to line 10, character 11
    Content: |resolved: int|
    references: []
    (c#4:6-7 -> c)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#5:6-7 -> d)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, character 4 to line 8, character 13
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#6:8-9 -> e)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    (a#7:8-9 -> a)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "records.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 13-36
    [ myrec#1:5-10  ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 8-9
    [ myrec#1:5-10 a#3:4-5  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 22-23
    [ myrec#1:5-10 a#3:4-5 i#6:18-19  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 35-36
    [ myrec#1:5-10 a#3:4-5 i#6:18-19 j#6:31-32  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ myrec#1:5-10 a#3:4-5  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 53-55
    [ myrec#1:5-10 a#3:4-5 b#6:4-5  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4
    [ myrec#1:5-10 a#3:4-5 b#6:4-5  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24
    [ myrec#1:5-10 a#3:4-5 b#6:4-5 g#16:19-20  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41

    Variable definitions:
    (a#3:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#6:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 8-56
    Content: |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (i#6:18-19 -> i)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 22-23
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#6:31-32 -> j)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 35-36
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    (e#15:4-5 -> e)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 2-44
    Content: |resolved: myrec|
    references: []
    (g#16:19-20 -> g)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    Type definitions:
    (myrec#1:5-10 -> myrec)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 5-10
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 13-36
    Content: : |record[bar -> int , foo -> int]|
    references: []
    Module definitions: |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "constant.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    [ a#1:4-5 c#5:10-11  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ a#1:4-5 c#5:10-11 d#5:26-27  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ a#1:4-5 e#6:9-10  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (c#5:10-11 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#5:26-27 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#6:9-10 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "application.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 11-27
    [ i#2:36-37 j#2:46-47  ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ f#2:6-7  ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 2-37
    [ f#2:6-7 b#3:7-8  ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ f#2:6-7 c#3:26-27  ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, character 2 to line 3, character 37
    Content: |resolved: int|
    references: []
    (f#2:6-7 -> f)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 31-63
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#2:36-37 -> i)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 31-63
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#2:46-47 -> j)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    (b#3:7-8 -> b)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 11-12
    Content: |resolved: int|
    references: []
    (c#3:26-27 -> c)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    Type definitions:
    Module definitions:
    Warnings:
    File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8:
      2 |   let f : (int-> int -> int) = fun (i : int) (j : int) -> j + i in
      3 |   (let b = 1 in f 1) (let c = 2 in c)
                 ^
    :
    Warning: unused variable "b".
    Hint: replace it by "_b" to prevent this warning.
 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "include.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  print_endline @@ replace_extraneous_slashes @@ [%expect.output];
  [%expect
    {|
    Scopes:
    [ a#1:4-5 b#3:4-5  ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 8-9
    [ x#3:4-5 a#1:4-5 b#3:4-5  ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ a#1:4-5 c#4:6-7  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ a#1:4-5 c#4:6-7 e#6:8-9  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ a#1:4-5 c#4:6-7 e#6:8-9 f#7:8-9  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ a#1:4-5 c#4:6-7 d#5:6-7  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11

    Variable definitions:
    (x#3:4-5 -> x)
    Range: File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#5:4-5 -> y)
    Range: File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    Content: |resolved: int|
    references: []
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, character 2 to line 10, character 11
    Content: |resolved: int|
    references: []
    (c#4:6-7 -> c)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#5:6-7 -> d)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, character 4 to line 8, character 17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#6:8-9 -> e)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#7:8-9 -> f)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "nominal_types.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 19-45
    [ foo_variant#1:5-16  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 18-58
    [ foo_variant#1:5-16 foo_record#2:5-15  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ foo_variant#1:5-16 foo_record#2:5-15 a#4:4-5  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ foo_variant#1:5-16 foo_record#2:5-15 a#4:4-5 b#6:4-5  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    [ foo_variant#1:5-16 foo_record#2:5-15 a#4:4-5 b#6:4-5  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    [ foo_variant#1:5-16 foo_record#2:5-15 a#4:4-5 b#6:4-5 c#8:4-5 p#13:10-11  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43

    Variable definitions:
    (a#4:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 8-13
    Content: |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#6:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 8-14
    Content: |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#8:4-5 -> c)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, character 8 to line 11, character 1
    Content: |resolved: foo_record|
    references: []
    (main#13:4-8 -> main)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 0-47
    Content: |core: foo_record -> foo_variant|
    references: []
    (p#13:10-11 -> p)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-47
    Content: |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_variant#1:5-16 -> foo_variant)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 5-16
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 19-45
    Content: : |sum[Bar -> string , Foo -> int]|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 26-37 ,
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 46-57 ,
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 28-39
    (foo_record#2:5-15 -> foo_record)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 5-15
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 18-58
    Content: : |record[bar -> foo_variant , foo -> foo_variant]|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 14-24
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 15-16
    [ A#1:7-8 toto#2:8-12  ] File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 8-14
    [ A#1:7-8 toto#2:8-12 a#5:4-5 B#7:7-8  ] File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 8-14
    [ A#1:7-8 toto#2:8-12 a#5:4-5 B#7:7-8 b#9:4-5  ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 16-17
    [ A#1:7-8 toto#2:8-12 a#5:4-5 B#7:7-8 b#9:4-5 C#12:11-12 a#13:12-13 D#15:11-12  ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7

    Variable definitions:
    (a#5:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 8-14
    Content: |resolved: int|
    references: []
    (b#9:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 8-14
    Content: |resolved: int|
    references: []
    (titi#11:4-8 -> titi)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 4-16
    Content: Members: Variable definitions:
                      (toto#2:8-12 -> toto)
                      Range: File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 8-12
                      Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 15-16
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 10-14 ,
                        File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 10-14
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 11-12

    (B#7:7-8 -> B)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 11-12
    Content: Alias: A#1:7-8
    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 8-9

    (C#12:11-12 -> C)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 12, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17
    Content: Members: Variable definitions:
                      (a#13:12-13 -> a)
                      Range: File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 16-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 6-7
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 15, characters 15-16

    (D#15:11-12 -> D)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 15, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 15, characters 15-16
    Content: Alias: C#12:11-12
    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-5 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module2.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 2, characters 12-13
    [ x#2:8-9  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 4, characters 16-17
    [ A#1:7-8 x#2:8-9 B#3:11-12 y#4:12-13 E#6:11-12 C#9:7-8 D#11:7-8 F#13:7-8  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 9-14
    [ A#1:7-8 x#2:8-9 B#3:11-12 y#4:12-13 E#6:11-12 C#9:7-8 D#11:7-8 F#13:7-8 a1#15:4-6  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 9-12
    [ A#1:7-8 x#2:8-9 B#3:11-12 y#4:12-13 E#6:11-12 C#9:7-8 D#11:7-8 F#13:7-8 a1#15:4-6 a2#16:4-6  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 9-12
    [ A#1:7-8 x#2:8-9 B#3:11-12 y#4:12-13 E#6:11-12 C#9:7-8 D#11:7-8 F#13:7-8 a1#15:4-6 a2#16:4-6 a3#17:4-6  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 9-12
    [ A#1:7-8 x#2:8-9 B#3:11-12 y#4:12-13 E#6:11-12 C#9:7-8 D#11:7-8 F#13:7-8 a1#15:4-6 a2#16:4-6 a3#17:4-6 a4#18:4-6  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 9-12

    Variable definitions:
    (a1#15:4-6 -> a1)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 9-14
    Content: |resolved: int|
    references: []
    (a2#16:4-6 -> a2)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 9-12
    Content: |resolved: int|
    references: []
    (a3#17:4-6 -> a3)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 9-12
    Content: |resolved: int|
    references: []
    (a4#18:4-6 -> a4)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 9-12
    Content: |resolved: int|
    references: []
    (a5#19:4-6 -> a5)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 9-12
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 2, character 4 to line 6, character 16
    Content: Members: Variable definitions:
                      (x#2:8-9 -> x)
                      Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 2, characters 8-9
                      Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 2, characters 12-13
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 11-12
                      Type definitions:
                      Module definitions:
                      (B#3:11-12 -> B)
                      Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 3, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 4, characters 8-17
                      Content: Members: Variable definitions:
                                        (y#4:12-13 -> y)
                                        Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 4, characters 12-13
                                        Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 4, characters 16-17
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 11-12 ,
                                          File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 11-12
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 6, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 13-14 ,
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 11-12

                      (E#6:11-12 -> E)
                      Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 6, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 6, characters 15-16
                      Content: Alias: B#3:11-12
                      references:
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 13-14


    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 9-10

    (C#9:7-8 -> C)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 11-14
    Content: Alias: A#1:7-8.B#3:11-12
    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 9-10

    (D#11:7-8 -> D)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 11-12
    Content: Alias: A#1:7-8
    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 9-10

    (F#13:7-8 -> F)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 11-14
    Content: Alias: A#1:7-8.E#6:11-12
    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 9-10 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module3.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 12-13
    [ A#1:7-8 x#2:8-9  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 7, characters 16-17
    [ A#1:7-8 x#2:8-9 y#7:12-13  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 16-19
    [ A#1:7-8 x#2:8-9 y#7:12-13 z#8:12-13  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 21-22
    [ A#1:7-8 x#2:8-9 y#7:12-13 z#8:12-13 a1#10:16-18  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 21-22
    [ A#1:7-8 x#2:8-9 y#7:12-13 z#8:12-13 a1#10:16-18 a2#11:16-18  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 12, characters 21-22
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 13-16
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 13-16
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 13-19
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 13-19
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 13-19
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 13-16
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 13-16
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 13-19
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 13-19
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 13-19
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10 c5#29:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 13-17
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10 c5#29:8-10 e1#31:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 13-17
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10 c5#29:8-10 e1#31:8-10 e2#32:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 13-17
    [ A#1:7-8 x#2:8-9 B#6:11-12 y#7:12-13 z#8:12-13 C#9:15-16 a1#10:16-18 a2#11:16-18 a3#12:16-18 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10 c5#29:8-10 e1#31:8-10 e2#32:8-10 e3#33:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 35, character 4 to line 38, character 16

    Variable definitions:
    (x#5:4-5 -> x)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 5, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 6, character 4 to line 38, character 16
    Content: |resolved: int|
    references: []
    (b1#19:8-10 -> b1)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 13-16
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 4-6
    (b2#20:8-10 -> b2)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 13-16
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 9-11
    (b3#21:8-10 -> b3)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 13-19
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 14-16
    (b4#22:8-10 -> b4)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 13-19
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 19-21
    (b5#23:8-10 -> b5)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 13-19
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 24-26
    (c1#25:8-10 -> c1)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 13-16
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 4-6
    (c2#26:8-10 -> c2)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 13-16
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 9-11
    (c3#27:8-10 -> c3)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 13-19
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 14-16
    (c4#28:8-10 -> c4)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 13-19
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 19-21
    (c5#29:8-10 -> c5)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 13-19
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 24-26
    (e1#31:8-10 -> e1)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 13-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 38, characters 4-6
    (e2#32:8-10 -> e2)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 13-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 38, characters 9-11
    (e3#33:8-10 -> e3)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 13-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 38, characters 14-16
    Type definitions:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 4-13
    Content: Members: Variable definitions:
                      (x#2:8-9 -> x)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 8-9
                      Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 12-13
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 18-19
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 16-17

    (B#6:11-12 -> B)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 6, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 7, character 8 to line 13, character 11
    Content: Members: Variable definitions:
                      (y#7:12-13 -> y)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 7, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 7, characters 16-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 21-22 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 15-16
                      (z#8:12-13 -> z)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 16-19
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 21-22 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 15-16
                      Type definitions:
                      Module definitions:
                      (C#9:15-16 -> C)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 9, characters 15-16
                      Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 10, character 12 to line 12, character 22
                      Content: Members: Variable definitions:
                                        (a1#10:16-18 -> a1)
                                        Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 16-18
                                        Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 21-22
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 15-17
                                        (a2#11:16-18 -> a2)
                                        Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 16-18
                                        Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 21-22
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 15-17
                                        (a3#12:16-18 -> a3)
                                        Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 12, characters 16-18
                                        Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 12, characters 21-22
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 15-17
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 17-18 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 15-16


    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 16, characters 15-16 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 15-16 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 13-14

    (D#16:11-12 -> D)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 16, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 16, characters 15-16
    Content: Alias: B#6:11-12
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 13-14

    (E#17:11-12 -> E)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 15-18
    Content: Alias: B#6:11-12.C#9:15-16
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 13-14 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module4.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module4.mligo", line 2, characters 12-13
    [ a#2:8-9  ] File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 16-17
    [ a#2:8-9 B#3:11-12 y#4:12-13  ] File "../../test/contracts/get_scope_tests/module4.mligo", line 6, characters 4-7

    Variable definitions:
    (x#1:4-5 -> x)
    Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 2, character 4 to line 6, character 7
    Content: |resolved: int|
    references: []
    (a#2:8-9 -> a)
    Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 2, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 2, characters 12-13
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 16-17
    Type definitions:
    Module definitions:
    (B#3:11-12 -> B)
    Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 3, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 8-17
    Content: Members: Variable definitions:
                      (y#4:12-13 -> y)
                      Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 16-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module4.mligo", line 6, characters 6-7
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module4.mligo", line 6, characters 4-5 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module5.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 4, characters 20-21
    [ A#1:7-8 B#2:11-12 C#3:15-16 x#4:16-17 F#6:15-16 E#8:11-12 D#11:7-8 G#13:7-8  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 9-16
    [ A#1:7-8 B#2:11-12 C#3:15-16 x#4:16-17 F#6:15-16 E#8:11-12 D#11:7-8 G#13:7-8 x1#15:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 9-16
    [ A#1:7-8 B#2:11-12 C#3:15-16 x#4:16-17 F#6:15-16 E#8:11-12 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 9-16
    [ A#1:7-8 B#2:11-12 C#3:15-16 x#4:16-17 F#6:15-16 E#8:11-12 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 9-16
    [ A#1:7-8 B#2:11-12 C#3:15-16 x#4:16-17 F#6:15-16 E#8:11-12 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6 x4#18:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 9-16
    [ A#1:7-8 B#2:11-12 C#3:15-16 x#4:16-17 F#6:15-16 E#8:11-12 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6 x4#18:4-6 x5#19:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 9-16
    [ A#1:7-8 B#2:11-12 C#3:15-16 x#4:16-17 F#6:15-16 E#8:11-12 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6 x4#18:4-6 x5#19:4-6 x6#20:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 9-16
    [ A#1:7-8 B#2:11-12 C#3:15-16 x#4:16-17 F#6:15-16 E#8:11-12 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6 x4#18:4-6 x5#19:4-6 x6#20:4-6 x7#21:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 9-16

    Variable definitions:
    (x1#15:4-6 -> x1)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 9-16
    Content: |resolved: int|
    references: []
    (x2#16:4-6 -> x2)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 9-16
    Content: |resolved: int|
    references: []
    (x3#17:4-6 -> x3)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 9-16
    Content: |resolved: int|
    references: []
    (x4#18:4-6 -> x4)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 9-16
    Content: |resolved: int|
    references: []
    (x5#19:4-6 -> x5)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 9-16
    Content: |resolved: int|
    references: []
    (x6#20:4-6 -> x6)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 9-16
    Content: |resolved: int|
    references: []
    (x7#21:4-6 -> x7)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 9-16
    Content: |resolved: int|
    references: []
    (x8#22:4-6 -> x8)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 9-16
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 2, character 4 to line 8, character 16
    Content: Members: Variable definitions:
                      Type definitions:
                      Module definitions:
                      (B#2:11-12 -> B)
                      Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 2, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 3, character 8 to line 6, character 20
                      Content: Members: Variable definitions:
                                        Type definitions:
                                        Module definitions:
                                        (C#3:15-16 -> C)
                                        Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 3, characters 15-16
                                        Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 4, characters 12-21
                                        Content: Members: Variable definitions:
                                                          (x#4:16-17 -> x)
                                                          Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 4, characters 16-17
                                                          Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 4, characters 20-21
                                                          Content: |resolved: int|
                                                          references:
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 15-16
                                                          Type definitions:
                                                          Module definitions:

                                        references:
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 6, characters 19-20 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 13-14

                                        (F#6:15-16 -> F)
                                        Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 6, characters 15-16
                                        Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 6, characters 19-20
                                        Content: Alias: C#3:15-16
                                        references:
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 15-16 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 13-14


                      references:
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 8, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 11-12

                      (E#8:11-12 -> E)
                      Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 8, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 8, characters 15-16
                      Content: Alias: B#2:11-12
                      references:
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 13-14 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 11-12


    references:
      File "../../test/contracts/get_scope_tests/module5.mligo", line 11, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 9-10

    (D#11:7-8 -> D)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 11, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 11, characters 11-12
    Content: Alias: A#1:7-8
    references:
      File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 9-10

    (G#13:7-8 -> G)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 11-16
    Content: Alias: D#11:7-8.E#8:11-12.F#6:15-16
    references: [] |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module_shadowing.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
      Scopes:
      [  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 2, characters 12-13
      [ A#1:7-8 x#2:8-9 D#5:7-8  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, characters 12-13
      [ A#1:7-8 x#2:8-9 D#5:7-8 x#8:8-9  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 16-19
      [ D#5:7-8 x#8:8-9 A#9:11-12 x#10:12-13 C#12:11-12  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 4-11

      Variable definitions:
      (y#7:4-5 -> y)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 7, characters 4-5
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, character 4 to line 13, character 11
      Content: |resolved: int|
      references: []
      (x#8:8-9 -> x)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, characters 8-9
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, characters 12-13
      Content: |resolved: int|
      references:
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 4-5
      Type definitions:
      Module definitions:
      (A#1:7-8 -> A)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 1, characters 7-8
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 2, characters 4-13
      Content: Members: Variable definitions:
                        (x#2:8-9 -> x)
                        Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 2, characters 8-9
                        Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 2, characters 12-13
                        Content: |resolved: int|
                        references:
                          File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 18-19
                        Type definitions:
                        Module definitions:

      references:
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 5, characters 11-12 ,
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 16-17

      (D#5:7-8 -> D)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 5, characters 7-8
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 5, characters 11-12
      Content: Alias: A#1:7-8
      references: []

      (A#9:11-12 -> A)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 9, characters 11-12
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 8-19
      Content: Members: Variable definitions:
                        (x#10:12-13 -> x)
                        Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 12-13
                        Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 16-19
                        Content: |resolved: int|
                        references:
                          File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 10-11
                        Type definitions:
                        Module definitions:

      references:
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 12, characters 15-16 ,
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 8-9

      (C#12:11-12 -> C)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 12, characters 11-12
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 12, characters 15-16
      Content: Alias: A#9:11-12
      references: [] |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "types.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
      Scopes:
      [  ] File "../../test/contracts/get_scope_tests/types.mligo", line 2, characters 13-16
      [ t#2:9-10  ] File "../../test/contracts/get_scope_tests/types.mligo", line 3, characters 16-28
      [ t#2:9-10 x#3:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 12-13
      [ t#2:9-10 x#3:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 16-17
      [ t#2:9-10 x#3:12-13 a#5:8-9  ] File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 12-20
      [ t#2:9-10 x#3:12-13 a#5:8-9  ] File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 31-36
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19  ] File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 16-19
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19  ] File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 22-29
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19 g#12:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 16-26
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19 g#12:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 37-44
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 15, characters 8-10
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 8-11
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 14-15
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 c#21:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 11-14
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 c#21:4-5 hmm#23:5-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 8-15
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 c#21:4-5 hmm#23:5-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 26-28
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 c#21:4-5 hmm#23:5-8 d#24:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 8-11
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 c#21:4-5 hmm#23:5-8 d#24:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 14-15
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 d#24:4-5 c#27:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 11-14
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 d#24:4-5 c#27:4-5 idk#29:5-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 8-16
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 d#24:4-5 c#27:4-5 idk#29:5-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 27-31
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 32, characters 9-12
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6  ] File "../../test/contracts/get_scope_tests/types.mligo", line 33, characters 12-21
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9  ] File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 8-9
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9  ] File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 12-14
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 e#36:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 8-14
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 e#36:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 21-26
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 e#36:4-5 f#38:4-5 qux#41:9-12  ] File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 12-15
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 e#36:4-5 f#38:4-5 qux#41:9-12  ] File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 18-23
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 e#36:4-5 f#38:4-5 qux#41:9-12 i#43:8-9 boo#44:12-15  ] File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 12-19
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 e#36:4-5 f#38:4-5 qux#41:9-12 i#43:8-9 boo#44:12-15  ] File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 27-29
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 e#36:4-5 f#38:4-5 qux#41:9-12 i#43:8-9 boo#44:12-15 j#46:8-9  ] File "../../test/contracts/get_scope_tests/types.mligo", line 47, characters 4-6
      [ A#1:7-8 t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 exp1#8:8-12 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 e#36:4-5 f#38:4-5 exp2#40:4-8 x#49:14-15  ] File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 30-35

      Variable definitions:
      (c#21:4-5 -> c)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 4-5
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 14-15
      Content: |core: B.t|
      references: []
      (d#24:4-5 -> d)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 4-5
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 18-30
      Content: |core: hmm (nat)|
      references: []
      (c#27:4-5 -> c)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 4-5
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 14-15
      Content: |core: A.t|
      references: []
      (d#30:4-5 -> d)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 4-5
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 19-33
      Content: |core: idk (bool)|
      references: []
      (e#36:4-5 -> e)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 4-5
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 12-14
      Content: |core: s|
      references: []
      (f#38:4-5 -> f)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 4-5
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 17-26
      Content: |core: q (bool)|
      references: []
      (exp2#40:4-8 -> exp2)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 40, characters 4-8
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 41, character 4 to line 47, character 6
      Content: |resolved: unit|
      references: []
      (i#43:8-9 -> i)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 8-9
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 18-23
      Content: |core: qux|
      references: []
      (j#46:8-9 -> j)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 8-9
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 22-29
      Content: |core: boo (nat)|
      references: []
      (fn#49:4-6 -> fn)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 4-6
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 9-35
      Content: |resolved: int -> nat|
      references: []
      (x#49:14-15 -> x)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 14-15
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 30-35
      Content: |core: A.t|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 34-35
      Type definitions:
      (hmm#23:5-8 -> hmm)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 5-8
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 11-14
      Content: : |B.x|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 12-15
      (idk#29:5-8 -> idk)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 5-8
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 11-14
      Content: : |A.x|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 13-16
      (s#32:5-6 -> s)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 32, characters 5-6
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 32, characters 9-12
      Content: : |nat|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 8-9 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 25-26
      (q#33:8-9 -> q)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 33, characters 8-9
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 33, characters 12-21
      Content: : |funtype 'a : * . sum[Baz -> 'a]|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 13-14
      (qux#41:9-12 -> qux)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 41, characters 9-12
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 41, characters 15-19
      Content: : |bool|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 12-15
      (boo#44:12-15 -> boo)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 44, characters 12-15
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 44, characters 18-27
      Content: : |funtype 'a : * . option ('a)|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 16-19
      Module definitions:
      (A#1:7-8 -> A)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 1, characters 7-8
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 2, character 4 to line 15, character 10
      Content: Members: Variable definitions:
                        (a#5:8-9 -> a)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 8-9
                        Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 16-17
                        Content: |core: t|
                        references: []
                        (b#6:8-9 -> b)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 8-9
                        Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 23-38
                        Content: |core: x (string)|
                        references: []
                        (exp1#8:8-12 -> exp1)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 8, characters 8-12
                        Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 9, character 8 to line 15, character 10
                        Content: |resolved: unit|
                        references: []
                        (g#12:12-13 -> g)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 12-13
                        Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 22-29
                        Content: |core: foo|
                        references: []
                        (h#14:12-13 -> h)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 12-13
                        Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 29-46
                        Content: |core: bar (string)|
                        references: []
                        Type definitions:
                        (t#2:9-10 -> t)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 2, characters 9-10
                        Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 2, characters 13-16
                        Content: : |int|
                        references:
                          File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 12-13 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 10-11 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 10-11 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 20-21
                        (x#3:12-13 -> x)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 3, characters 12-13
                        Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 3, characters 16-28
                        Content: : |funtype 'a : * . record[foo -> 'a]|
                        references:
                          File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 19-20 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 13-14 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 13-14
                        (foo#9:13-16 -> foo)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 9, characters 13-16
                        Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 9, characters 19-25
                        Content: : |string|
                        references:
                          File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 16-19
                        (bar#10:16-19 -> bar)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 10, characters 16-19
                        Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 10, characters 22-34
                        Content: : |funtype 'a : * . record[bar -> 'a]|
                        references:
                          File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 23-26
                        Module definitions:

      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 18, characters 11-12 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 8-9 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 11-12 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 18-19

      (B#18:7-8 -> B)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 18, characters 7-8
      Body Range: File "../../test/contracts/get_scope_tests/types.mligo", line 18, characters 11-12
      Content: Alias: A#1:7-8
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 8-9 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 11-12

      Warnings:
      File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 12-13:
       13 |         // Module parametric type in
       14 |         let h : string bar = { bar = "World" } in
                        ^
       15 |         ()
      :
      Warning: unused variable "h".
      Hint: replace it by "_h" to prevent this warning.

      File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 12-13:
       11 |         // Module type in
       12 |         let g : foo = "Hello" in
                        ^
       13 |         // Module parametric type in
      :
      Warning: unused variable "g".
      Hint: replace it by "_g" to prevent this warning.

      File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 8-9:
       45 |     // paramertic type in
       46 |     let j : nat boo = Some 1n in
                    ^
       47 |     ()
      :
      Warning: unused variable "j".
      Hint: replace it by "_j" to prevent this warning.

      File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 8-9:
       42 |     // type in
       43 |     let i : qux = false in
                    ^
       44 |     type 'a boo = 'a option in
      :
      Warning: unused variable "i".
      Hint: replace it by "_i" to prevent this warning. |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "import_x.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ X#1:7-8 x#1:5-6 y#3:4-5 x#4:4-5 Mangled_module_____________________test__contracts__get_scope_tests__x____mligo#env  ] File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 8-13
    [  ] File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 9-33
    [ x#1:5-6  ] File "../../test/contracts/get_scope_tests/x.mligo", line 3, characters 8-9
    [ x#1:5-6 y#3:4-5  ] File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 14-15
    [ x#1:5-6 y#3:4-5  ] File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 22-24

    Variable definitions:
    (z#3:4-5 -> z)
    Range: File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 10-13
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (X#1:7-8 -> X)
    Range: File "../../test/contracts/get_scope_tests/import_x.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/import_x.mligo", line 1, characters 11-90
    Content: Alias: Mangled_module_____________________test__contracts__get_scope_tests__x____mligo#env
    references:
      File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 8-9

    (Mangled_module_____________________test__contracts__get_scope_tests__x____mligo#env -> Mangled_module_____________________test__contracts__get_scope_tests__x____mligo)
    Range:
    Body Range:
    Content: Members: Variable definitions:
                      (y#3:4-5 -> y)
                      Range: File "../../test/contracts/get_scope_tests/x.mligo", line 3, characters 4-5
                      Body Range: File "../../test/contracts/get_scope_tests/x.mligo", line 3, characters 8-9
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 14-15
                      (x#4:4-5 -> x)
                      Range: File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 4-5
                      Body Range: File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 8-25
                      Content: |resolved: x|
                      references:
                        File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 10-11
                      Type definitions:
                      (x#1:5-6 -> x)
                      Range: File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 5-6
                      Body Range: File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 9-33
                      Content: : |record[a -> int , b -> string]|
                      references: []
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/import_x.mligo", line 1, characters 11-90 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "local_module_using_local_binding.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 4, characters 12-13
    [ a#4:8-9  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 6, characters 16-17
    [ a#4:8-9 A#5:11-12 x#6:12-13  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 8, characters 4-7
    [ a#1:4-5 b#3:4-5  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 10, characters 8-9
    [ a#1:4-5 b#3:4-5 c#10:4-5  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 13, characters 12-13
    [ a#1:4-5 b#3:4-5 c#13:8-9  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 15, characters 16-17
    [ a#1:4-5 b#3:4-5 c#15:12-13  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 16, characters 16-17
    [ a#1:4-5 b#3:4-5 c#13:8-9 C#14:11-12 c#15:12-13 x#16:12-13  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 18, characters 4-7

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references: []
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 4, character 4 to line 8, character 7
    Content: |resolved: int|
    references: []
    (a#4:8-9 -> a)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 4, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 4, characters 12-13
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 6, characters 16-17
    (c#10:4-5 -> c)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 10, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 10, characters 8-9
    Content: |resolved: int|
    references: []
    (d#12:4-5 -> d)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 12, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 13, character 4 to line 18, character 7
    Content: |resolved: int|
    references: []
    (c#13:8-9 -> c)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 13, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 13, characters 12-13
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A#5:11-12 -> A)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 5, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 6, characters 8-17
    Content: Members: Variable definitions:
                      (x#6:12-13 -> x)
                      Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 6, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 6, characters 16-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 8, characters 6-7
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 8, characters 4-5

    (C#14:11-12 -> C)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 14, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 15, character 8 to line 16, character 17
    Content: Members: Variable definitions:
                      (c#15:12-13 -> c)
                      Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 15, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 15, characters 16-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 16, characters 16-17
                      (x#16:12-13 -> x)
                      Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 16, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 16, characters 16-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 18, characters 6-7
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 18, characters 4-5 |}]

(* Unresponsive "go-to-definition" VSCode extension command
   cause by [Location.compare] behaviour w.r.t. LSet / LMap
   (https://gitlab.com/ligolang/ligo/-/merge_requests/2532)
*)
let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "min_repr_2532/main.mligo"
    ; "--format"
    ; "json"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  let () =
    let open Yojson.Safe in
    from_string [%expect.output]
    |> Util.member "definitions"
    |> Util.member "types"
    |> Util.member "parameter#4:5-14"
    |> Util.member "references"
    |> pretty_to_string
    |> print_endline
  in
  [%expect
    {|
      [
        [
          "File",
          {
            "start": {
              "byte": {
                "pos_fname": "../../test/contracts/get_scope_tests/min_repr_2532/main.mligo",
                "pos_lnum": 3,
                "pos_bol": 1,
                "pos_cnum": 25
              },
              "point_num": 163,
              "point_bol": 138
            },
            "stop": {
              "byte": {
                "pos_fname": "../../test/contracts/get_scope_tests/min_repr_2532/main.mligo",
                "pos_lnum": 3,
                "pos_bol": 1,
                "pos_cnum": 34
              },
              "point_num": 172,
              "point_bol": 138
            }
          }
        ]
      ] |}]
