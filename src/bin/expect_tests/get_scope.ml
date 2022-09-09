open Cli_expect

let gs = fun s -> ("../../test/contracts/get_scope_tests/"^s)

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda_letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 8-9
    [ j#3 i#2 a#0  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
    [ g#4 j#3 i#2 a#0  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
    [ k#5 g#4 j#3 i#2 a#0  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
    [ f#1 a#0  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7

    Variable definitions:
    (a#0 -> a)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
    (b#6 -> b)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, character 2 to line 9, character 7
    Content: |resolved: int|
    references: []
    (f#1 -> f)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, character 32 to line 7, character 21
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
    (g#4 -> g)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
    (i#2 -> i)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, character 32 to line 7, character 21
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
    (j#3 -> j)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, character 4 to line 7, character 21
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
    (k#5 -> k)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
    Type definitions:
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "letin.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    [ a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ c#1 a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ e#3 c#1 a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ f#4 e#3 c#1 a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ d#2 c#1 a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11

    Variable definitions:
    (a#0 -> a)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#5 -> b)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, character 2 to line 10, character 11
    Content: |resolved: int|
    references: []
    (c#1 -> c)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#2 -> d)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, character 4 to line 8, character 17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#3 -> e)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#4 -> f)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    Type definitions:
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "lambda.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 8-9
    [ j#3 i#2 a#0  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
    [ f#1 a#0  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7

    Variable definitions:
    (a#0 -> a)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references: []
    (b#4 -> b)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, character 2 to line 5, character 7
    Content: |resolved: int|
    references: []
    (f#1 -> f)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 31-63
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
    (i#2 -> i)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 31-63
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
    (j#3 -> j)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47
    Body Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
    Type definitions:
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "match.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 8-9
    [ a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18
    [ c#2 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ x#3 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ y#4 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ b#5 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 11-19
    [ b#5 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 12-13
    [ c#6 b#5 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ hd#8 tl#7 b#5 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ c#9 b#5 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 17-18
    [ d#10 c#9 b#5 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ s#11 c#9 b#5 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ c#9 b#5 a#1 mytype#0  ] File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13

    Variable definitions:
    (a#1 -> a)
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
    (b#5 -> b)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 6, character 2 to line 8, character 18
    Content: |resolved: int|
    references: []
    (c#2 -> c)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    (c#6 -> c)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 12-13
    Content: |resolved: int|
    references: []
    (c#9 -> c)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 11, character 2 to line 15, character 15
    Content: |resolved: int|
    references: []
    (d#10 -> d)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 17-18
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    (d#12 -> d)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 18, character 2 to line 20, character 13
    Content: |resolved: int|
    references: []
    (hd#8 -> hd)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    Content: |resolved: int|
    references: []
    (s#11 -> s)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    (tl#7 -> tl)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    Content: |resolved: list (int)|
    references: []
    (x#3 -> x)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    (y#4 -> y)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    Content: |resolved: string|
    references: []
    Type definitions:
    (mytype#0 -> mytype)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 5-11
    Body Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 14-40
    Content: : sum[Bar -> string , Foo -> int]
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "rec.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 8-9
    [ c#1 a#0  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-40
    [ j#3 i#2 c#1 a#0  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ k#4 j#3 i#2 c#1 a#0  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-10
    [ m#7 n#6 z#5 c#1 a#0  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 7-36
    [ z#5 c#1 a#0  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 10-18
    [ v#8 z#5 c#1 a#0  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 10-11
    [ b#9 v#8 z#5 c#1 a#0  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 2-9
    [ y#12 x#11 b#10 a#0  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 16, character 5 to line 17, character 15

    Variable definitions:
    (a#0 -> a)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 5-6
    (b#10 -> b)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, character 2 to line 13, character 10
    Content: |resolved: int|
    references: []
    (b#9 -> b)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 10-11
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 8-9
    (c#1 -> c)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-40
    Content: |core: ( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 2-3
    (i#2 -> i)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, character 4 to line 6, character 11
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    (j#3 -> j)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, character 4 to line 6, character 11
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    (k#4 -> k)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    (m#7 -> m)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 23-24
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 4-36
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 18-19 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 35-36
    (n#6 -> n)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 13-14
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 23-24
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 7-8 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 28-29
    (v#8 -> v)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 10-18
    Content: |resolved: int|
    references: []
    (x#11 -> x)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 15, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 16, character 2 to line 17, character 16
    Content: |core: int -> unit|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 17, characters 7-8
    (y#12 -> y)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 15, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 16, character 2 to line 17, character 16
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 16, characters 5-6 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 17, characters 10-11
    (z#5 -> z)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 23-24
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 25-26 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 10-11
    Type definitions:
    Module definitions:
 |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "shadowing.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 8-9
    [ a#0  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ c#1 a#0  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ e#3 c#1 a#0  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ a#4 e#3 c#1  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ d#2 c#1 a#0  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11

    Variable definitions:
    (a#0 -> a)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    (a#4 -> a)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    (b#5 -> b)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, character 2 to line 10, character 11
    Content: |resolved: int|
    references: []
    (c#1 -> c)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    (d#2 -> d)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, character 4 to line 8, character 13
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    (e#3 -> e)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Type definitions:
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info" ; "get-scope" ; gs "records.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ myrec#0  ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 8-9
    [ a#1 myrec#0  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 22-23
    [ i#2 a#1 myrec#0  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 35-36
    [ j#3 i#2 a#1 myrec#0  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ a#1 myrec#0  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 53-55
    [ b#4 a#1 myrec#0  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4
    [ b#4 a#1 myrec#0  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24
    [ g#5 b#4 a#1 myrec#0  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41

    Variable definitions:
    (a#1 -> a)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    (b#4 -> b)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 8-56
    Content: |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    (e#6 -> e)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4
    Content: |resolved: myrec|
    references: []
    (g#5 -> g)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    (i#2 -> i)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 22-23
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    (j#3 -> j)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 35-36
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Type definitions:
    (myrec#0 -> myrec)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 5-10
    Body Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 13-36
    Content: : record[bar -> int , foo -> int]
    Module definitions: |} ] ;

  run_ligo_good [ "info" ; "get-scope" ; gs "constant.mligo" ; "--format" ; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ a#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 14
    [ c#1 a#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ d#2 c#1 a#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ e#3 a#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30

    Variable definitions:
    (a#0 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (b#4 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (c#1 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#2 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#3 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Type definitions:
    Module definitions: |} ] 

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "application.mligo" ; "--format";"dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ j#2 i#1  ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ b#3 f#0  ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ f#0  ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 11-31
    [ c#4 f#0  ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36

    Variable definitions:
    (a#5 -> a)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, character 2 to line 3, character 37
    Content: |resolved: int|
    references: []
    (b#3 -> b)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 11-12
    Content: |resolved: int|
    references: []
    (c#4 -> c)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    (f#0 -> f)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 31-63
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    (i#1 -> i)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 31-63
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    (j#2 -> j)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47
    Body Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Type definitions:
    Module definitions:
 |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "include.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ b#5 a#0  ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 8-9
    [ x#6 b#5 a#0  ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    [ a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ c#1 a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ e#3 c#1 a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ f#4 e#3 c#1 a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ d#2 c#1 a#0  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11

    Variable definitions:
    (a#0 -> a)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    (b#5 -> b)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, character 2 to line 10, character 11
    Content: |resolved: int|
    references: []
    (c#1 -> c)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    (d#2 -> d)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, character 4 to line 8, character 17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    (e#3 -> e)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    (f#4 -> f)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    (x#6 -> x)
    Range: File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    (y#7 -> y)
    Range: File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions: |} ]

let%expect_test _ =
  run_ligo_good [ "info"; "get-scope" ; gs "bad_field_record.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
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
    Content: : record[bar -> int , foo -> int]
    Module definitions: |} ];

  run_ligo_good [ "info"; "get-scope" ; gs "nominal_types.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [ foo_record#1 foo_variant#0  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ a#2 foo_record#1 foo_variant#0  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ b#3 a#2 foo_record#1 foo_variant#0  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    [ b#3 a#2 foo_record#1 foo_variant#0  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    [ p#5 c#4 b#3 a#2 foo_record#1 foo_variant#0  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43

    Variable definitions:
    (a#2 -> a)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 8-13
    Content: |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    (b#3 -> b)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 8-14
    Content: |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    (c#4 -> c)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, character 8 to line 11, character 1
    Content: |resolved: foo_record|
    references: []
    (main#6 -> main)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11
    Content: |core: foo_record -> foo_variant|
    references: []
    (p#5 -> p)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-47
    Content: |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 42-43
    Type definitions:
    (foo_record#1 -> foo_record)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 5-15
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 18-58
    Content: : record[bar -> foo_variant , foo -> foo_variant]
    (foo_variant#0 -> foo_variant)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 5-16
    Body Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 19-45
    Content: : sum[Bar -> string , Foo -> int]
    Module definitions: |} ]

let%expect_test _ =    
  run_ligo_good [ "info"; "get-scope" ; gs "module.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 15-16
    [ A#1 toto#0  ] File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 8-14
    [ B#3 a#2 A#1 toto#0  ] File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 8-14
    [ b#4 B#3 a#2 A#1 toto#0  ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 16-17
    [ D#7 C#6 a#5 b#4 B#3 a#2 A#1 toto#0  ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7

    Variable definitions:
    (a#2 -> a)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 8-14
    Content: |resolved: int|
    references: []
    (b#4 -> b)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 8-14
    Content: |resolved: int|
    references: []
    (titi#8 -> titi)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 16, character 7
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A#1 -> A)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3
    Content: Members: Variable definitions:
                      (toto#0 -> toto)
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

    (B#3 -> B)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 11-12
    Content: Alias: A#1
    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 8-9

    (C#6 -> C)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 12, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 14, character 7
    Content: Members: Variable definitions:
                      (a#5 -> a)
                      Range: File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 16-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 6-7
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 15, characters 15-16

    (D#7 -> D)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 15, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module.mligo", line 15, characters 15-16
    Content: Alias: C#6
    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-5 |} ] ;

  run_ligo_good [ "info"; "get-scope" ; gs "module2.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 2, characters 12-13
    [ x#0  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 4, characters 16-17
    [ F#7 D#6 C#5 A#4 E#3 B#2 y#1 x#0  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 9-14
    [ a1#8 F#7 D#6 C#5 A#4 E#3 B#2 y#1 x#0  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 9-12
    [ a2#9 a1#8 F#7 D#6 C#5 A#4 E#3 B#2 y#1 x#0  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 9-12
    [ a3#10 a2#9 a1#8 F#7 D#6 C#5 A#4 E#3 B#2 y#1 x#0  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 9-12
    [ a4#11 a3#10 a2#9 a1#8 F#7 D#6 C#5 A#4 E#3 B#2 y#1 x#0  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 9-12

    Variable definitions:
    (a1#8 -> a1)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 9-14
    Content: |resolved: int|
    references: []
    (a2#9 -> a2)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 9-12
    Content: |resolved: int|
    references: []
    (a3#10 -> a3)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 9-12
    Content: |resolved: int|
    references: []
    (a4#11 -> a4)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 9-12
    Content: |resolved: int|
    references: []
    (a5#12 -> a5)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 9-12
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A#4 -> A)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 1, character 0 to line 7, character 3
    Content: Members: Variable definitions:
                      (x#0 -> x)
                      Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 2, characters 8-9
                      Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 2, characters 12-13
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 11-12
                      Type definitions:
                      Module definitions:
                      (B#2 -> B)
                      Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 3, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 3, character 4 to line 5, character 7
                      Content: Members: Variable definitions:
                                        (y#1 -> y)
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

                      (E#3 -> E)
                      Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 6, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 6, characters 15-16
                      Content: Alias: B#2
                      references:
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 13-14


    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 9-10

    (C#5 -> C)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 11-14
    Content: Alias: A#4.B#2
    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 9-10

    (D#6 -> D)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 11-12
    Content: Alias: A#4
    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 9-10

    (F#7 -> F)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 11-14
    Content: Alias: A#4.E#3
    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 9-10 |}] ;

  run_ligo_good [ "info"; "get-scope" ; gs "module3.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 12-13
    [ A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 7, characters 16-17
    [ y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 16-19
    [ z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 21-22
    [ a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 21-22
    [ a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 12, characters 21-22
    [ E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 13-16
    [ b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 13-16
    [ b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 13-19
    [ b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 13-19
    [ b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 13-19
    [ b5#15 b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 13-16
    [ c1#16 b5#15 b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 13-16
    [ c2#17 c1#16 b5#15 b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 13-19
    [ c3#18 c2#17 c1#16 b5#15 b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 13-19
    [ c4#19 c3#18 c2#17 c1#16 b5#15 b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 13-19
    [ c5#20 c4#19 c3#18 c2#17 c1#16 b5#15 b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 13-17
    [ e1#21 c5#20 c4#19 c3#18 c2#17 c1#16 b5#15 b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 13-17
    [ e2#22 e1#21 c5#20 c4#19 c3#18 c2#17 c1#16 b5#15 b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 13-17
    [ e3#23 e2#22 e1#21 c5#20 c4#19 c3#18 c2#17 c1#16 b5#15 b4#14 b3#13 b2#12 b1#11 E#10 D#9 B#8 C#7 a3#6 a2#5 a1#4 z#3 y#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 35, characters 4-6

    Variable definitions:
    (b1#11 -> b1)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 13-16
    Content: |resolved: int|
    references: []
    (b2#12 -> b2)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 13-16
    Content: |resolved: int|
    references: []
    (b3#13 -> b3)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 13-19
    Content: |resolved: int|
    references: []
    (b4#14 -> b4)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 13-19
    Content: |resolved: int|
    references: []
    (b5#15 -> b5)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 13-19
    Content: |resolved: int|
    references: []
    (c1#16 -> c1)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 13-16
    Content: |resolved: int|
    references: []
    (c2#17 -> c2)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 13-16
    Content: |resolved: int|
    references: []
    (c3#18 -> c3)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 13-19
    Content: |resolved: int|
    references: []
    (c4#19 -> c4)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 13-19
    Content: |resolved: int|
    references: []
    (c5#20 -> c5)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 13-19
    Content: |resolved: int|
    references: []
    (e1#21 -> e1)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 13-17
    Content: |resolved: int|
    references: []
    (e2#22 -> e2)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 13-17
    Content: |resolved: int|
    references: []
    (e3#23 -> e3)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 8-10
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 13-17
    Content: |resolved: int|
    references: []
    (x#24 -> x)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 5, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 6, character 4 to line 35, character 6
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A#1 -> A)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 1, character 0 to line 3, character 3
    Content: Members: Variable definitions:
                      (x#0 -> x)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 8-9
                      Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 12-13
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 18-19
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 16-17

    (B#8 -> B)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 6, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 6, character 4 to line 14, character 7
    Content: Members: Variable definitions:
                      (y#2 -> y)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 7, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 7, characters 16-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 21-22 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 15-16
                      (z#3 -> z)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 16-19
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 21-22 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 15-16
                      Type definitions:
                      Module definitions:
                      (C#7 -> C)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 9, characters 15-16
                      Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 9, character 8 to line 13, character 11
                      Content: Members: Variable definitions:
                                        (a1#4 -> a1)
                                        Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 16-18
                                        Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 21-22
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 15-17
                                        (a2#5 -> a2)
                                        Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 16-18
                                        Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 21-22
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 15-17
                                        (a3#6 -> a3)
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

    (D#9 -> D)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 16, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 16, characters 15-16
    Content: Alias: B#8
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 13-14

    (E#10 -> E)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 15-18
    Content: Alias: B#8.C#7
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 13-14 |}] ;

  run_ligo_good [ "info"; "get-scope" ; gs "module4.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module4.mligo", line 2, characters 12-13
    [ a#0  ] File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 16-17
    [ B#2 y#1 a#0  ] File "../../test/contracts/get_scope_tests/module4.mligo", line 6, characters 4-7

    Variable definitions:
    (a#0 -> a)
    Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 2, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 2, characters 12-13
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 16-17
    (x#3 -> x)
    Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 2, character 4 to line 6, character 7
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (B#2 -> B)
    Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 3, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 3, character 4 to line 5, character 7
    Content: Members: Variable definitions:
                      (y#1 -> y)
                      Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 12-13
                      Body Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 16-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module4.mligo", line 6, characters 6-7
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module4.mligo", line 6, characters 4-5 |}];

  run_ligo_good [ "info"; "get-scope" ; gs "module5.mligo" ; "--format"; "dev" ; "--with-types" ] ;
  [%expect{|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 4, characters 20-21
    [ G#7 D#6 A#5 E#4 B#3 F#2 C#1 x#0  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 9-16
    [ x1#8 G#7 D#6 A#5 E#4 B#3 F#2 C#1 x#0  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 9-16
    [ x2#9 x1#8 G#7 D#6 A#5 E#4 B#3 F#2 C#1 x#0  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 9-16
    [ x3#10 x2#9 x1#8 G#7 D#6 A#5 E#4 B#3 F#2 C#1 x#0  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 9-16
    [ x4#11 x3#10 x2#9 x1#8 G#7 D#6 A#5 E#4 B#3 F#2 C#1 x#0  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 9-16
    [ x5#12 x4#11 x3#10 x2#9 x1#8 G#7 D#6 A#5 E#4 B#3 F#2 C#1 x#0  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 9-16
    [ x6#13 x5#12 x4#11 x3#10 x2#9 x1#8 G#7 D#6 A#5 E#4 B#3 F#2 C#1 x#0  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 9-16
    [ x7#14 x6#13 x5#12 x4#11 x3#10 x2#9 x1#8 G#7 D#6 A#5 E#4 B#3 F#2 C#1 x#0  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 9-16

    Variable definitions:
    (x1#8 -> x1)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 9-16
    Content: |resolved: int|
    references: []
    (x2#9 -> x2)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 9-16
    Content: |resolved: int|
    references: []
    (x3#10 -> x3)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 9-16
    Content: |resolved: int|
    references: []
    (x4#11 -> x4)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 9-16
    Content: |resolved: int|
    references: []
    (x5#12 -> x5)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 9-16
    Content: |resolved: int|
    references: []
    (x6#13 -> x6)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 9-16
    Content: |resolved: int|
    references: []
    (x7#14 -> x7)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 9-16
    Content: |resolved: int|
    references: []
    (x8#15 -> x8)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 9-16
    Content: |resolved: int|
    references: []
    Type definitions:
    Module definitions:
    (A#5 -> A)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 1, character 0 to line 9, character 3
    Content: Members: Variable definitions:
                      Type definitions:
                      Module definitions:
                      (B#3 -> B)
                      Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 2, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 2, character 4 to line 7, character 7
                      Content: Members: Variable definitions:
                                        Type definitions:
                                        Module definitions:
                                        (C#1 -> C)
                                        Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 3, characters 15-16
                                        Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 3, character 8 to line 5, character 11
                                        Content: Members: Variable definitions:
                                                          (x#0 -> x)
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

                                        (F#2 -> F)
                                        Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 6, characters 15-16
                                        Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 6, characters 19-20
                                        Content: Alias: C#1
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

                      (E#4 -> E)
                      Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 8, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 8, characters 15-16
                      Content: Alias: B#3
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

    (D#6 -> D)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 11, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 11, characters 11-12
    Content: Alias: A#5
    references:
      File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 9-10

    (G#7 -> G)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 11-16
    Content: Alias: D#6.E#4.F#2
    references: [] |}] ;

  run_ligo_good [ "info"; "get-scope" ; gs "module_shadowing.mligo" ; "--format"; "dev" ; "--with-types" ] ;
    [%expect{|
      Scopes:
      [  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 2, characters 12-13
      [ D#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, characters 12-13
      [ x#3 D#2 A#1 x#0  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 16-19
      [ C#6 A#5 x#4 x#3 D#2  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 4-11

      Variable definitions:
      (x#3 -> x)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, characters 8-9
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, characters 12-13
      Content: |resolved: int|
      references:
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 4-5
      (y#7 -> y)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 7, characters 4-5
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, character 4 to line 13, character 11
      Content: |resolved: int|
      references: []
      Type definitions:
      Module definitions:
      (A#1 -> A)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 1, characters 7-8
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 1, character 0 to line 3, character 3
      Content: Members: Variable definitions:
                        (x#0 -> x)
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

      (A#5 -> A)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 9, characters 11-12
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 9, character 4 to line 11, character 7
      Content: Members: Variable definitions:
                        (x#4 -> x)
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

      (C#6 -> C)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 12, characters 11-12
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 12, characters 15-16
      Content: Alias: A#5
      references: []

      (D#2 -> D)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 5, characters 7-8
      Body Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 5, characters 11-12
      Content: Alias: A#1
      references: [] |}] ;