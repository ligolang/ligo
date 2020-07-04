open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/"^s

let%expect_test _ =
  run_ligo_good [ "get-scope" ; gs "lambda_letin.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f#5 a#0 ] in file "lambda_letin.mligo", line 9, characters 6-7
    [ f#5 a#0 ] in file "lambda_letin.mligo", line 9, characters 4-5
    [ f#5 a#0 ] in file "lambda_letin.mligo", line 9, characters 2-3
    [ k#4 j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 7, characters 20-21
    [ k#4 j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 7, characters 16-17
    [ k#4 j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 7, characters 12-13
    [ k#4 j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 7, characters 8-9
    [ k#4 j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 7, characters 4-5
    [ j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 6, characters 24-25
    [ j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 6, characters 20-21
    [ j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 6, characters 16-17
    [ j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 6, characters 12-13
    [ j#2 i#1 a#0 ] in file "lambda_letin.mligo", line 5, characters 20-21
    [ j#2 i#1 a#0 ] in file "lambda_letin.mligo", line 5, characters 16-17
    [ j#2 i#1 a#0 ] in file "lambda_letin.mligo", line 5, characters 12-13
    [ ] in file "lambda_letin.mligo", line 1, characters 0-9

    Variable definitions:
    (k#4 -> k) in file "lambda_letin.mligo", line 6, characters 8-9
    (j#2 -> j) in file "lambda_letin.mligo", line 4, characters 47-48
    (i#1 -> i) in file "lambda_letin.mligo", line 4, characters 37-38
    (g#3 -> g) in file "lambda_letin.mligo", line 5, characters 8-9
    (f#5 -> f) in file "lambda_letin.mligo", line 4, characters 6-7
    (b#6 -> b) in file "lambda_letin.mligo", line 3, characters 4-5
    (a#0 -> a) in file "lambda_letin.mligo", line 1, characters 4-5
    Type definitions: |} ];

  run_ligo_good [ "get-scope" ; gs "letin.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ d#4 c#1 a#0 ] in file "letin.mligo", line 10, characters 10-11
    [ d#4 c#1 a#0 ] in file "letin.mligo", line 10, characters 6-7
    [ d#4 c#1 a#0 ] in file "letin.mligo", line 10, characters 2-3
    [ f#3 e#2 c#1 a#0 ] in file "letin.mligo", line 8, characters 16-17
    [ f#3 e#2 c#1 a#0 ] in file "letin.mligo", line 8, characters 12-13
    [ f#3 e#2 c#1 a#0 ] in file "letin.mligo", line 8, characters 8-9
    [ f#3 e#2 c#1 a#0 ] in file "letin.mligo", line 8, characters 4-5
    [ e#2 c#1 a#0 ] in file "letin.mligo", line 7, characters 20-21
    [ e#2 c#1 a#0 ] in file "letin.mligo", line 7, characters 16-17
    [ e#2 c#1 a#0 ] in file "letin.mligo", line 7, characters 12-13
    [ c#1 a#0 ] in file "letin.mligo", line 6, characters 16-17
    [ c#1 a#0 ] in file "letin.mligo", line 6, characters 12-13
    [ a#0 ] in file "letin.mligo", line 4, characters 14-15
    [ a#0 ] in file "letin.mligo", line 4, characters 10-11
    [ ] in file "letin.mligo", line 1, characters 0-9

    Variable definitions:
    (f#3 -> f) in file "letin.mligo", line 7, characters 8-9
    (e#2 -> e) in file "letin.mligo", line 6, characters 8-9
    (d#4 -> d) in file "letin.mligo", line 5, characters 6-7
    (c#1 -> c) in file "letin.mligo", line 4, characters 6-7
    (b#5 -> b) in file "letin.mligo", line 3, characters 4-5
    (a#0 -> a) in file "letin.mligo", line 1, characters 4-5
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "lambda.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f#3 a#0 ] in file "lambda.mligo", line 5, characters 6-7
    [ f#3 a#0 ] in file "lambda.mligo", line 5, characters 4-5
    [ f#3 a#0 ] in file "lambda.mligo", line 5, characters 2-3
    [ j#2 i#1 a#0 ] in file "lambda.mligo", line 4, characters 62-63
    [ j#2 i#1 a#0 ] in file "lambda.mligo", line 4, characters 58-59
    [ ] in file "lambda.mligo", line 1, characters 0-9

    Variable definitions:
    (j#2 -> j) in file "lambda.mligo", line 4, characters 46-47
    (i#1 -> i) in file "lambda.mligo", line 4, characters 36-37
    (f#3 -> f) in file "lambda.mligo", line 4, characters 6-7
    (b#4 -> b) in file "lambda.mligo", line 3, characters 4-5
    (a#0 -> a) in file "lambda.mligo", line 1, characters 4-5
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "match.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ s#11 mytype#0 c#9 b#5 a#1 ] in file "match.mligo", line 19, characters 20-21
    [ s#11 mytype#0 c#9 b#5 a#1 ] in file "match.mligo", line 19, characters 16-17
    [ mytype#0 c#9 b#5 a#1 ] in file "match.mligo", line 20, characters 12-13
    [ mytype#0 d#10 c#9 b#5 a#1 ] in file "match.mligo", line 18, characters 30-31
    [ mytype#0 d#10 c#9 b#5 a#1 ] in file "match.mligo", line 18, characters 28-29
    [ mytype#0 c#9 b#5 a#1 ] in file "match.mligo", line 18, characters 9-32
    [ tl#8 mytype#0 hd#7 b#5 a#1 ] in file "match.mligo", line 15, characters 14-15
    [ mytype#0 c#6 b#5 a#1 ] in file "match.mligo", line 14, characters 4-5
    [ mytype#0 b#5 a#1 ] in file "match.mligo", line 13, character 4 to line 14, character 5
    [ mytype#0 b#5 a#1 ] in file "match.mligo", line 11, characters 18-19
    [ mytype#0 b#5 a#1 ] in file "match.mligo", line 11, characters 15-16
    [ mytype#0 b#5 a#1 ] in file "match.mligo", line 11, characters 11-12
    [ y#4 mytype#0 a#1 ] in file "match.mligo", line 8, characters 17-18
    [ y#4 mytype#0 a#1 ] in file "match.mligo", line 8, characters 13-14
    [ x#3 mytype#0 a#1 ] in file "match.mligo", line 7, characters 17-18
    [ x#3 mytype#0 a#1 ] in file "match.mligo", line 7, characters 13-14
    [ mytype#0 c#2 a#1 ] in file "match.mligo", line 6, characters 26-27
    [ mytype#0 a#1 ] in file "match.mligo", line 6, characters 9-27
    [ mytype#0 ] in file "match.mligo", line 3, characters 0-9

    Variable definitions:
    (y#4 -> y) in file "match.mligo", line 8, characters 8-9
    (x#3 -> x) in file "match.mligo", line 7, characters 8-9
    (tl#8 -> tl) in file "match.mligo", line 15, characters 8-10
    (s#11 -> s) in file "match.mligo", line 19, characters 10-11
    (hd#7 -> hd) in file "match.mligo", line 15, characters 4-6
    (d#12 -> d) in file "match.mligo", line 17, characters 4-5
    (d#10 -> d) in file "match.mligo", line 18, characters 13-14
    (c#9 -> c) in file "match.mligo", line 10, characters 4-5
    (c#6 -> c) in file "match.mligo", line 13, characters 8-9
    (c#2 -> c) in file "match.mligo", line 6, characters 13-14
    (b#5 -> b) in file "match.mligo", line 5, characters 4-5
    (a#1 -> a) in file "match.mligo", line 3, characters 4-5
    Type definitions:
    (mytype#0 -> mytype) in file "match.mligo", line 1, characters 0-40 |} ] ;

  run_ligo_good [ "get-scope" ; gs "rec.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ c#5 b#6 a#0 ] in file "rec.mligo", line 9, characters 5-6
    [ c#5 b#6 a#0 ] in file "rec.mligo", line 9, characters 8-9
    [ c#5 b#6 a#0 ] in file "rec.mligo", line 9, characters 2-3
    [ c#5 a#0 ] in file "rec.mligo", line 8, character 2 to line 9, character 10
    [ k#4 j#3 i#2 c#1 a#0 ] in file "rec.mligo", line 6, characters 7-8
    [ k#4 j#3 i#2 c#1 a#0 ] in file "rec.mligo", line 6, characters 9-10
    [ k#4 j#3 i#2 c#1 a#0 ] in file "rec.mligo", line 6, characters 4-5
    [ j#3 i#2 c#1 a#0 ] in file "rec.mligo", line 5, characters 20-21
    [ j#3 i#2 c#1 a#0 ] in file "rec.mligo", line 5, characters 16-17
    [ j#3 i#2 c#1 a#0 ] in file "rec.mligo", line 5, characters 12-13
    [ i#2 c#1 a#0 ] in file "rec.mligo", line 4, characters 36-49
    [ c#1 a#0 ] in file "rec.mligo", line 4, characters 36-49
    [ c#1 a#0 ]
    [ ] in file "rec.mligo", line 1, characters 0-9

    Variable definitions:
    (k#4 -> k) in file "rec.mligo", line 5, characters 8-9
    (j#3 -> j) in file "rec.mligo", line 4, characters 39-40
    (i#2 -> i) in file "rec.mligo", line 4, characters 37-38
    (c#5 -> c) in file "rec.mligo", line 4, characters 10-11
    (c#1 -> c) in file "rec.mligo", line 4, characters 10-11
    (b#7 -> b) in file "rec.mligo", line 3, characters 4-5
    (b#6 -> b) in file "rec.mligo", line 8, characters 6-7
    (a#0 -> a) in file "rec.mligo", line 1, characters 4-5
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "shadowing.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ d#4 c#1 a#0 ] in file "shadowing.mligo", line 10, characters 10-11
    [ d#4 c#1 a#0 ] in file "shadowing.mligo", line 10, characters 6-7
    [ d#4 c#1 a#0 ] in file "shadowing.mligo", line 10, characters 2-3
    [ e#2 c#1 a#3 ] in file "shadowing.mligo", line 8, characters 12-13
    [ e#2 c#1 a#3 ] in file "shadowing.mligo", line 8, characters 8-9
    [ e#2 c#1 a#3 ] in file "shadowing.mligo", line 8, characters 4-5
    [ e#2 c#1 a#0 ] in file "shadowing.mligo", line 7, characters 20-21
    [ e#2 c#1 a#0 ] in file "shadowing.mligo", line 7, characters 16-17
    [ e#2 c#1 a#0 ] in file "shadowing.mligo", line 7, characters 12-13
    [ c#1 a#0 ] in file "shadowing.mligo", line 6, characters 16-17
    [ c#1 a#0 ] in file "shadowing.mligo", line 6, characters 12-13
    [ a#0 ] in file "shadowing.mligo", line 4, characters 14-15
    [ a#0 ] in file "shadowing.mligo", line 4, characters 10-11
    [ ] in file "shadowing.mligo", line 1, characters 0-9

    Variable definitions:
    (e#2 -> e) in file "shadowing.mligo", line 6, characters 8-9
    (d#4 -> d) in file "shadowing.mligo", line 5, characters 6-7
    (c#1 -> c) in file "shadowing.mligo", line 4, characters 6-7
    (b#5 -> b) in file "shadowing.mligo", line 3, characters 4-5
    (a#3 -> a) in file "shadowing.mligo", line 7, characters 8-9
    (a#0 -> a) in file "shadowing.mligo", line 1, characters 4-5
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "records.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ myrec#0 g#5 b#4 a#1 ] in file "records.mligo", line 16, characters 40-41
    [ myrec#0 g#5 b#4 a#1 ] in file "records.mligo", line 16, characters 32-37
    [ myrec#0 g#5 b#4 a#1 ] in file "records.mligo", line 16, characters 28-29
    [ myrec#0 b#4 a#1 ] in file "records.mligo", line 16, characters 15-41
    [ myrec#0 b#4 a#1 ]
    [ myrec#0 a#1 ] in file "records.mligo", line 6, characters 53-55
    [ myrec#0 j#3 i#2 a#1 ] in file "records.mligo", line 6, characters 44-45
    [ myrec#0 j#3 i#2 a#1 ] in file "records.mligo", line 6, characters 42-43
    [ myrec#0 j#3 i#2 a#1 ] in file "records.mligo", line 6, characters 40-41
    [ myrec#0 i#2 a#1 ] in file "records.mligo", line 6, characters 27-45
    [ myrec#0 a#1 ] in file "records.mligo", line 6, characters 14-45
    [ myrec#0 ] in file "records.mligo", line 3, characters 0-9

    Variable definitions:
    (j#3 -> j) in file "records.mligo", line 6, characters 31-32
    (i#2 -> i) in file "records.mligo", line 6, characters 18-19
    (g#5 -> g) in file "records.mligo", line 16, characters 19-20
    (e#6 -> e) in file "records.mligo", line 15, characters 4-5
    (b#4 -> b) in file "records.mligo", line 6, characters 4-5
    (a#1 -> a) in file "records.mligo", line 3, characters 4-5
    Type definitions:
    (myrec#0 -> myrec) in file "records.mligo", line 1, characters 0-36 |} ] ;

  run_ligo_good [ "get-scope" ; gs "constant.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ e#3 a#0 ] in file "constant.mligo", line 6, characters 29-30
    [ e#3 a#0 ] in file "constant.mligo", line 6, characters 27-28
    [ e#3 a#0 ] in file "constant.mligo", line 6, characters 22-23
    [ e#3 a#0 ] in file "constant.mligo", line 6, characters 20-21
    [ a#0 ] in file "constant.mligo", line 6, characters 5-32
    [ d#2 c#1 a#0 ] in file "constant.mligo", line 5, characters 43-44
    [ d#2 c#1 a#0 ] in file "constant.mligo", line 5, characters 39-40
    [ d#2 c#1 a#0 ] in file "constant.mligo", line 5, characters 35-36
    [ c#1 a#0 ] in file "constant.mligo", line 5, characters 22-44
    [ ] in file "constant.mligo", line 1, characters 0-9

    Variable definitions:
    (e#3 -> e) in file "constant.mligo", line 6, characters 9-10
    (d#2 -> d) in file "constant.mligo", line 5, characters 26-27
    (c#1 -> c) in file "constant.mligo", line 5, characters 10-11
    (b#4 -> b) in file "constant.mligo", line 3, characters 4-5
    (a#0 -> a) in file "constant.mligo", line 1, characters 4-5
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "application.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f#2 c#4 ] in file "application.mligo", line 3, characters 35-36
    [ f#2 ] in file "application.mligo", line 3, characters 22-36
    [ f#2 b#3 ] in file "application.mligo", line 3, characters 18-19
    [ f#2 b#3 ] in file "application.mligo", line 3, characters 16-17
    [ f#2 ] in file "application.mligo", line 3, characters 3-19
    [ j#1 i#0 ] in file "application.mligo", line 2, characters 62-63
    [ j#1 i#0 ] in file "application.mligo", line 2, characters 58-59

    Variable definitions:
    (j#1 -> j) in file "application.mligo", line 2, characters 46-47
    (i#0 -> i) in file "application.mligo", line 2, characters 36-37
    (f#2 -> f) in file "application.mligo", line 2, characters 6-7
    (c#4 -> c) in file "application.mligo", line 3, characters 26-27
    (b#3 -> b) in file "application.mligo", line 3, characters 7-8
    (a#5 -> a) in file "application.mligo", line 1, characters 4-5
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "include.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ x#6 b#5 a#0 ] in file "include.mligo", line 5, characters 12-13
    [ x#6 b#5 a#0 ] in file "include.mligo", line 5, characters 8-9
    [ b#5 a#0 ] in file "include.mligo", line 3, characters 0-9
    [ d#4 c#1 a#0 ] in file "letin.mligo", line 10, characters 10-11
    [ d#4 c#1 a#0 ] in file "letin.mligo", line 10, characters 6-7
    [ d#4 c#1 a#0 ] in file "letin.mligo", line 10, characters 2-3
    [ f#3 e#2 c#1 a#0 ] in file "letin.mligo", line 8, characters 16-17
    [ f#3 e#2 c#1 a#0 ] in file "letin.mligo", line 8, characters 12-13
    [ f#3 e#2 c#1 a#0 ] in file "letin.mligo", line 8, characters 8-9
    [ f#3 e#2 c#1 a#0 ] in file "letin.mligo", line 8, characters 4-5
    [ e#2 c#1 a#0 ] in file "letin.mligo", line 7, characters 20-21
    [ e#2 c#1 a#0 ] in file "letin.mligo", line 7, characters 16-17
    [ e#2 c#1 a#0 ] in file "letin.mligo", line 7, characters 12-13
    [ c#1 a#0 ] in file "letin.mligo", line 6, characters 16-17
    [ c#1 a#0 ] in file "letin.mligo", line 6, characters 12-13
    [ a#0 ] in file "letin.mligo", line 4, characters 14-15
    [ a#0 ] in file "letin.mligo", line 4, characters 10-11
    [ ] in file "letin.mligo", line 1, characters 0-9

    Variable definitions:
    (y#7 -> y) in file "include.mligo", line 5, characters 4-5
    (x#6 -> x) in file "include.mligo", line 3, characters 4-5
    (f#3 -> f) in file "letin.mligo", line 7, characters 8-9
    (e#2 -> e) in file "letin.mligo", line 6, characters 8-9
    (d#4 -> d) in file "letin.mligo", line 5, characters 6-7
    (c#1 -> c) in file "letin.mligo", line 4, characters 6-7
    (b#5 -> b) in file "letin.mligo", line 3, characters 4-5
    (a#0 -> a) in file "letin.mligo", line 1, characters 4-5
    Type definitions: |} ] ;