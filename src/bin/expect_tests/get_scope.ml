open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/"^s

let%expect_test _ =
  run_ligo_good [ "get-scope" ; gs "lambda_letin.mligo" ; "--syntax=cameligo" ; "--format=dev" ] ;
  [%expect {|
Scopes:
[ a#0 f#5 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
[ a#0 f#5 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 4-5
[ a#0 f#5 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
[ a#0 g#3 i#1 j#2 k#4 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
[ a#0 g#3 i#1 j#2 k#4 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
[ a#0 g#3 i#1 j#2 k#4 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13
[ a#0 g#3 i#1 j#2 k#4 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
[ a#0 g#3 i#1 j#2 k#4 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
[ a#0 g#3 i#1 j#2 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25
[ a#0 g#3 i#1 j#2 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21
[ a#0 g#3 i#1 j#2 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17
[ a#0 g#3 i#1 j#2 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13
[ a#0 i#1 j#2 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21
[ a#0 i#1 j#2 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17
[ a#0 i#1 j#2 ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13
[ ] in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9

Variable definitions:
(a#0 -> a) in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5
(b#6 -> b) in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5
(f#5 -> f) in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7
(g#3 -> g) in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9
(i#1 -> i) in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 36-45
(j#2 -> j) in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 46-55
(k#4 -> k) in file "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9
Type definitions: |} ];

  run_ligo_good [ "get-scope" ; gs "letin.mligo" ; "--syntax=cameligo" ; "--format=dev" ] ;
  [%expect {|
Scopes:
[ a#0 c#1 d#4 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
[ a#0 c#1 d#4 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
[ a#0 c#1 d#4 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
[ a#0 c#1 e#2 f#3 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
[ a#0 c#1 e#2 f#3 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
[ a#0 c#1 e#2 f#3 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9
[ a#0 c#1 e#2 f#3 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5
[ a#0 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21
[ a#0 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17
[ a#0 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13
[ a#0 c#1 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17
[ a#0 c#1 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13
[ a#0 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15
[ a#0 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-11
[ ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9

Variable definitions:
(a#0 -> a) in file "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5
(b#5 -> b) in file "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5
(c#1 -> c) in file "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7
(d#4 -> d) in file "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7
(e#2 -> e) in file "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9
(f#3 -> f) in file "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9
Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "lambda.mligo" ; "--syntax=cameligo" ; "--format=dev" ] ;
  [%expect {|
Scopes:
[ a#0 f#3 ] in file "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 6-7
[ a#0 f#3 ] in file "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 4-5
[ a#0 f#3 ] in file "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
[ a#0 i#1 j#2 ] in file "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
[ a#0 i#1 j#2 ] in file "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
[ ] in file "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9

Variable definitions:
(a#0 -> a) in file "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5
(b#4 -> b) in file "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5
(f#3 -> f) in file "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7
(i#1 -> i) in file "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 35-44
(j#2 -> j) in file "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 45-54
Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "match.mligo" ; "--syntax=cameligo" ; "--format=dev" ] ;
  [%expect {|
Scopes:
[ a#1 b#5 c#9 mytype#0 s#11 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21
[ a#1 b#5 c#9 mytype#0 s#11 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
[ a#1 b#5 c#9 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
[ a#1 b#5 c#9 d#10 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
[ a#1 b#5 c#9 d#10 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29
[ a#1 b#5 c#9 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 9-32
[ a#1 b#5 hd#7 mytype#0 tl#8 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
[ a#1 b#5 c#6 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
[ a#1 b#5 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 13, character 4 to line 14, character 5
[ a#1 b#5 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 18-19
[ a#1 b#5 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 15-16
[ a#1 b#5 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 11-12
[ a#1 mytype#0 y#4 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18
[ a#1 mytype#0 y#4 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-14
[ a#1 mytype#0 x#3 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18
[ a#1 mytype#0 x#3 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
[ a#1 c#2 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
[ a#1 mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27
[ mytype#0 ] in file "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9

Variable definitions:
(a#1 -> a) in file "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5
(b#5 -> b) in file "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5
(c#2 -> c) in file "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14
(c#6 -> c) in file "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9
(c#9 -> c) in file "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5
(d#10 -> d) in file "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14
(d#12 -> d) in file "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5
(hd#7 -> hd) in file "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6
(s#11 -> s) in file "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 4-12
(tl#8 -> tl) in file "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6
(x#3 -> x) in file "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 4-9
(y#4 -> y) in file "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 4-9
Type definitions:
(mytype#0 -> mytype) in file "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40 |} ] ;

  run_ligo_good [ "get-scope" ; gs "rec.mligo" ; "--syntax=cameligo" ; "--format=dev" ] ;
  [%expect {|
Scopes:
[ a#0 b#6 c#5 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9
[ a#0 b#6 c#5 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6
[ a#0 b#6 c#5 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3
[ a#0 c#5 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10
[ a#0 c#1 i#2 j#3 k#4 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 9-10
[ a#0 c#1 i#2 j#3 k#4 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
[ a#0 c#1 i#2 j#3 k#4 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5
[ a#0 c#1 i#2 j#3 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21
[ a#0 c#1 i#2 j#3 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
[ a#0 c#1 i#2 j#3 ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
[ a#0 c#1 i#2 ]
[ a#0 c#1 ]
[ ] in file "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9

Variable definitions:
(a#0 -> a) in file "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5
(b#6 -> b) in file "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7
(b#7 -> b) in file "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5
(c#1 -> c) in file "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11
(c#5 -> c) in file "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11
(i#2 -> i) in file "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38
(j#3 -> j) in file "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40
(k#4 -> k) in file "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9
Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "shadowing.mligo" ; "--syntax=cameligo" ; "--format=dev" ] ;
  [%expect {|
Scopes:
[ a#0 c#1 d#4 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
[ a#0 c#1 d#4 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
[ a#0 c#1 d#4 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
[ a#3 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
[ a#3 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9
[ a#3 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
[ a#0 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21
[ a#0 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17
[ a#0 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13
[ a#0 c#1 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17
[ a#0 c#1 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13
[ a#0 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15
[ a#0 ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-11
[ ] in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9

Variable definitions:
(a#0 -> a) in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5
(a#3 -> a) in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9
(b#5 -> b) in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5
(c#1 -> c) in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7
(d#4 -> d) in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7
(e#2 -> e) in file "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9
Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "records.mligo" ; "--syntax=cameligo" ; "--format=dev" ] ;
  [%expect {|
Scopes:
[ a#1 b#4 g#5 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
[ a#1 b#4 g#5 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-37
[ a#1 b#4 g#5 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
[ a#1 b#4 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 15-41
[ a#1 b#4 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4
[ a#1 i#2 j#3 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
[ a#1 i#2 j#3 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
[ a#1 i#2 j#3 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41
[ a#1 i#2 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45
[ a#1 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-45
[ a#1 myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 53-55
[ myrec#0 ] in file "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9

Variable definitions:
(a#1 -> a) in file "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5
(b#4 -> b) in file "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5
(e#6 -> e) in file "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5
(g#5 -> g) in file "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20
(i#2 -> i) in file "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19
(j#3 -> j) in file "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32
Type definitions:
(myrec#0 -> myrec) in file "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36 |} ] ;

  run_ligo_good [ "get-scope" ; gs "constant.mligo" ; "--syntax=cameligo" ; "--format=dev" ] ;
  [%expect {|
Scopes:
[ a#0 e#3 ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
[ a#0 e#3 ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
[ a#0 e#3 ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23
[ a#0 e#3 ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21
[ a#0 ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32
[ a#0 c#1 d#2 ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44
[ a#0 c#1 d#2 ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
[ a#0 c#1 d#2 ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
[ a#0 c#1 ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
[ ] in file "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9

Variable definitions:
(a#0 -> a) in file "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
(b#4 -> b) in file "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
(c#1 -> c) in file "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 9-18
(d#2 -> d) in file "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
(e#3 -> e) in file "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "application.mligo" ; "--syntax=cameligo" ; "--format=dev" ] ;
  [%expect {|
Scopes:
[ c#4 f#2 ] in file "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
[ f#2 ] in file "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 22-36
[ b#3 f#2 ] in file "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 18-19
[ b#3 f#2 ] in file "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
[ f#2 ] in file "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-19
[ i#0 j#1 ] in file "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
[ i#0 j#1 ] in file "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59

Variable definitions:
(a#5 -> a) in file "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5
(b#3 -> b) in file "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8
(c#4 -> c) in file "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27
(f#2 -> f) in file "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7
(i#0 -> i) in file "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 35-44
(j#1 -> j) in file "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 45-54
Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "include.mligo" ; "--format=dev" ] ;
  [%expect{|
    Scopes:
    [ a#0 b#5 x#6 ] in file "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 12-13
    [ a#0 b#5 x#6 ] in file "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    [ a#0 b#5 ] in file "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    [ a#0 c#1 d#4 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    [ a#0 c#1 d#4 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    [ a#0 c#1 d#4 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    [ a#0 c#1 e#2 f#3 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    [ a#0 c#1 e#2 f#3 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    [ a#0 c#1 e#2 f#3 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9
    [ a#0 c#1 e#2 f#3 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5
    [ a#0 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21
    [ a#0 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17
    [ a#0 c#1 e#2 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13
    [ a#0 c#1 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17
    [ a#0 c#1 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13
    [ a#0 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15
    [ a#0 ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-11
    [ ] in file "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9

    Variable definitions:
    (a#0 -> a) in file "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5
    (b#5 -> b) in file "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5
    (c#1 -> c) in file "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7
    (d#4 -> d) in file "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7
    (e#2 -> e) in file "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9
    (f#3 -> f) in file "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9
    (x#6 -> x) in file "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5
    (y#7 -> y) in file "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5
    Type definitions: |} ] ;