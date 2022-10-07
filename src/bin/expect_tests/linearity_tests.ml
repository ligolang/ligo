open Cli_expect

(* Checking type linearity *)
let%expect_test _ =
  run_ligo_bad ["run"; "interpret" ; "foo"  ; "--init-file" ; bad_test "linearity.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/linearity.mligo", line 1, characters 14-39:
      1 | type foofoo = {foo : string; foo : int}

    Duplicated field or variant name.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "foo"  ; "--init-file"; bad_test "linearity.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/linearity.ligo", line 1, characters 15-49:
      1 | type foofoo is record [ foo : string; foo : int ]

    Duplicated field or variant name.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "foo"  ; "--init-file"; bad_test "linearity.religo" ] ;
  [%expect {|
    File "../../test/contracts/negative/linearity.religo", line 1, characters 14-39:
      1 | type foofoo = {foo : string, foo : int};

    Duplicated field or variant name.
    Hint: Change the name. |}];

(* Checking binders linearity *)
  run_ligo_bad ["run"; "interpret" ; "let foo (x, x : int * int) : int = x in foo 1"  ; "--syntax";"cameligo" ] ;
  [%expect {|
    Repeated variable in pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "let bar (p : int * int) : int = let (x, x) = p in x in bar (1,2)" ; "--syntax";"cameligo" ] ;
  [%expect {|
    Repeated variable in pattern.
    Hint: Change the name. |}];

(* error is different just because jsligo has different set of tests *)
  run_ligo_bad [ "run" ; "interpret" ; "--syntax" ; "jsligo" ; "(( [x,x] : [int , int] ) : int => x)" ] ;
  [%expect{|
    Repeated variable in pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_pattern_matching.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/linearity_pattern_matching.mligo", line 4, characters 4-30:
      3 | let yy : string = match { a = 1 ; b = 2n ; c = "33" } with
      4 |   | { a = a ;  b = b ; c = a } -> a

    Repeated variable in pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_pattern_matching.religo" ] ;
  [%expect{|
    File "../../test/contracts/negative/linearity_pattern_matching.religo", line 4, characters 4-30:
      3 | let yy : string = switch ({ a : 1 , b : 2n , c : "33" }) {
      4 |   | { a : a ,  b : b , c : a } => a
      5 |   }

    Repeated variable in pattern.
    Hint: Change the name. |}];
  
  run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_pattern_matching.ligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/linearity_pattern_matching.ligo", line 4, characters 4-37:
      3 | const yy : string = case (record [ a = 1 ; b = 2n ; c = "33" ]) of [
      4 |   | record [ a = a ;  b = b ; c = a ] -> a
      5 | ]

    Repeated variable in pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_pattern_matching.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/linearity_pattern_matching.mligo", line 4, characters 4-30:
      3 | let yy : string = match { a = 1 ; b = 2n ; c = "33" } with
      4 |   | { a = a ;  b = b ; c = a } -> a

    Repeated variable in pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_record_pattern_let_in.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/linearity_record_pattern_let_in.mligo", line 6, characters 8-27:
      5 | let y =
      6 |     let { foo ; foo ; bar } = x in
      7 |     foo + bar

    Repeated variable in pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_record_pattern_fun.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/linearity_record_pattern_fun.mligo", line 3, characters 12-31:
      2 |
      3 | let y = fun { foo ; foo ; bar } -> foo + bar

    Repeated variable in pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_record_pattern.mligo" ] ;
  [%expect{|
    File "../../test/contracts/negative/linearity_record_pattern.mligo", line 3, characters 32-51:
      2 |
      3 | let y (x:foofoo) = match x with { foo ; foo ; bar } -> foo + bar

    Repeated variable in pattern.
    Hint: Change the name. |}];