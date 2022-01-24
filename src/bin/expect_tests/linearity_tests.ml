open Cli_expect


let%expect_test _ =
  run_ligo_bad ["run"; "interpret" ; "foo"  ; "--init-file" ; bad_test "linearity.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/linearity.mligo", line 1, characters 29-32:
      1 | type foofoo = {foo : string; foo : int}
      2 | let foofoo : foofoo = {foo = "foo"; foo = 1}

    Duplicate field name "foo" in this record declaration.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "foo"  ; "--init-file"; bad_test "linearity.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/linearity.ligo", line 1, characters 38-41:
      1 | type foofoo is record [ foo : string; foo : int ]
      2 | const foofoo : foofoo = record [ foo = "foo"; foo = 1 ]

    Duplicate field name "foo" in this record declaration.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "foo"  ; "--init-file"; bad_test "linearity.religo" ] ;
  [%expect {|
    File "../../test/contracts/negative/linearity.religo", line 1, characters 29-32:
      1 | type foofoo = {foo : string, foo : int};
      2 | let foofoo : foofoo = {foo : "foo", foo : 1};

    Duplicate field name "foo" in this record declaration.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "let foo (x, x : int * int) : int = x in foo 1"  ; "--syntax";"cameligo" ] ;
  [%expect {|
    Repeated variable "x" in this pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "let bar (p : int * int) : int = let (x, x) = p in x in bar (1,2)" ; "--syntax";"cameligo" ] ;
  [%expect {|
    Repeated variable "x" in this pattern.
    Hint: Change the name. |}];

  (* run_ligo_bad [ "interpret" ; "--syntax=cameligo" ; "(( (x,x) : (int , int) ) : int => x) (1,1)" ] ;
  [%expect {|
    Repeated variable "x" in this pattern.
    Hint: Change the name. |}]; *)

  (* run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_pattern_matching.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/linearity_pattern_matching.mligo", line 4, characters 27-28:
      3 | let yy : string = match { a = 1 ; b = 2n ; c = "33" } with
      4 |   | { a = a ;  b = b ; c = a } -> a

    Repeated variable "a" in this pattern.
    Hint: Change the name. |}];

  run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_pattern_matching.religo" ] ;
  [%expect {|
    File "../../test/contracts/negative/linearity_pattern_matching.religo", line 4, characters 27-28:
      3 | let yy : string = switch ({ a : 1 , b : 2n , c : "33" }) {
      4 |   | { a : a ,  b : b , c : a } => a
      5 |   }

    Repeated variable "a" in this pattern.
    Hint: Change the name. |}];
  
  run_ligo_bad ["run"; "interpret" ; "yy"  ; "--init-file"; bad_test "linearity_pattern_matching.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/linearity_pattern_matching.ligo", line 4, characters 34-35:
      3 | const yy : string = case (record [ a = 1 ; b = 2n ; c = "33" ]) of
      4 |   | record [ a = a ;  b = b ; c = a ] -> a
      5 |   end

    Repeated variable "a" in this pattern.
    Hint: Change the name. |}]; *)