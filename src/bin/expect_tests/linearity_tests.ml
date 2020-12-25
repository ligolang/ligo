open Cli_expect

let c = "../../test/contracts/negative/linearity.mligo"
let p = "../../test/contracts/negative/linearity.ligo"


let%expect_test _ =
  run_ligo_bad [ "interpret" ; "--init-file="^c ; "foo"  ] ;
  [%expect {|
    Duplicate field name "foo" in this record declaration.
    Hint: Change the name. |}];

  run_ligo_bad [ "interpret" ; "--init-file="^p ; "foo"  ] ;
  [%expect {|
    Duplicate field name "foo" in this record declaration.
    Hint: Change the name. |}];

  run_ligo_bad [ "interpret" ; "--syntax=cameligo" ; "let foo (x, x : int * int) : int = x in foo 1"  ] ;
  [%expect {|
    Repeated variable "x" in this pattern.
    Hint: Change the name. |}];

  run_ligo_bad [ "interpret" ; "--syntax=cameligo" ; "let bar (p : int * int) : int = let (x, x) = p in x in bar (1,2)" ] ;
  [%expect {|
    Repeated variable "x" in this pattern.
    Hint: Change the name. |}];

  (* run_ligo_bad [ "interpret" ; "--syntax=cameligo" ; "(( (x,x) : (int , int) ) : int => x) (1,1)" ] ;
  [%expect {|
    Repeated variable "x" in this pattern.
    Hint: Change the name. |}]; *)