open Cli_expect

(* avoid pretty printing *)
let () = Unix.putenv "TERM" "dumb"

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_syntax.ligo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/error_syntax.ligo", line 1, characters 16-17
      1 | type foo is bar - 42
    Ill-formed declaration.
    At this point, if the declaration is complete, another is possible. |} ] ;

