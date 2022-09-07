open Cli_expect

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; test "infer_fun_application.mligo" ] ;
  [%expect{|
    const magic : funtype a : * . unit -> option (a) = Λ a ->
      lambda (xunit)option (a) return (failwith@{unit}@{option (a)})@(unit)
    const test : funtype a : * . unit = Λ a ->
      (UNOPT((magic@{( int * unit ) -> unit})@(unit)))@(( 1 , unit )) |}]