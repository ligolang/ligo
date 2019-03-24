open Ligo_helpers.Trace
open Ligo
open Test_helpers

let pass (source:string) : unit result =
  let%bind raw =
    trace (simple_error "parsing") @@
    parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    simplify raw in
  let%bind typed =
    trace (simple_error "typing") @@
    type_ simplified in
  let%bind mini_c =
    trace (simple_error "transpiling") @@
    transpile typed in
  Format.printf "mini_c code : %a" Mini_c.PP.program mini_c ;
  ok ()

let basic () : unit result =
  Format.printf "basic test" ;
  pass "./contracts/toto.ligo"

let function_ () : unit result =
  Format.printf "function test" ;
  let%bind _ = pass "./contracts/function.ligo" in
  let%bind result = easy_run_main "./contracts/function.ligo" "2" in
  Format.printf "result : %a" AST_Typed.PP.annotated_expression result ;
  ok ()

let main = "Integration (End to End)", [
    test "basic" basic ;
    test "function" function_ ;
  ]
