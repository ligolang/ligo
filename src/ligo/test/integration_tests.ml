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
  let%bind _mini_c =
    trace (simple_error "transpiling") @@
    transpile typed in
  ok ()

let basic () : unit result =
  pass "./contracts/toto.ligo"

let function_ () : unit result =
  let%bind _ = pass "./contracts/function.ligo" in
  let%bind _ = easy_run_main "./contracts/function.ligo" "2" in
  ok ()

let declarations () : unit result =
  let%bind _ = easy_run_main "./contracts/declarations.ligo" "2" in
  ok ()

let main = "Integration (End to End)", [
    test "basic" basic ;
    test "function" function_ ;
    test "declarations" declarations ;
  ]
