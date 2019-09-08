open Trace
open Ligo.Run
open Test_helpers

let syntax_error () : unit result =
  let%bind _program = type_file `pascaligo "./contracts/error_syntax.ligo" in
  ok ()

let type_error () : unit result =
  let%bind _program = type_file `pascaligo "./contracts/error_type.ligo" in
  ok ()

let () =
  List.iter wrap_test_raw [
    type_error ;
    syntax_error ;
  ]
