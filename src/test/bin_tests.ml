open Trace
open Test_helpers

let compile_contract_basic () : unit result =
  let%bind _ =
    Ligo.Compile.Of_source.compile_file_entry "./contracts/dispatch-counter.ligo" "main" (Syntax_name "pascaligo")
  in
  ok ()

let main = test_suite "Bin" [
    test "compile contract basic" compile_contract_basic ;
]
