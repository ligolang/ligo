open Trace
open Ligo.Run
open Test_helpers

let compile_contract_basic () : unit result =
  let%bind _ =
    compile_contract_file "./contracts/dispatch-counter.ligo" "main" "pascaligo"
  in
  ok ()

let main = test_suite "Bin" [
    test "compile contract basic" compile_contract_basic ;
]
