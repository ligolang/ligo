open Trace
open Ligo
open Test_helpers

let compile_contract_basic () : unit result =
  let%bind _ =
    Contract.compile_contract_file "./contracts/dispatch-counter.ligo" "main"
  in
  ok ()

let main = "Bin", [
    test "compile contract basic" compile_contract_basic ;
]
