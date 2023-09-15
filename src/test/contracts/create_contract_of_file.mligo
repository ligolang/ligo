[@entry]
let main (x : unit) (_ : unit) : operation list * unit =
  let op, _addr = [%create_contract_of_file "./interpreter_tests/contract_under_test/compiled.tz"] None 1tez x  in
  [op], ()
