#import "./contract_under_test/module_adder.mligo" "Adder"

let _tester (a : (Adder parameter_of, int) typed_address) (_ : michelson_contract) (_ : int) : unit =
  let c : (Adder parameter_of) contract = Test.to_contract a in
  (* Test 1 *)
  let _ = Test.transfer_to_contract_exn c (Add 0) 0tez in
  let () = assert (Test.get_storage a = 0) in
  (* Test 2 *)
  let _ = Test.transfer_to_contract_exn c (Add 1) 0tez in 
  let () = assert (Test.get_storage a = 1) in
  ()

let test =
    Test.originate_module_and_mutate_all (contract_of Adder) 0 0tez _tester
