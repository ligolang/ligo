module Adder = Contract_under_test.Module_adder

let _tester (a : (Adder parameter_of, int) typed_address) (_ : (Adder parameter_of, int) michelson_contract) (_ : int) : unit =
  let c : (Adder parameter_of) contract = Test.Typed_address.to_contract a in
  (* Test 1 *)
  let _ = Test.Contract.transfer_exn c (Add 0) 0tez in
  let () = Assert.assert (Test.Typed_address.get_storage a = 0) in
  (* Test 2 *)
  let _ = Test.Contract.transfer_exn c (Add 1) 0tez in
  let () = Assert.assert (Test.Typed_address.get_storage a = 1) in
  ()

let test = Test.Mutation.All.contract (contract_of Adder) 0 0tez _tester
