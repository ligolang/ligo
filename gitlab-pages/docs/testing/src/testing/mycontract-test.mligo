(* This is mycontract-test.mligo *)

module Test = Test.Next

#import "gitlab-pages/docs/testing/src/testing/mycontract.mligo" "MyContract"

let run_test1 =
  let initial_storage = 10 in
  let orig = Test.Originate.contract (contract_of MyContract.MyContract) initial_storage 0tez in
  let () = Assert.assert (Test.Typed_address.get_storage(orig.taddr) = initial_storage) in
  let _: nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "increment" orig.taddr) 32 0tez in
  Assert.assert (Test.Typed_address.get_storage(orig.taddr) = initial_storage + 32)