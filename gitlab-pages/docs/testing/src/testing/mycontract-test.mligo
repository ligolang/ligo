(* This is mycontract-test.mligo *)

#import "gitlab-pages/docs/testing/src/testing/mycontract.mligo" "MyContract"

let run_test1 =
  let initial_storage = 10 in
  let orig = Test.Next.Originate.contract (contract_of MyContract.MyContract) initial_storage 0tez in
  let () = Assert.assert (Test.Next.Typed_address.get_storage(orig.taddr) = initial_storage) in
  let _: nat = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint "increment" orig.taddr) 32 0tez in
  Assert.assert (Test.Next.Typed_address.get_storage(orig.taddr) = initial_storage + 32)