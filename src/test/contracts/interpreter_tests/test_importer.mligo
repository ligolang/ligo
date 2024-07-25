#import "test_imported.mligo" "External"

module Test = Test.Next
let assert = Assert.assert

let test =
  let orig = Test.Originate.contract (contract_of External) External.D.default.initial 0tez in
  let contr = Test.Typed_address.to_contract orig.taddr in
  let () = assert (Test.Typed_address.get_storage orig.taddr = External.D.default.initial) in
  let _ = Test.Contract.transfer_exn contr (Main (External.D.default.final)) 0tez in
  assert (Test.Typed_address.get_storage orig.taddr = External.D.default.final)
