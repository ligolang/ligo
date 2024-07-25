#import "C.mligo" "C"

let assert = Assert.assert

module Test = Test.Next

let test =
  let orig = Test.Originate.contract (contract_of C) () 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main ()) 0tez in
  assert (Test.Typed_address.get_storage orig.taddr = ())
