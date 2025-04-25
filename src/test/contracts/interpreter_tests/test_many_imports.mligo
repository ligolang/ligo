module Test = Test.Next

module C = C

let test =
  let orig = Test.Originate.contract (contract_of C) () 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main ()) 0tez in
  Assert.assert (Test.Typed_address.get_storage orig.taddr = ())
