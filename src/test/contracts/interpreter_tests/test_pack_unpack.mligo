module Test = Test.Next
let assert = Assert.assert

module C = struct
  [@entry]
  let main (b : bytes) (_ : bytes) : operation list * bytes =
    ([] : operation list), b
end

let test =
  let b = Bytes.pack 42n in
  let orig = Test.Originate.contract (contract_of C) b 0tez in
  let () = assert ((Bytes.unpack (Test.Typed_address.get_storage orig.taddr) : nat option) = Some 42n) in
  let b = Bytes.pack "bonjour" in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main b) 0tez in
  assert ((Bytes.unpack (Test.Typed_address.get_storage orig.taddr) : string option) = Some "bonjour")
