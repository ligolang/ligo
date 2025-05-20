let id (type a) (x : a) : a = x

module C = struct
  [@entry]
  let main (m : int) (n : int) : operation list * int = [], id n + id m
end

let test =
  let orig = Test.Originate.contract (contract_of C) 0 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main 42) 0tez in
  Assert.assert (Test.Typed_address.get_storage orig.taddr = 42)
