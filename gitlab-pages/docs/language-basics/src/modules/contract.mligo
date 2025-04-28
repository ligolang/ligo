module C = struct
  [@entry] let increment (p : int) (s : int) : operation list * int = [], s + p
  [@entry] let decrement (p : int) (s : int) : operation list * int = [], s - p
end
module Test = Test.Next

let test =
  let orig = Test.Originate.contract (contract_of C) 0 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Increment 42) 0tez
  in Assert.assert (Test.Typed_address.get_storage orig.taddr = 42)
module FA0 = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end

module FA0Ext = struct
  include FA0
  [@entry] let transfer2 (a : unit) (b : t) = transfer a b
end