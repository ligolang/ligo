module Test = Test.Next

module C = struct
  module F = struct
    let f (x : int) = x + 1
  end

  [@entry]
  let main (_ : unit) (i : int) : operation list * int =
    ([] : operation list), F.f i
end

let test =
  let orig = Test.Originate.contract (contract_of C) 0 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main ()) 0tez in
  Test.Typed_address.get_storage orig.taddr
