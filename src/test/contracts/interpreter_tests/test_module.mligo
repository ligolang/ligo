
module C = struct
  module F = struct
    let f (x : int) = x + 1
  end

  [@entry]
  let main (_ : unit) (i : int) : operation list * int =
    ([] : operation list), F.f i
end

let test =
  let orig = Test.originate (contract_of C) 0 0tez in
  let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
  Test.get_storage orig.addr
