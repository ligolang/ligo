let id (type a) (x : a) : a = x

module C = struct
  [@entry] let main (m : int) (n : int) : operation list * int =
    ([] : operation list), (id n) + (id m)
end

let test =
  let orig = Test.originate (contract_of C) 0 0tez in
  let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
  assert (Test.get_storage orig.addr = 42)
