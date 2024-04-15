module C = struct
  [@entry] let increment (p : int) (s : int) : operation list * int = [], s + p
  [@entry] let decrement (p : int) (s : int) : operation list * int = [], s - p
end
let test =
  let orig = Test.originate (contract_of C) 0 0tez in
  let _ = Test.transfer_exn orig.addr (Increment 42) 0tez
  in assert (42 = Test.get_storage orig.addr)
module FA0 = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end

module FA0Ext = struct
  include FA0
  [@entry] let transfer2 (a : unit) (b : t) = transfer a b
end