module C = struct
  [@entry] let increment (p : int) (s : int) : operation list * int = [], s + p
  [@entry] let decrement (p : int) (s : int) : operation list * int = [], s - p
end

let test =
  let ta, _, _ = Test.originate (contract_of C) 0 0tez in
  let c = Test.to_contract ta in
  let _ = Test.transfer_to_contract_exn c (Increment 42) 0tez in
  assert (42 = Test.get_storage ta)
