module Counter = struct
  type storage = int
  type return = operation list * storage

  (* Three entrypoints *)
  [@entry] let increment (delta : int) (store : storage) : return = [], store + delta
  [@entry] let decrement (delta : int) (store : storage) : return = [], store - delta
  [@entry] let reset (() : unit) (_ : storage) : return = [], 0
end
let test_increment =
  let initial_storage = 10 in
  let orig = Test.originate (contract_of Counter) initial_storage 0tez in
  let _ = Test.transfer_exn orig.addr (Increment (32)) 1mutez in
  assert (Test.get_storage(orig.addr) = initial_storage + 32)