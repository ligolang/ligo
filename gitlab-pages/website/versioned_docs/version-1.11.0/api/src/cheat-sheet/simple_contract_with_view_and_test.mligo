module C = struct
  type storage = int

  [@entry]
  let increment (action: int) (store: storage) : operation list * storage =
    [], store + action

  [@entry]
  let decrement (action: int) (store: storage) : operation list * storage =
    [], store - action

  [@view]
  let get_storage (must_be_positive: bool) (storage: int) : int =
    if must_be_positive && storage < 0 then
      failwith "Negative value in storage"
    else
      storage
end

let testC =
    let initial_storage = 42 in
    let originated = Test.originate (contract_of C) initial_storage 0tez in
    let p : C parameter_of = Increment 1 in
    let _ = Test.transfer_exn originated.addr p 1mutez in
    assert (Test.get_storage originated.addr = initial_storage + 1)