module C = struct
  type storage = int

  type parameter =
  | Increment of int
  | Decrement of int
  | Reset

  type return = operation list * storage

  // Two entrypoints

  [@entry]
  let increment (store : storage) (delta : int) : operation list * storage =
    let () = Test.log "foo" in
    [], store + delta

  [@entry]
  let decrement (store : storage) (delta : int) : operation list * storage = [], store - delta
end

let _test () =
  let initial_storage = 10 in
  let orig = Test.originate (contract_of C) initial_storage 0mutez in
  let contr = Test.to_contract (orig.addr) in
  let _r = Test.transfer_to_contract_exn contr (Increment (32)) 1000000mutez in
  (Test.get_storage (orig.addr) = initial_storage + 32)

let test = _test ()
