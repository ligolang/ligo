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
    let () = Test.IO.log "foo" in
    [], store + delta

  [@entry]
  let decrement (store : storage) (delta : int) : operation list * storage = [], store - delta
end

let _test () =
  let initial_storage = 10 in
  let orig = Test.Originate.contract (contract_of C) initial_storage 0mutez in
  let contr = Test.Typed_address.to_contract orig.taddr in
  let _r = Test.Contract.transfer_exn contr (Increment (32)) 1000000mutez in
  (Test.Typed_address.get_storage orig.taddr = initial_storage + 32)

let test = _test ()
