module Test = Test.Next

let get_storage = Test.Typed_address.get_storage
let transfer_exn = Test.Typed_address.transfer_exn
let get_balance = Test.Address.get_balance
let to_typed_address = Test.Address.to_typed_address

module C = struct
  [@entry]
  let main (k : int) (i : int) : operation list * int =
    ([] : operation list), k + i
end

let test_contract =
  let () = Test.IO.log "test_contract:" in
  let orig = Test.Originate.contract (contract_of C) 0 0tez in
  let () = Test.IO.log (get_storage orig.taddr) in
  let () = Test.State.save () in
  let _ = transfer_exn orig.taddr (Main 5) 123tez in
  let () = Test.State.save () in
  let _ = transfer_exn orig.taddr (Main 5) 123tez in
  let () = Test.IO.log (get_storage orig.taddr) in
  let () = Test.State.restore () in
  let () = Test.IO.log (get_storage orig.taddr) in
  let () = Test.State.restore () in
  let () = Test.IO.log (get_storage orig.taddr) in
  let () = Test.State.restore () in
  let () = Test.IO.log (get_storage orig.taddr) in
  ()

let test_move =
  let () = Test.IO.log "test_move:" in
  let _ = Test.State.reset 4n ([] : tez list) in
  let to_ = Test.Account.address 2 in
  let () = Test.IO.log (get_balance to_) in
  let () = Test.State.save () in
  let _ = transfer_exn (to_typed_address to_: implicit_address) () 100tez in
  let () = Test.IO.log (get_balance to_) in
  let () = Test.State.restore () in
  let () = Test.IO.log (get_balance to_) in
  ()

let test_drop =
  let () = Test.IO.log "test_drop:" in
  let _ = Test.State.reset 4n ([] : tez list) in
  let to_ = Test.Account.address 2 in
  let () = Test.IO.log (get_balance to_) in
  let () = Test.State.save () in
  let _ = transfer_exn (to_typed_address to_ : implicit_address) () 100tez in
  let () = Test.IO.log (get_balance to_) in
  let () = Test.State.drop () in
  let () = Test.IO.log (get_balance to_) in
  ()
