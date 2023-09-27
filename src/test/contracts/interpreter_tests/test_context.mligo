module C = struct
  [@entry]
  let main (k : int) (i : int) : operation list * int =
    ([] : operation list), k + i
end

let test_contract =
  let () = Test.log "test_contract:" in
  let orig = Test.originate (contract_of C) 0 0tez in
  let () = Test.log (Test.get_storage orig.addr) in
  let () = Test.save_context () in
  let _ = Test.transfer_exn orig.addr (Main 5) 123tez in
  let () = Test.save_context () in
  let _ = Test.transfer_exn orig.addr (Main 5) 123tez in
  let () = Test.log (Test.get_storage orig.addr) in
  let () = Test.restore_context () in
  let () = Test.log (Test.get_storage orig.addr) in
  let () = Test.restore_context () in
  let () = Test.log (Test.get_storage orig.addr) in
  let () = Test.restore_context () in
  let () = Test.log (Test.get_storage orig.addr) in
  ()  

let test_move =
  let () = Test.log "test_move:" in
  let _ = Test.reset_state 4n ([] : tez list) in  
  let to_ = Test.nth_bootstrap_account 2 in
  let () = Test.log (Test.get_balance_of_address to_) in
  let () = Test.save_context () in
  let _ = Test.transfer_exn (Test.cast_address to_: implicit_address) () 100tez in
  let () = Test.log (Test.get_balance_of_address to_) in
  let () = Test.restore_context () in
  let () = Test.log (Test.get_balance_of_address to_) in
  ()

let test_drop =
  let () = Test.log "test_drop:" in
  let _ = Test.reset_state 4n ([] : tez list) in
  let to_ = Test.nth_bootstrap_account 2 in
  let () = Test.log (Test.get_balance_of_address to_) in
  let () = Test.save_context () in
  let _ = Test.transfer_exn (Test.cast_address to_ : implicit_address) () 100tez in
  let () = Test.log (Test.get_balance_of_address to_) in
  let () = Test.drop_context () in
  let () = Test.log (Test.get_balance_of_address to_) in
  ()
