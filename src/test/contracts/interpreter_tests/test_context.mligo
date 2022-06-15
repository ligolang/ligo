let main ((k, i) : int * int) : operation list * int =
  ([] : operation list), k + i


let test_contract =
  let () = Test.log "test_contract:" in
  let (ta, _, _) = Test.originate main 0 0tez in
  let c = Test.to_contract ta in
  let () = Test.log (Test.get_storage ta) in
  let () = Test.save_context () in
  let _ = Test.transfer_to_contract_exn c 5 123tez in
  let () = Test.save_context () in
  let _ = Test.transfer_to_contract_exn c 5 123tez in
  let () = Test.log (Test.get_storage ta) in
  let () = Test.restore_context () in
  let () = Test.log (Test.get_storage ta) in
  let () = Test.restore_context () in
  let () = Test.log (Test.get_storage ta) in
  let () = Test.restore_context () in
  let () = Test.log (Test.get_storage ta) in
  ()  

let test_move =
  let () = Test.log "test_move:" in
  let _ = Test.reset_state 4n ([] : tez list) in  
  let to_ = Test.nth_bootstrap_account 2 in
  let () = Test.log (Test.get_balance to_) in
  let () = Test.save_context () in
  let _ = Test.transfer_exn to_ (Test.eval ()) 100tez in
  let () = Test.log (Test.get_balance to_) in
  let () = Test.restore_context () in
  let () = Test.log (Test.get_balance to_) in
  ()

let test_drop =
  let () = Test.log "test_drop:" in
  let _ = Test.reset_state 4n ([] : tez list) in
  let to_ = Test.nth_bootstrap_account 2 in
  let () = Test.log (Test.get_balance to_) in
  let () = Test.save_context () in
  let _ = Test.transfer_exn to_ (Test.eval ()) 100tez in
  let () = Test.log (Test.get_balance to_) in
  let () = Test.drop_context () in
  let () = Test.log (Test.get_balance to_) in
  ()
