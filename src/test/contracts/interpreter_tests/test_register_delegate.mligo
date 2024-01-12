module C = struct
  [@entry] let main (p : int) (s : int) : operation list * int = ([] : operation list), p + s
end

let test =

  let acc = Test.new_account () in
  let pkh = Crypto.hash_key acc.1 in
  let c = Tezos.implicit_account pkh in
  let a = Tezos.address c in

  let _ = Test.transfer_to_contract_exn c () 1000000tez in
  let () = Test.register_delegate pkh in
  (*
   * since Oxford 2, we do not allow manual staking:
  let () = Test.stake pkh 1000000tez in
  let () = Test.bake_until_n_cycle_end 4n in
  *)
  let () = Test.bake_until_n_cycle_end 8n in

  let () = Test.log "STARTING BALANCE AND VOTING POWER" in
  let () = Test.log(Test.get_balance_of_address a) in
  let () = Test.log(Test.get_voting_power pkh) in
  let () = Test.set_baker a in
  let orig = Test.originate (contract_of C) 41 5tez in

  let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
  let () = Test.log(Test.get_balance_of_address a) in
  let () = Test.log(Test.get_voting_power pkh) in
  let _ = Test.transfer orig.addr (Main 1) 3tez in

  let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
  let () = Test.log(Test.get_balance_of_address a) in
  let () = Test.log(Test.get_voting_power pkh) in
  ()
