
let main (p : int) (s : int) : operation list * int = ([] : operation list), p + s

let test =
  let acc = Test.new_account () in
  let () = Test.baker_account acc (None : tez option) in
  let () = Test.reset_state 2n ([] : tez list) in
  let pkh = Crypto.hash_key acc.1 in
  let c = Tezos.implicit_account pkh in
  let a = Tezos.address c in
  let () = Test.log "STARTING BALANCE AND VOTING POWER" in
  let () = Test.log(Test.get_balance a) in
  let () = Test.log(Test.get_voting_power pkh) in
  let () = Test.set_baker a in
  let (ta, _, _) = Test.originate main 41 5tez in
  let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
  let () = Test.log(Test.get_balance a) in
  let () = Test.log(Test.get_voting_power pkh) in
  let cc = Test.to_contract ta in
  let _ = Test.transfer_to_contract cc 1 3tez in
  let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
  let () = Test.log(Test.get_balance a) in
  let () = Test.log(Test.get_voting_power pkh) in
  ()
